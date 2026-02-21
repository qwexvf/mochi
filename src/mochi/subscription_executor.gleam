// mochi/subscription_executor.gleam
// Executes GraphQL subscription operations
//
// Subscriptions work differently from queries/mutations:
// 1. Client sends subscription operation
// 2. Server resolves the topic from arguments
// 3. Server registers the subscription with PubSub
// 4. When events are published, the selection set is executed
// 5. Results are sent to the subscriber

import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import mochi/ast
import mochi/executor.{type ExecutionResult}
import mochi/parser
import mochi/schema
import mochi/subscription.{type PubSub, type SubscriptionId, type Topic}
import mochi/types

// ============================================================================
// Types
// ============================================================================

/// Result of starting a subscription
pub type SubscriptionResult {
  SubscriptionResult(
    subscription_id: SubscriptionId,
    topic: Topic,
    pubsub: PubSub,
  )
  SubscriptionError(message: String)
}

/// Subscription context for execution
pub type SubscriptionContext {
  SubscriptionContext(
    schema: schema.Schema,
    pubsub: PubSub,
    execution_context: schema.ExecutionContext,
    variable_values: Dict(String, Dynamic),
  )
}

/// Callback type for subscription events
pub type SubscriptionCallback =
  fn(ExecutionResult) -> Nil

// ============================================================================
// Main API
// ============================================================================

/// Subscribe to a GraphQL subscription operation
/// Returns the subscription ID and updated PubSub, or an error
pub fn subscribe(
  context: SubscriptionContext,
  query: String,
  callback: SubscriptionCallback,
) -> SubscriptionResult {
  case parser.parse(query) {
    Ok(document) -> subscribe_document(context, document, callback)
    Error(_) -> SubscriptionError("Failed to parse subscription query")
  }
}

/// Subscribe with a pre-parsed document
pub fn subscribe_document(
  context: SubscriptionContext,
  document: ast.Document,
  callback: SubscriptionCallback,
) -> SubscriptionResult {
  // Find the subscription operation
  case find_subscription_operation(document) {
    Some(operation) ->
      execute_subscription(context, document, operation, callback)
    None ->
      SubscriptionError("Document does not contain a subscription operation")
  }
}

/// Unsubscribe from a subscription
pub fn unsubscribe(pubsub: PubSub, subscription_id: SubscriptionId) -> PubSub {
  subscription.unsubscribe(pubsub, subscription_id)
}

/// Publish an event to subscribers
/// This will execute the selection set for each subscriber and call their callbacks
pub fn publish_event(
  context: SubscriptionContext,
  topic: Topic,
  event_data: Dynamic,
) -> Nil {
  subscription.publish(context.pubsub, topic, event_data)
}

// ============================================================================
// Internal Implementation
// ============================================================================

fn find_subscription_operation(document: ast.Document) -> Option(ast.Operation) {
  document.definitions
  |> list.find_map(fn(def) {
    case def {
      ast.OperationDefinition(
        ast.Operation(operation_type: ast.Subscription, ..) as op,
      ) -> Ok(op)
      _ -> Error(Nil)
    }
  })
  |> result.map(Some)
  |> result.unwrap(None)
}

fn execute_subscription(
  context: SubscriptionContext,
  document: ast.Document,
  operation: ast.Operation,
  callback: SubscriptionCallback,
) -> SubscriptionResult {
  use subscription_type <- require_subscription_type(context.schema)

  let selection_set = get_selection_set(operation)

  use field <- require_single_root_field_result(selection_set)
  use field_def <- require_subscription_field(subscription_type, field.name)

  let args = coerce_arguments(field.arguments, context.variable_values)
  // Use topic_fn if available (set by subscription_to_field_def), else fall back to field name
  let topic = case field_def.topic_fn {
    Some(topic_fn) ->
      case topic_fn(args, context.execution_context) {
        Ok(t) -> t
        Error(_) -> field.name
      }
    None -> field.name
  }

  let event_callback = fn(event_data: Dynamic) {
    let result =
      execute_subscription_event(
        context,
        document,
        field,
        field_def,
        event_data,
      )
    callback(result)
  }

  let result =
    subscription.subscribe(
      context.pubsub,
      topic,
      field.name,
      args,
      event_callback,
    )

  SubscriptionResult(
    subscription_id: result.subscription_id,
    topic: topic,
    pubsub: result.pubsub,
  )
}

/// Require the schema defines a subscription type
fn require_subscription_type(
  schema_def: schema.Schema,
  next: fn(schema.ObjectType) -> SubscriptionResult,
) -> SubscriptionResult {
  case schema_def.subscription {
    Some(subscription_type) -> next(subscription_type)
    None -> SubscriptionError("Schema does not define a Subscription type")
  }
}

/// Require exactly one root field in selection set
fn require_single_root_field_result(
  selection_set: ast.SelectionSet,
  next: fn(ast.Field) -> SubscriptionResult,
) -> SubscriptionResult {
  case get_single_root_field(selection_set) {
    Ok(field) -> next(field)
    Error(msg) -> SubscriptionError(msg)
  }
}

/// Require the field exists on the subscription type
fn require_subscription_field(
  subscription_type: schema.ObjectType,
  field_name: String,
  next: fn(schema.FieldDefinition) -> SubscriptionResult,
) -> SubscriptionResult {
  case dict.get(subscription_type.fields, field_name) {
    Ok(field_def) -> next(field_def)
    Error(_) ->
      SubscriptionError(
        "Field '" <> field_name <> "' not found on Subscription type",
      )
  }
}

fn get_selection_set(operation: ast.Operation) -> ast.SelectionSet {
  case operation {
    ast.Operation(selection_set: ss, ..) -> ss
    ast.ShorthandQuery(selection_set: ss) -> ss
  }
}

fn get_single_root_field(
  selection_set: ast.SelectionSet,
) -> Result(ast.Field, String) {
  let fields =
    list.filter_map(selection_set.selections, fn(selection) {
      case selection {
        ast.FieldSelection(field) -> Ok(field)
        _ -> Error(Nil)
      }
    })

  case fields {
    [] -> Error("Subscription must have at least one field")
    [field] -> Ok(field)
    _ -> Error("Subscription must have exactly one root field")
  }
}

fn coerce_arguments(
  ast_args: List(ast.Argument),
  variables: Dict(String, Dynamic),
) -> Dict(String, Dynamic) {
  list.fold(ast_args, dict.new(), fn(acc, arg) {
    dict.insert(acc, arg.name, coerce_value(arg.value, variables))
  })
}

fn coerce_value(value: ast.Value, variables: Dict(String, Dynamic)) -> Dynamic {
  case value {
    ast.IntValue(i) -> types.to_dynamic(i)
    ast.FloatValue(f) -> types.to_dynamic(f)
    ast.StringValue(s) -> types.to_dynamic(s)
    ast.BooleanValue(b) -> types.to_dynamic(b)
    ast.NullValue -> types.to_dynamic(Nil)
    ast.EnumValue(e) -> types.to_dynamic(e)
    ast.VariableValue(name) ->
      dict.get(variables, name) |> result.unwrap(types.to_dynamic(Nil))
    ast.ListValue(values) ->
      types.to_dynamic(list.map(values, coerce_value(_, variables)))
    ast.ObjectValue(fields) ->
      types.to_dynamic(
        list.fold(fields, dict.new(), fn(acc, f) {
          dict.insert(acc, f.name, coerce_value(f.value, variables))
        }),
      )
  }
}

fn execute_subscription_event(
  context: SubscriptionContext,
  _document: ast.Document,
  field: ast.Field,
  field_def: schema.FieldDefinition,
  event_data: Dynamic,
) -> ExecutionResult {
  // Execute the selection set against the event data
  let response_name = option.unwrap(field.alias, field.name)

  case field.selection_set {
    None -> {
      // No sub-selection, return event data directly
      executor.ExecutionResult(
        data: Some(make_field(response_name, event_data)),
        errors: [],
      )
    }
    Some(sub_ss) -> {
      // Execute the selection set on the event data
      // This requires getting the type and executing
      case get_field_type_name(field_def.field_type) {
        None ->
          executor.ExecutionResult(
            data: Some(make_field(response_name, event_data)),
            errors: [],
          )
        Some(type_name) -> {
          case dict.get(context.schema.types, type_name) {
            Ok(schema.ObjectTypeDef(obj_type)) -> {
              execute_selection_on_event(
                context,
                sub_ss,
                obj_type,
                event_data,
                response_name,
              )
            }
            _ ->
              executor.ExecutionResult(
                data: Some(make_field(response_name, event_data)),
                errors: [],
              )
          }
        }
      }
    }
  }
}

fn get_field_type_name(field_type: schema.FieldType) -> Option(String) {
  case field_type {
    schema.Named(name) -> Some(name)
    schema.NonNull(inner) -> get_field_type_name(inner)
    schema.List(inner) -> get_field_type_name(inner)
  }
}

fn execute_selection_on_event(
  context: SubscriptionContext,
  selection_set: ast.SelectionSet,
  object_type: schema.ObjectType,
  event_data: Dynamic,
  response_name: String,
) -> ExecutionResult {
  // Execute each field in the selection set
  let field_results =
    list.filter_map(selection_set.selections, fn(selection) {
      case selection {
        ast.FieldSelection(field) ->
          Ok(execute_event_field(context, field, object_type, event_data))
        _ -> Error(Nil)
      }
    })

  // Merge results
  let errors = list.flat_map(field_results, fn(r) { r.errors })
  let data_parts =
    list.filter_map(field_results, fn(r) { option.to_result(r.data, Nil) })

  case data_parts {
    [] ->
      executor.ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(dict.new()))),
        errors: errors,
      )
    _ -> {
      let merged = merge_field_results(data_parts)
      executor.ExecutionResult(
        data: Some(make_field(response_name, merged)),
        errors: errors,
      )
    }
  }
}

fn execute_event_field(
  context: SubscriptionContext,
  field: ast.Field,
  object_type: schema.ObjectType,
  event_data: Dynamic,
) -> ExecutionResult {
  let response_name = option.unwrap(field.alias, field.name)

  case field.name {
    "__typename" ->
      executor.ExecutionResult(
        data: Some(make_field(response_name, types.to_dynamic(object_type.name))),
        errors: [],
      )
    _ -> {
      case dict.get(object_type.fields, field.name) {
        Error(_) ->
          executor.ExecutionResult(data: None, errors: [
            executor.ValidationError(
              "Field '" <> field.name <> "' not found",
              [],
              location: None,
            ),
          ])
        Ok(field_def) -> {
          // Try to resolve from event data or use resolver
          case field_def.resolver {
            Some(resolver) -> {
              let resolver_info =
                schema.ResolverInfo(
                  parent: Some(event_data),
                  arguments: coerce_arguments(
                    field.arguments,
                    context.variable_values,
                  ),
                  context: context.execution_context,
                  info: types.to_dynamic(dict.new()),
                )
              case resolver(resolver_info) {
                Ok(value) ->
                  executor.ExecutionResult(
                    data: Some(make_field(response_name, value)),
                    errors: [],
                  )
                Error(msg) ->
                  executor.ExecutionResult(data: None, errors: [
                    executor.ResolverError(msg, [response_name], location: None),
                  ])
              }
            }
            None -> {
              // Try to extract field from event data
              let value = extract_field_from_dynamic(event_data, field.name)
              executor.ExecutionResult(
                data: Some(make_field(response_name, value)),
                errors: [],
              )
            }
          }
        }
      }
    }
  }
}

fn make_field(name: String, value: Dynamic) -> Dynamic {
  types.to_dynamic(dict.from_list([#(name, value)]))
}

fn merge_field_results(results: List(Dynamic)) -> Dynamic {
  // Merge multiple single-entry field dicts into one combined dict
  list.fold(results, dict.new(), fn(acc, item) {
    case decode.run(item, decode.dict(decode.string, decode.dynamic)) {
      Ok(d) -> dict.merge(acc, d)
      Error(_) -> acc
    }
  })
  |> types.to_dynamic
}

fn extract_field_from_dynamic(data: Dynamic, field: String) -> Dynamic {
  case decode.run(data, decode.at([field], decode.dynamic)) {
    Ok(value) -> value
    Error(_) -> types.to_dynamic(Nil)
  }
}
