import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import { Ok, Error, toList, Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";
import * as $ast from "../mochi/ast.mjs";
import * as $executor from "../mochi/executor.mjs";
import * as $parser from "../mochi/parser.mjs";
import * as $schema from "../mochi/schema.mjs";
import * as $subscription from "../mochi/subscription.mjs";
import * as $types from "../mochi/types.mjs";
import { extract_field as extract_field_from_dynamic } from "../mochi_ffi.mjs";

export class SubscriptionResult extends $CustomType {
  constructor(subscription_id, topic, pubsub) {
    super();
    this.subscription_id = subscription_id;
    this.topic = topic;
    this.pubsub = pubsub;
  }
}
export const SubscriptionResult$SubscriptionResult = (subscription_id, topic, pubsub) =>
  new SubscriptionResult(subscription_id, topic, pubsub);
export const SubscriptionResult$isSubscriptionResult = (value) =>
  value instanceof SubscriptionResult;
export const SubscriptionResult$SubscriptionResult$subscription_id = (value) =>
  value.subscription_id;
export const SubscriptionResult$SubscriptionResult$0 = (value) =>
  value.subscription_id;
export const SubscriptionResult$SubscriptionResult$topic = (value) =>
  value.topic;
export const SubscriptionResult$SubscriptionResult$1 = (value) => value.topic;
export const SubscriptionResult$SubscriptionResult$pubsub = (value) =>
  value.pubsub;
export const SubscriptionResult$SubscriptionResult$2 = (value) => value.pubsub;

export class SubscriptionError extends $CustomType {
  constructor(message) {
    super();
    this.message = message;
  }
}
export const SubscriptionResult$SubscriptionError = (message) =>
  new SubscriptionError(message);
export const SubscriptionResult$isSubscriptionError = (value) =>
  value instanceof SubscriptionError;
export const SubscriptionResult$SubscriptionError$message = (value) =>
  value.message;
export const SubscriptionResult$SubscriptionError$0 = (value) => value.message;

export class SubscriptionContext extends $CustomType {
  constructor(schema, pubsub, execution_context, variable_values) {
    super();
    this.schema = schema;
    this.pubsub = pubsub;
    this.execution_context = execution_context;
    this.variable_values = variable_values;
  }
}
export const SubscriptionContext$SubscriptionContext = (schema, pubsub, execution_context, variable_values) =>
  new SubscriptionContext(schema, pubsub, execution_context, variable_values);
export const SubscriptionContext$isSubscriptionContext = (value) =>
  value instanceof SubscriptionContext;
export const SubscriptionContext$SubscriptionContext$schema = (value) =>
  value.schema;
export const SubscriptionContext$SubscriptionContext$0 = (value) =>
  value.schema;
export const SubscriptionContext$SubscriptionContext$pubsub = (value) =>
  value.pubsub;
export const SubscriptionContext$SubscriptionContext$1 = (value) =>
  value.pubsub;
export const SubscriptionContext$SubscriptionContext$execution_context = (value) =>
  value.execution_context;
export const SubscriptionContext$SubscriptionContext$2 = (value) =>
  value.execution_context;
export const SubscriptionContext$SubscriptionContext$variable_values = (value) =>
  value.variable_values;
export const SubscriptionContext$SubscriptionContext$3 = (value) =>
  value.variable_values;

/**
 * Unsubscribe from a subscription
 */
export function unsubscribe(pubsub, subscription_id) {
  return $subscription.unsubscribe(pubsub, subscription_id);
}

/**
 * Publish an event to subscribers
 * This will execute the selection set for each subscriber and call their callbacks
 */
export function publish_event(context, topic, event_data) {
  return $subscription.publish(context.pubsub, topic, event_data);
}

function find_subscription_operation(document) {
  let _pipe = document.definitions;
  let _pipe$1 = $list.find_map(
    _pipe,
    (def) => {
      if (def instanceof $ast.OperationDefinition) {
        let $ = def.operation;
        if ($ instanceof $ast.Operation) {
          let $1 = $.operation_type;
          if ($1 instanceof $ast.Subscription) {
            let op = $;
            return new Ok(op);
          } else {
            return new Error(undefined);
          }
        } else {
          return new Error(undefined);
        }
      } else {
        return new Error(undefined);
      }
    },
  );
  let _pipe$2 = $result.map(_pipe$1, (var0) => { return new Some(var0); });
  return $result.unwrap(_pipe$2, new None());
}

/**
 * Require the schema defines a subscription type
 * 
 * @ignore
 */
function require_subscription_type(schema_def, next) {
  let $ = schema_def.subscription;
  if ($ instanceof Some) {
    let subscription_type = $[0];
    return next(subscription_type);
  } else {
    return new SubscriptionError("Schema does not define a Subscription type");
  }
}

/**
 * Require the field exists on the subscription type
 * 
 * @ignore
 */
function require_subscription_field(subscription_type, field_name, next) {
  let $ = $dict.get(subscription_type.fields, field_name);
  if ($ instanceof Ok) {
    let field_def = $[0];
    return next(field_def);
  } else {
    return new SubscriptionError(
      ("Field '" + field_name) + "' not found on Subscription type",
    );
  }
}

function get_selection_set(operation) {
  if (operation instanceof $ast.Operation) {
    let ss = operation.selection_set;
    return ss;
  } else {
    let ss = operation.selection_set;
    return ss;
  }
}

function get_single_root_field(selection_set) {
  let fields = $list.filter_map(
    selection_set.selections,
    (selection) => {
      if (selection instanceof $ast.FieldSelection) {
        let field = selection.field;
        return new Ok(field);
      } else {
        return new Error(undefined);
      }
    },
  );
  if (fields instanceof $Empty) {
    return new Error("Subscription must have at least one field");
  } else {
    let $ = fields.tail;
    if ($ instanceof $Empty) {
      let field = fields.head;
      return new Ok(field);
    } else {
      return new Error("Subscription must have exactly one root field");
    }
  }
}

/**
 * Require exactly one root field in selection set
 * 
 * @ignore
 */
function require_single_root_field_result(selection_set, next) {
  let $ = get_single_root_field(selection_set);
  if ($ instanceof Ok) {
    let field = $[0];
    return next(field);
  } else {
    let msg = $[0];
    return new SubscriptionError(msg);
  }
}

function coerce_value(value, variables) {
  if (value instanceof $ast.IntValue) {
    let i = value.value;
    return $types.to_dynamic(i);
  } else if (value instanceof $ast.FloatValue) {
    let f = value.value;
    return $types.to_dynamic(f);
  } else if (value instanceof $ast.StringValue) {
    let s = value.value;
    return $types.to_dynamic(s);
  } else if (value instanceof $ast.BooleanValue) {
    let b = value.value;
    return $types.to_dynamic(b);
  } else if (value instanceof $ast.NullValue) {
    return $types.to_dynamic(undefined);
  } else if (value instanceof $ast.EnumValue) {
    let e = value.value;
    return $types.to_dynamic(e);
  } else if (value instanceof $ast.ListValue) {
    let values = value.values;
    return $types.to_dynamic(
      $list.map(
        values,
        (_capture) => { return coerce_value(_capture, variables); },
      ),
    );
  } else if (value instanceof $ast.ObjectValue) {
    let fields = value.fields;
    return $types.to_dynamic(
      $list.fold(
        fields,
        $dict.new$(),
        (acc, f) => {
          return $dict.insert(acc, f.name, coerce_value(f.value, variables));
        },
      ),
    );
  } else {
    let name = value.name;
    let _pipe = $dict.get(variables, name);
    return $result.unwrap(_pipe, $types.to_dynamic(undefined));
  }
}

function coerce_arguments(ast_args, variables) {
  return $list.fold(
    ast_args,
    $dict.new$(),
    (acc, arg) => {
      return $dict.insert(acc, arg.name, coerce_value(arg.value, variables));
    },
  );
}

function get_field_type_name(loop$field_type) {
  while (true) {
    let field_type = loop$field_type;
    if (field_type instanceof $schema.NonNull) {
      let inner = field_type.inner;
      loop$field_type = inner;
    } else if (field_type instanceof $schema.List) {
      let inner = field_type.inner;
      loop$field_type = inner;
    } else {
      let name = field_type.name;
      return new Some(name);
    }
  }
}

function make_field(name, value) {
  return $types.to_dynamic($dict.from_list(toList([[name, value]])));
}

function merge_field_results(results) {
  return $types.to_dynamic(results);
}

function execute_event_field(context, field, object_type, event_data) {
  let response_name = $option.unwrap(field.alias, field.name);
  let $ = field.name;
  if ($ === "__typename") {
    return new $executor.ExecutionResult(
      new Some(make_field(response_name, $types.to_dynamic(object_type.name))),
      toList([]),
    );
  } else {
    let $1 = $dict.get(object_type.fields, field.name);
    if ($1 instanceof Ok) {
      let field_def = $1[0];
      let $2 = field_def.resolver;
      if ($2 instanceof Some) {
        let resolver = $2[0];
        let resolver_info = new $schema.ResolverInfo(
          new Some(event_data),
          coerce_arguments(field.arguments, context.variable_values),
          context.execution_context,
          $types.to_dynamic($dict.new$()),
        );
        let $3 = resolver(resolver_info);
        if ($3 instanceof Ok) {
          let value = $3[0];
          return new $executor.ExecutionResult(
            new Some(make_field(response_name, value)),
            toList([]),
          );
        } else {
          let msg = $3[0];
          return new $executor.ExecutionResult(
            new None(),
            toList([new $executor.ResolverError(msg, toList([response_name]))]),
          );
        }
      } else {
        let value = extract_field_from_dynamic(event_data, field.name);
        return new $executor.ExecutionResult(
          new Some(make_field(response_name, value)),
          toList([]),
        );
      }
    } else {
      return new $executor.ExecutionResult(
        new None(),
        toList([
          new $executor.ValidationError(
            ("Field '" + field.name) + "' not found",
            toList([]),
          ),
        ]),
      );
    }
  }
}

function execute_selection_on_event(
  context,
  selection_set,
  object_type,
  event_data,
  response_name
) {
  let field_results = $list.filter_map(
    selection_set.selections,
    (selection) => {
      if (selection instanceof $ast.FieldSelection) {
        let field = selection.field;
        return new Ok(
          execute_event_field(context, field, object_type, event_data),
        );
      } else {
        return new Error(undefined);
      }
    },
  );
  let errors = $list.flat_map(field_results, (r) => { return r.errors; });
  let data_parts = $list.filter_map(
    field_results,
    (r) => { return $option.to_result(r.data, undefined); },
  );
  if (data_parts instanceof $Empty) {
    return new $executor.ExecutionResult(
      new Some(make_field(response_name, $types.to_dynamic($dict.new$()))),
      errors,
    );
  } else {
    let merged = merge_field_results(data_parts);
    return new $executor.ExecutionResult(
      new Some(make_field(response_name, merged)),
      errors,
    );
  }
}

function execute_subscription_event(context, _, field, field_def, event_data) {
  let response_name = $option.unwrap(field.alias, field.name);
  let $ = field.selection_set;
  if ($ instanceof Some) {
    let sub_ss = $[0];
    let $1 = get_field_type_name(field_def.field_type);
    if ($1 instanceof Some) {
      let type_name = $1[0];
      let $2 = $dict.get(context.schema.types, type_name);
      if ($2 instanceof Ok) {
        let $3 = $2[0];
        if ($3 instanceof $schema.ObjectTypeDef) {
          let obj_type = $3.object_type;
          return execute_selection_on_event(
            context,
            sub_ss,
            obj_type,
            event_data,
            response_name,
          );
        } else {
          return new $executor.ExecutionResult(
            new Some(make_field(response_name, event_data)),
            toList([]),
          );
        }
      } else {
        return new $executor.ExecutionResult(
          new Some(make_field(response_name, event_data)),
          toList([]),
        );
      }
    } else {
      return new $executor.ExecutionResult(
        new Some(make_field(response_name, event_data)),
        toList([]),
      );
    }
  } else {
    return new $executor.ExecutionResult(
      new Some(make_field(response_name, event_data)),
      toList([]),
    );
  }
}

function execute_subscription(context, document, operation, callback) {
  return require_subscription_type(
    context.schema,
    (subscription_type) => {
      let selection_set = get_selection_set(operation);
      return require_single_root_field_result(
        selection_set,
        (field) => {
          return require_subscription_field(
            subscription_type,
            field.name,
            (field_def) => {
              let args = coerce_arguments(
                field.arguments,
                context.variable_values,
              );
              let topic = field.name;
              let event_callback = (event_data) => {
                let result = execute_subscription_event(
                  context,
                  document,
                  field,
                  field_def,
                  event_data,
                );
                return callback(result);
              };
              let result = $subscription.subscribe(
                context.pubsub,
                topic,
                field.name,
                args,
                event_callback,
              );
              return new SubscriptionResult(
                result.subscription_id,
                topic,
                result.pubsub,
              );
            },
          );
        },
      );
    },
  );
}

/**
 * Subscribe with a pre-parsed document
 */
export function subscribe_document(context, document, callback) {
  let $ = find_subscription_operation(document);
  if ($ instanceof Some) {
    let operation = $[0];
    return execute_subscription(context, document, operation, callback);
  } else {
    return new SubscriptionError(
      "Document does not contain a subscription operation",
    );
  }
}

/**
 * Subscribe to a GraphQL subscription operation
 * Returns the subscription ID and updated PubSub, or an error
 */
export function subscribe(context, query, callback) {
  let $ = $parser.parse(query);
  if ($ instanceof Ok) {
    let document = $[0];
    return subscribe_document(context, document, callback);
  } else {
    return new SubscriptionError("Failed to parse subscription query");
  }
}
