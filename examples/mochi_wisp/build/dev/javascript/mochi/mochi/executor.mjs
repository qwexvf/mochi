import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $result from "../../gleam_stdlib/gleam/result.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { Ok, Error, toList, Empty as $Empty, CustomType as $CustomType } from "../gleam.mjs";
import * as $ast from "../mochi/ast.mjs";
import * as $parser from "../mochi/parser.mjs";
import * as $schema from "../mochi/schema.mjs";
import * as $types from "../mochi/types.mjs";
import {
  dynamic_to_bool as decode_bool_from_dynamic,
  get_list_elements,
  is_null,
  dynamic_to_string as decode_string_from_dynamic,
} from "../mochi_ffi.mjs";

export class ExecutionResult extends $CustomType {
  constructor(data, errors) {
    super();
    this.data = data;
    this.errors = errors;
  }
}
export const ExecutionResult$ExecutionResult = (data, errors) =>
  new ExecutionResult(data, errors);
export const ExecutionResult$isExecutionResult = (value) =>
  value instanceof ExecutionResult;
export const ExecutionResult$ExecutionResult$data = (value) => value.data;
export const ExecutionResult$ExecutionResult$0 = (value) => value.data;
export const ExecutionResult$ExecutionResult$errors = (value) => value.errors;
export const ExecutionResult$ExecutionResult$1 = (value) => value.errors;

export class ValidationError extends $CustomType {
  constructor(message, path) {
    super();
    this.message = message;
    this.path = path;
  }
}
export const ExecutionError$ValidationError = (message, path) =>
  new ValidationError(message, path);
export const ExecutionError$isValidationError = (value) =>
  value instanceof ValidationError;
export const ExecutionError$ValidationError$message = (value) => value.message;
export const ExecutionError$ValidationError$0 = (value) => value.message;
export const ExecutionError$ValidationError$path = (value) => value.path;
export const ExecutionError$ValidationError$1 = (value) => value.path;

export class ResolverError extends $CustomType {
  constructor(message, path) {
    super();
    this.message = message;
    this.path = path;
  }
}
export const ExecutionError$ResolverError = (message, path) =>
  new ResolverError(message, path);
export const ExecutionError$isResolverError = (value) =>
  value instanceof ResolverError;
export const ExecutionError$ResolverError$message = (value) => value.message;
export const ExecutionError$ResolverError$0 = (value) => value.message;
export const ExecutionError$ResolverError$path = (value) => value.path;
export const ExecutionError$ResolverError$1 = (value) => value.path;

export class TypeError extends $CustomType {
  constructor(message, path) {
    super();
    this.message = message;
    this.path = path;
  }
}
export const ExecutionError$TypeError = (message, path) =>
  new TypeError(message, path);
export const ExecutionError$isTypeError = (value) => value instanceof TypeError;
export const ExecutionError$TypeError$message = (value) => value.message;
export const ExecutionError$TypeError$0 = (value) => value.message;
export const ExecutionError$TypeError$path = (value) => value.path;
export const ExecutionError$TypeError$1 = (value) => value.path;

export class NullValueError extends $CustomType {
  constructor(message, path) {
    super();
    this.message = message;
    this.path = path;
  }
}
export const ExecutionError$NullValueError = (message, path) =>
  new NullValueError(message, path);
export const ExecutionError$isNullValueError = (value) =>
  value instanceof NullValueError;
export const ExecutionError$NullValueError$message = (value) => value.message;
export const ExecutionError$NullValueError$0 = (value) => value.message;
export const ExecutionError$NullValueError$path = (value) => value.path;
export const ExecutionError$NullValueError$1 = (value) => value.path;

export const ExecutionError$message = (value) => value.message;
export const ExecutionError$path = (value) => value.path;

export class QueryExecutionContext extends $CustomType {
  constructor(schema, root_value, execution_context, variable_values, fragments) {
    super();
    this.schema = schema;
    this.root_value = root_value;
    this.execution_context = execution_context;
    this.variable_values = variable_values;
    this.fragments = fragments;
  }
}
export const QueryExecutionContext$QueryExecutionContext = (schema, root_value, execution_context, variable_values, fragments) =>
  new QueryExecutionContext(schema,
  root_value,
  execution_context,
  variable_values,
  fragments);
export const QueryExecutionContext$isQueryExecutionContext = (value) =>
  value instanceof QueryExecutionContext;
export const QueryExecutionContext$QueryExecutionContext$schema = (value) =>
  value.schema;
export const QueryExecutionContext$QueryExecutionContext$0 = (value) =>
  value.schema;
export const QueryExecutionContext$QueryExecutionContext$root_value = (value) =>
  value.root_value;
export const QueryExecutionContext$QueryExecutionContext$1 = (value) =>
  value.root_value;
export const QueryExecutionContext$QueryExecutionContext$execution_context = (value) =>
  value.execution_context;
export const QueryExecutionContext$QueryExecutionContext$2 = (value) =>
  value.execution_context;
export const QueryExecutionContext$QueryExecutionContext$variable_values = (value) =>
  value.variable_values;
export const QueryExecutionContext$QueryExecutionContext$3 = (value) =>
  value.variable_values;
export const QueryExecutionContext$QueryExecutionContext$fragments = (value) =>
  value.fragments;
export const QueryExecutionContext$QueryExecutionContext$4 = (value) =>
  value.fragments;

export class FieldContext extends $CustomType {
  constructor(parent_value, field_name, field_args, path) {
    super();
    this.parent_value = parent_value;
    this.field_name = field_name;
    this.field_args = field_args;
    this.path = path;
  }
}
export const FieldContext$FieldContext = (parent_value, field_name, field_args, path) =>
  new FieldContext(parent_value, field_name, field_args, path);
export const FieldContext$isFieldContext = (value) =>
  value instanceof FieldContext;
export const FieldContext$FieldContext$parent_value = (value) =>
  value.parent_value;
export const FieldContext$FieldContext$0 = (value) => value.parent_value;
export const FieldContext$FieldContext$field_name = (value) => value.field_name;
export const FieldContext$FieldContext$1 = (value) => value.field_name;
export const FieldContext$FieldContext$field_args = (value) => value.field_args;
export const FieldContext$FieldContext$2 = (value) => value.field_args;
export const FieldContext$FieldContext$path = (value) => value.path;
export const FieldContext$FieldContext$3 = (value) => value.path;

export class DebugContext extends $CustomType {
  constructor(enabled, indent_level, step_counter) {
    super();
    this.enabled = enabled;
    this.indent_level = indent_level;
    this.step_counter = step_counter;
  }
}
export const DebugContext$DebugContext = (enabled, indent_level, step_counter) =>
  new DebugContext(enabled, indent_level, step_counter);
export const DebugContext$isDebugContext = (value) =>
  value instanceof DebugContext;
export const DebugContext$DebugContext$enabled = (value) => value.enabled;
export const DebugContext$DebugContext$0 = (value) => value.enabled;
export const DebugContext$DebugContext$indent_level = (value) =>
  value.indent_level;
export const DebugContext$DebugContext$1 = (value) => value.indent_level;
export const DebugContext$DebugContext$step_counter = (value) =>
  value.step_counter;
export const DebugContext$DebugContext$2 = (value) => value.step_counter;

function ok_result(data) {
  return new ExecutionResult(new Some(data), toList([]));
}

function error_result(error) {
  return new ExecutionResult(new None(), toList([error]));
}

function validation_error(msg, path) {
  return error_result(new ValidationError(msg, path));
}

function resolver_error(msg, path) {
  return error_result(new ResolverError(msg, path));
}

function type_error(msg, path) {
  return error_result(new TypeError(msg, path));
}

function null_value_error(msg, path) {
  return error_result(new NullValueError(msg, path));
}

function extract_fragments(document) {
  let _pipe = document.definitions;
  let _pipe$1 = $list.filter_map(
    _pipe,
    (def) => {
      if (def instanceof $ast.OperationDefinition) {
        return new Error(undefined);
      } else {
        let fragment = def.fragment;
        return new Ok([fragment.name, fragment]);
      }
    },
  );
  return $dict.from_list(_pipe$1);
}

function get_root_type(schema_def, operation) {
  if (operation instanceof $ast.Operation) {
    let $ = operation.operation_type;
    if ($ instanceof $ast.Query) {
      return schema_def.query;
    } else if ($ instanceof $ast.Mutation) {
      return schema_def.mutation;
    } else {
      return schema_def.subscription;
    }
  } else {
    return schema_def.query;
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

function does_type_apply(schema_def, object_type_name, type_condition) {
  let $ = object_type_name === type_condition;
  if ($) {
    return $;
  } else {
    let $1 = $dict.get(schema_def.types, object_type_name);
    if ($1 instanceof Ok) {
      let $2 = $1[0];
      if ($2 instanceof $schema.ObjectTypeDef) {
        let obj = $2.object_type;
        return $list.any(
          obj.interfaces,
          (iface) => { return iface.name === type_condition; },
        );
      } else {
        return false;
      }
    } else {
      return false;
    }
  }
}

/**
 * Evaluate a Value to a boolean, handling variables
 * 
 * @ignore
 */
function eval_bool_value(value, variables) {
  if (value instanceof $ast.BooleanValue) {
    let b = value.value;
    return new Some(b);
  } else if (value instanceof $ast.VariableValue) {
    let name = value.name;
    let _pipe = $dict.get(variables, name);
    let _pipe$1 = $result.map(_pipe, decode_bool_from_dynamic);
    return $result.unwrap(_pipe$1, new None());
  } else {
    return new None();
  }
}

/**
 * Get a boolean argument value from a specific directive
 * 
 * @ignore
 */
function get_directive_bool_arg(directives, directive_name, arg_name, variables) {
  let _pipe = directives;
  let _pipe$1 = $list.find(_pipe, (d) => { return d.name === directive_name; });
  let _pipe$2 = $result.map(
    _pipe$1,
    (directive) => {
      let _pipe$2 = directive.arguments;
      let _pipe$3 = $list.find(
        _pipe$2,
        (arg) => { return arg.name === arg_name; },
      );
      let _pipe$4 = $result.map(
        _pipe$3,
        (arg) => { return eval_bool_value(arg.value, variables); },
      );
      return $result.unwrap(_pipe$4, new None());
    },
  );
  return $result.unwrap(_pipe$2, new None());
}

/**
 * Check if a field should be included based on @skip and @include directives
 * 
 * @ignore
 */
function should_include_field(directives, variables) {
  let skip_value = get_directive_bool_arg(directives, "skip", "if", variables);
  if (skip_value instanceof Some) {
    let $ = skip_value[0];
    if ($) {
      return false;
    } else {
      let include_value = get_directive_bool_arg(
        directives,
        "include",
        "if",
        variables,
      );
      if (include_value instanceof Some) {
        let $1 = include_value[0];
        if (!$1) {
          return false;
        } else {
          return true;
        }
      } else {
        return true;
      }
    }
  } else {
    let include_value = get_directive_bool_arg(
      directives,
      "include",
      "if",
      variables,
    );
    if (include_value instanceof Some) {
      let $ = include_value[0];
      if (!$) {
        return false;
      } else {
        return true;
      }
    } else {
      return true;
    }
  }
}

/**
 * Skip built-in directives and call continuation for custom ones
 * 
 * @ignore
 */
function skip_builtin_directive(name, value, next) {
  if (name === "skip") {
    return new Ok(value);
  } else if (name === "include") {
    return new Ok(value);
  } else if (name === "deprecated") {
    return new Ok(value);
  } else {
    return next();
  }
}

/**
 * Get directive definition or pass through value if not found
 * 
 * @ignore
 */
function with_directive_def(schema_def, name, value, next) {
  let $ = $dict.get(schema_def.directives, name);
  if ($ instanceof Ok) {
    let directive_def = $[0];
    return next(directive_def);
  } else {
    return new Ok(value);
  }
}

/**
 * Get directive handler or pass through value if none defined
 * 
 * @ignore
 */
function with_directive_handler(directive_def, value, next) {
  let $ = directive_def.handler;
  if ($ instanceof Some) {
    let handler = $[0];
    return next(handler);
  } else {
    return new Ok(value);
  }
}

function require_field(object_type, field_name, path, next) {
  let $ = $dict.get(object_type.fields, field_name);
  if ($ instanceof Ok) {
    let field_def = $[0];
    return next(field_def);
  } else {
    return validation_error(
      ((("Field '" + field_name) + "' not found on type '") + object_type.name) + "'",
      path,
    );
  }
}

/**
 * Check if a field type is a list type (unwrapping NonNull)
 * 
 * @ignore
 */
function is_list_field_type(loop$field_type) {
  while (true) {
    let field_type = loop$field_type;
    if (field_type instanceof $schema.NonNull) {
      let inner = field_type.inner;
      loop$field_type = inner;
    } else if (field_type instanceof $schema.List) {
      return true;
    } else {
      return false;
    }
  }
}

/**
 * Check if a field type is non-null at the outermost level
 * 
 * @ignore
 */
function is_non_null_type(field_type) {
  if (field_type instanceof $schema.NonNull) {
    return true;
  } else {
    return false;
  }
}

/**
 * Get the inner type of a list field type
 * 
 * @ignore
 */
function get_list_inner_type(loop$field_type) {
  while (true) {
    let field_type = loop$field_type;
    if (field_type instanceof $schema.NonNull) {
      let inner = field_type.inner;
      loop$field_type = inner;
    } else if (field_type instanceof $schema.List) {
      let inner = field_type.inner;
      return inner;
    } else {
      return field_type;
    }
  }
}

/**
 * Require a type resolver function exists
 * 
 * @ignore
 */
function require_type_resolver(resolve_type, path, next) {
  if (resolve_type instanceof Some) {
    let resolver = resolve_type[0];
    return next(resolver);
  } else {
    return type_error("Abstract type requires a resolve_type function", path);
  }
}

/**
 * Require the type resolver successfully returns a type name
 * 
 * @ignore
 */
function require_resolved_type(resolver, value, path, next) {
  let $ = resolver(value);
  if ($ instanceof Ok) {
    let type_name = $[0];
    return next(type_name);
  } else {
    let msg = $[0];
    return resolver_error("resolve_type failed: " + msg, path);
  }
}

/**
 * Require the type name resolves to an object type in the schema
 * 
 * @ignore
 */
function require_object_type(schema_def, type_name, path, next) {
  let $ = $dict.get(schema_def.types, type_name);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof $schema.ObjectTypeDef) {
      let concrete_type = $1.object_type;
      return next(concrete_type);
    } else {
      return type_error(
        "resolve_type returned non-object type: " + type_name,
        path,
      );
    }
  } else {
    return type_error("resolve_type returned unknown type: " + type_name, path);
  }
}

function get_field_type_definition(loop$schema_def, loop$field_type) {
  while (true) {
    let schema_def = loop$schema_def;
    let field_type = loop$field_type;
    if (field_type instanceof $schema.NonNull) {
      let inner = field_type.inner;
      loop$schema_def = schema_def;
      loop$field_type = inner;
    } else if (field_type instanceof $schema.List) {
      let inner = field_type.inner;
      loop$schema_def = schema_def;
      loop$field_type = inner;
    } else {
      let name = field_type.name;
      let _pipe = $dict.get(schema_def.types, name);
      return $result.map_error(
        _pipe,
        (_) => { return ("Type '" + name) + "' not found in schema"; },
      );
    }
  }
}

function add_default_values(args, arg_defs, provided_args) {
  let provided_names = $list.map(provided_args, (a) => { return a.name; });
  return $dict.fold(
    arg_defs,
    args,
    (acc, name, def) => {
      let $ = $list.contains(provided_names, name);
      let $1 = def.default_value;
      if (!$ && $1 instanceof Some) {
        let default$ = $1[0];
        return $dict.insert(acc, name, default$);
      } else {
        return acc;
      }
    },
  );
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

/**
 * Coerce directive arguments from AST to Dynamic values
 * 
 * @ignore
 */
function coerce_directive_arguments(args, variables) {
  return $list.fold(
    args,
    $dict.new$(),
    (acc, arg) => {
      return $dict.insert(acc, arg.name, coerce_value(arg.value, variables));
    },
  );
}

/**
 * Apply a single directive handler to a value.
 * Returns the transformed value or an error.
 * 
 * @ignore
 */
function apply_single_directive(schema_def, directive, value, variables, _) {
  return skip_builtin_directive(
    directive.name,
    value,
    () => {
      return with_directive_def(
        schema_def,
        directive.name,
        value,
        (directive_def) => {
          return with_directive_handler(
            directive_def,
            value,
            (handler) => {
              let args = coerce_directive_arguments(
                directive.arguments,
                variables,
              );
              return handler(args, value);
            },
          );
        },
      );
    },
  );
}

/**
 * Apply custom directive handlers to a resolved field value.
 * Directives are applied in order (left to right as they appear in the query).
 * Built-in directives (@skip, @include, @deprecated) are skipped as they
 * are handled specially elsewhere.
 * 
 * @ignore
 */
function apply_custom_directives(schema_def, directives, value, variables, path) {
  return $list.fold(
    directives,
    new Ok(value),
    (acc, directive) => {
      if (acc instanceof Ok) {
        let current_value = acc[0];
        return apply_single_directive(
          schema_def,
          directive,
          current_value,
          variables,
          path,
        );
      } else {
        return acc;
      }
    },
  );
}

function coerce_arguments(ast_args, arg_defs, variables) {
  let _pipe = ast_args;
  let _pipe$1 = $list.fold(
    _pipe,
    $dict.new$(),
    (acc, arg) => {
      return $dict.insert(acc, arg.name, coerce_value(arg.value, variables));
    },
  );
  return add_default_values(_pipe$1, arg_defs, ast_args);
}

function extract_string_value(value, variables) {
  if (value instanceof $ast.StringValue) {
    let s = value.value;
    return new Some(s);
  } else if (value instanceof $ast.VariableValue) {
    let var_name = value.name;
    let _pipe = $dict.get(variables, var_name);
    let _pipe$1 = $result.map(_pipe, decode_string_from_dynamic);
    return $result.unwrap(_pipe$1, new None());
  } else {
    return new None();
  }
}

function get_string_argument(args, name, variables) {
  let _pipe = $list.find(args, (arg) => { return arg.name === name; });
  let _pipe$1 = $result.map(
    _pipe,
    (arg) => { return extract_string_value(arg.value, variables); },
  );
  return $result.unwrap(_pipe$1, new None());
}

function build_type_ref(obj) {
  let _pipe = obj;
  let _pipe$1 = $option.map(
    _pipe,
    (o) => {
      return $types.to_dynamic(
        $dict.from_list(toList([["name", $types.to_dynamic(o.name)]])),
      );
    },
  );
  return $option.unwrap(_pipe$1, $types.to_dynamic(undefined));
}

function get_all_type_names(schema_def) {
  let builtin = toList(["String", "Int", "Float", "Boolean", "ID"]);
  let introspection = toList([
    "__Schema",
    "__Type",
    "__Field",
    "__InputValue",
    "__EnumValue",
    "__Directive",
    "__DirectiveLocation",
    "__TypeKind",
  ]);
  let user_types = $dict.keys(schema_def.types);
  let _block;
  let _pipe = toList([
    schema_def.query,
    schema_def.mutation,
    schema_def.subscription,
  ]);
  _block = $list.filter_map(
    _pipe,
    (opt) => {
      let _pipe$1 = $option.map(opt, (o) => { return o.name; });
      return $option.to_result(_pipe$1, undefined);
    },
  );
  let root_types = _block;
  let _pipe$1 = $list.flatten(
    toList([builtin, user_types, root_types, introspection]),
  );
  return $list.unique(_pipe$1);
}

function get_scalar_description(name) {
  if (name === "String") {
    return "The String scalar type represents textual data";
  } else if (name === "Int") {
    return "The Int scalar type represents non-fractional signed whole numeric values";
  } else if (name === "Float") {
    return "The Float scalar type represents signed double-precision fractional values";
  } else if (name === "Boolean") {
    return "The Boolean scalar type represents true or false";
  } else if (name === "ID") {
    return "The ID scalar type represents a unique identifier";
  } else {
    return "";
  }
}

function build_union_introspection(union) {
  let possible = $list.map(
    union.types,
    (t) => {
      return $types.to_dynamic(
        $dict.from_list(
          toList([
            ["kind", $types.to_dynamic("OBJECT")],
            ["name", $types.to_dynamic(t.name)],
          ]),
        ),
      );
    },
  );
  return $types.to_dynamic(
    $dict.from_list(
      toList([
        ["kind", $types.to_dynamic("UNION")],
        ["name", $types.to_dynamic(union.name)],
        [
          "description",
          $types.to_dynamic($option.unwrap(union.description, "")),
        ],
        ["possibleTypes", $types.to_dynamic(possible)],
        ["fields", $types.to_dynamic(undefined)],
        ["interfaces", $types.to_dynamic(undefined)],
        ["enumValues", $types.to_dynamic(undefined)],
        ["inputFields", $types.to_dynamic(undefined)],
        ["ofType", $types.to_dynamic(undefined)],
      ]),
    ),
  );
}

function make_type_object(
  kind,
  name,
  description,
  fields,
  interfaces,
  enum_values,
  input_fields
) {
  return $types.to_dynamic(
    $dict.from_list(
      toList([
        ["kind", $types.to_dynamic(kind)],
        ["name", $types.to_dynamic(name)],
        ["description", $types.to_dynamic(description)],
        [
          "fields",
          (() => {
            let _pipe = $option.map(fields, $types.to_dynamic);
            return $option.unwrap(_pipe, $types.to_dynamic(undefined));
          })(),
        ],
        [
          "interfaces",
          (() => {
            let _pipe = $option.map(interfaces, $types.to_dynamic);
            return $option.unwrap(_pipe, $types.to_dynamic(undefined));
          })(),
        ],
        ["possibleTypes", $types.to_dynamic(undefined)],
        [
          "enumValues",
          (() => {
            let _pipe = $option.map(enum_values, $types.to_dynamic);
            return $option.unwrap(_pipe, $types.to_dynamic(undefined));
          })(),
        ],
        [
          "inputFields",
          (() => {
            let _pipe = $option.map(input_fields, $types.to_dynamic);
            return $option.unwrap(_pipe, $types.to_dynamic(undefined));
          })(),
        ],
        ["ofType", $types.to_dynamic(undefined)],
      ]),
    ),
  );
}

function build_scalar_introspection(name) {
  return make_type_object(
    "SCALAR",
    name,
    get_scalar_description(name),
    new None(),
    new None(),
    new None(),
    new None(),
  );
}

function build_enum_introspection(enum$) {
  let _block;
  let _pipe = $dict.to_list(enum$.values);
  _block = $list.map(
    _pipe,
    (kv) => {
      let name;
      let def;
      name = kv[0];
      def = kv[1];
      return $types.to_dynamic(
        $dict.from_list(
          toList([
            ["name", $types.to_dynamic(name)],
            [
              "description",
              $types.to_dynamic($option.unwrap(def.description, "")),
            ],
            ["isDeprecated", $types.to_dynamic(def.is_deprecated)],
            [
              "deprecationReason",
              (() => {
                let $ = def.deprecation_reason;
                if ($ instanceof $option.Some) {
                  let reason = $[0];
                  return $types.to_dynamic(reason);
                } else {
                  return $types.to_dynamic(undefined);
                }
              })(),
            ],
          ]),
        ),
      );
    },
  );
  let values = _block;
  return make_type_object(
    "ENUM",
    enum$.name,
    $option.unwrap(enum$.description, ""),
    new None(),
    new None(),
    new Some(values),
    new None(),
  );
}

function build_meta_type_introspection(name) {
  if (name === "__Schema") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__Type") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__Field") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__InputValue") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__EnumValue") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__Directive") {
    return make_type_object(
      "OBJECT",
      name,
      "Introspection type",
      new Some(toList([])),
      new Some(toList([])),
      new None(),
      new None(),
    );
  } else if (name === "__TypeKind") {
    return make_type_object(
      "ENUM",
      name,
      "Introspection enum",
      new None(),
      new None(),
      new Some(toList([])),
      new None(),
    );
  } else if (name === "__DirectiveLocation") {
    return make_type_object(
      "ENUM",
      name,
      "Introspection enum",
      new None(),
      new None(),
      new Some(toList([])),
      new None(),
    );
  } else {
    return $types.to_dynamic(undefined);
  }
}

function get_type_kind(name) {
  if (name === "String") {
    return "SCALAR";
  } else if (name === "Int") {
    return "SCALAR";
  } else if (name === "Float") {
    return "SCALAR";
  } else if (name === "Boolean") {
    return "SCALAR";
  } else if (name === "ID") {
    return "SCALAR";
  } else {
    return "OBJECT";
  }
}

function build_field_type_introspection(field_type) {
  if (field_type instanceof $schema.NonNull) {
    let inner = field_type.inner;
    return $types.to_dynamic(
      $dict.from_list(
        toList([
          ["kind", $types.to_dynamic("NON_NULL")],
          ["name", $types.to_dynamic(undefined)],
          ["ofType", build_field_type_introspection(inner)],
        ]),
      ),
    );
  } else if (field_type instanceof $schema.List) {
    let inner = field_type.inner;
    return $types.to_dynamic(
      $dict.from_list(
        toList([
          ["kind", $types.to_dynamic("LIST")],
          ["name", $types.to_dynamic(undefined)],
          ["ofType", build_field_type_introspection(inner)],
        ]),
      ),
    );
  } else {
    let name = field_type.name;
    return $types.to_dynamic(
      $dict.from_list(
        toList([
          ["kind", $types.to_dynamic(get_type_kind(name))],
          ["name", $types.to_dynamic(name)],
          ["ofType", $types.to_dynamic(undefined)],
        ]),
      ),
    );
  }
}

function build_input_introspection(input) {
  let _block;
  let _pipe = $dict.to_list(input.fields);
  _block = $list.map(
    _pipe,
    (kv) => {
      let name;
      let def;
      name = kv[0];
      def = kv[1];
      return $types.to_dynamic(
        $dict.from_list(
          toList([
            ["name", $types.to_dynamic(name)],
            [
              "description",
              $types.to_dynamic($option.unwrap(def.description, "")),
            ],
            ["type", build_field_type_introspection(def.field_type)],
            ["defaultValue", $types.to_dynamic(undefined)],
          ]),
        ),
      );
    },
  );
  let fields = _block;
  return make_type_object(
    "INPUT_OBJECT",
    input.name,
    $option.unwrap(input.description, ""),
    new None(),
    new None(),
    new None(),
    new Some(fields),
  );
}

function build_fields_introspection(fields) {
  let _pipe = $dict.to_list(fields);
  return $list.map(
    _pipe,
    (kv) => {
      let name;
      let def;
      name = kv[0];
      def = kv[1];
      let _block;
      let _pipe$1 = $dict.to_list(def.arguments);
      _block = $list.map(
        _pipe$1,
        (arg_kv) => {
          let arg_name;
          let arg_def;
          arg_name = arg_kv[0];
          arg_def = arg_kv[1];
          return $types.to_dynamic(
            $dict.from_list(
              toList([
                ["name", $types.to_dynamic(arg_name)],
                [
                  "description",
                  $types.to_dynamic($option.unwrap(arg_def.description, "")),
                ],
                ["type", build_field_type_introspection(arg_def.arg_type)],
                ["defaultValue", $types.to_dynamic(undefined)],
              ]),
            ),
          );
        },
      );
      let args = _block;
      return $types.to_dynamic(
        $dict.from_list(
          toList([
            ["name", $types.to_dynamic(name)],
            [
              "description",
              $types.to_dynamic($option.unwrap(def.description, "")),
            ],
            ["args", $types.to_dynamic(args)],
            ["type", build_field_type_introspection(def.field_type)],
            ["isDeprecated", $types.to_dynamic(def.is_deprecated)],
            [
              "deprecationReason",
              (() => {
                let $ = def.deprecation_reason;
                if ($ instanceof $option.Some) {
                  let reason = $[0];
                  return $types.to_dynamic(reason);
                } else {
                  return $types.to_dynamic(undefined);
                }
              })(),
            ],
          ]),
        ),
      );
    },
  );
}

function build_object_introspection(obj) {
  let fields = build_fields_introspection(obj.fields);
  let interfaces = $list.map(
    obj.interfaces,
    (i) => {
      return $types.to_dynamic(
        $dict.from_list(
          toList([
            ["kind", $types.to_dynamic("INTERFACE")],
            ["name", $types.to_dynamic(i.name)],
          ]),
        ),
      );
    },
  );
  return make_type_object(
    "OBJECT",
    obj.name,
    $option.unwrap(obj.description, ""),
    new Some(fields),
    new Some(interfaces),
    new None(),
    new None(),
  );
}

function lookup_root_type(schema_def, name) {
  let _pipe = toList([
    schema_def.query,
    schema_def.mutation,
    schema_def.subscription,
  ]);
  let _pipe$1 = $list.find(
    _pipe,
    (opt) => {
      let _pipe$1 = $option.map(opt, (o) => { return o.name === name; });
      return $option.unwrap(_pipe$1, false);
    },
  );
  let _pipe$2 = $result.map(
    _pipe$1,
    (opt) => {
      let _pipe$2 = $option.map(opt, build_object_introspection);
      return $option.unwrap(_pipe$2, $types.to_dynamic(undefined));
    },
  );
  return $result.unwrap(_pipe$2, build_meta_type_introspection(name));
}

function build_interface_introspection(iface) {
  let fields = build_fields_introspection(iface.fields);
  return make_type_object(
    "INTERFACE",
    iface.name,
    $option.unwrap(iface.description, ""),
    new Some(fields),
    new None(),
    new None(),
    new None(),
  );
}

function build_type_def_introspection(type_def) {
  if (type_def instanceof $schema.ObjectTypeDef) {
    let obj = type_def.object_type;
    return build_object_introspection(obj);
  } else if (type_def instanceof $schema.ScalarTypeDef) {
    let scalar = type_def.scalar_type;
    return make_type_object(
      "SCALAR",
      scalar.name,
      $option.unwrap(scalar.description, ""),
      new None(),
      new None(),
      new None(),
      new None(),
    );
  } else if (type_def instanceof $schema.EnumTypeDef) {
    let enum$ = type_def.enum_type;
    return build_enum_introspection(enum$);
  } else if (type_def instanceof $schema.InterfaceTypeDef) {
    let iface = type_def.interface_type;
    return build_interface_introspection(iface);
  } else if (type_def instanceof $schema.UnionTypeDef) {
    let union = type_def.union_type;
    return build_union_introspection(union);
  } else {
    let input = type_def.input_object_type;
    return build_input_introspection(input);
  }
}

function lookup_type_introspection(schema_def, name) {
  let $ = $dict.get(schema_def.types, name);
  if ($ instanceof Ok) {
    let type_def = $[0];
    return build_type_def_introspection(type_def);
  } else {
    return lookup_root_type(schema_def, name);
  }
}

function build_type_introspection(schema_def, name) {
  if (name === "String") {
    return build_scalar_introspection(name);
  } else if (name === "Int") {
    return build_scalar_introspection(name);
  } else if (name === "Float") {
    return build_scalar_introspection(name);
  } else if (name === "Boolean") {
    return build_scalar_introspection(name);
  } else if (name === "ID") {
    return build_scalar_introspection(name);
  } else {
    return lookup_type_introspection(schema_def, name);
  }
}

function build_schema_introspection(schema_def, _) {
  return $types.to_dynamic(
    $dict.from_list(
      toList([
        ["queryType", build_type_ref(schema_def.query)],
        ["mutationType", build_type_ref(schema_def.mutation)],
        ["subscriptionType", build_type_ref(schema_def.subscription)],
        [
          "types",
          $types.to_dynamic(
            $list.map(
              get_all_type_names(schema_def),
              (_capture) => {
                return build_type_introspection(schema_def, _capture);
              },
            ),
          ),
        ],
        ["directives", $types.to_dynamic(toList([]))],
      ]),
    ),
  );
}

function make_field(name, value) {
  return $types.to_dynamic($dict.from_list(toList([[name, value]])));
}

/**
 * Check for null values and handle non-null field constraints
 * 
 * @ignore
 */
function with_non_null_check(
  resolved,
  field_type,
  response_name,
  field_path,
  next
) {
  let $ = is_null(resolved);
  let $1 = is_non_null_type(field_type);
  if ($) {
    if ($1) {
      return null_value_error(
        ("Cannot return null for non-null field '" + response_name) + "'",
        field_path,
      );
    } else {
      return ok_result(make_field(response_name, $types.to_dynamic(undefined)));
    }
  } else {
    return next(resolved);
  }
}

/**
 * Handle null errors in list results based on nullability
 * 
 * @ignore
 */
function handle_list_null_error(field_type, response_name, errors) {
  let $ = is_non_null_type(field_type);
  if ($) {
    return new ExecutionResult(new None(), errors);
  } else {
    return new ExecutionResult(
      new Some(make_field(response_name, $types.to_dynamic(undefined))),
      errors,
    );
  }
}

/**
 * Aggregate results from list item executions
 * 
 * @ignore
 */
function aggregate_list_results(results, field_type, response_name) {
  let errors = $list.flat_map(results, (r) => { return r.errors; });
  let has_null_error = $list.any(
    errors,
    (e) => {
      if (e instanceof NullValueError) {
        return true;
      } else {
        return false;
      }
    },
  );
  if (has_null_error) {
    return handle_list_null_error(field_type, response_name, errors);
  } else {
    let data_list = $list.filter_map(
      results,
      (r) => { return $option.to_result(r.data, undefined); },
    );
    if (errors instanceof $Empty) {
      return ok_result(make_field(response_name, $types.to_dynamic(data_list)));
    } else {
      return new ExecutionResult(
        new Some(make_field(response_name, $types.to_dynamic(data_list))),
        errors,
      );
    }
  }
}

/**
 * Wrap an execution result's data in a field
 * 
 * @ignore
 */
function wrap_result_in_field(result, name) {
  let $ = result.data;
  if ($ instanceof Some) {
    let data = $[0];
    return new ExecutionResult(new Some(make_field(name, data)), result.errors);
  } else {
    return result;
  }
}

/**
 * Handle null propagation from sub-selection results
 * 
 * @ignore
 */
function handle_sub_selection_result(sub_result, field_type, response_name) {
  let $ = sub_result.data;
  if ($ instanceof Some) {
    return wrap_result_in_field(sub_result, response_name);
  } else {
    let $1 = is_non_null_type(field_type);
    if ($1) {
      return sub_result;
    } else {
      return new ExecutionResult(
        new Some(make_field(response_name, $types.to_dynamic(undefined))),
        sub_result.errors,
      );
    }
  }
}

function resolve_from_parent(parent, _, response_name, field_path) {
  let _pipe = parent;
  let _pipe$1 = $option.map(
    _pipe,
    (_) => {
      return ok_result(make_field(response_name, $types.to_dynamic(undefined)));
    },
  );
  return $option.unwrap(
    _pipe$1,
    resolver_error("No resolver and no parent value", field_path),
  );
}

function execute_introspection_schema(context, field, response_name) {
  return ok_result(
    make_field(response_name, build_schema_introspection(context.schema, field)),
  );
}

function execute_introspection_type(context, field, field_path, response_name) {
  let _pipe = get_string_argument(
    field.arguments,
    "name",
    context.variable_values,
  );
  let _pipe$1 = $option.map(
    _pipe,
    (name) => {
      return ok_result(
        make_field(
          response_name,
          build_type_introspection(context.schema, name),
        ),
      );
    },
  );
  return $option.unwrap(
    _pipe$1,
    validation_error("Missing required argument 'name' for __type", field_path),
  );
}

function merge_results(results) {
  if (results instanceof $Empty) {
    return $types.to_dynamic($dict.new$());
  } else {
    return $types.to_dynamic(results);
  }
}

function log_step(debug, msg) {
  let $ = debug.enabled;
  if ($) {
    return $io.println(($string.repeat("  ", debug.indent_level) + "📋 ") + msg);
  } else {
    return undefined;
  }
}

function log_info(debug, msg) {
  let $ = debug.enabled;
  if ($) {
    return $io.println(($string.repeat("  ", debug.indent_level) + "🔍 ") + msg);
  } else {
    return undefined;
  }
}

function log_success(debug, msg) {
  let $ = debug.enabled;
  if ($) {
    return $io.println(($string.repeat("  ", debug.indent_level) + "✅ ") + msg);
  } else {
    return undefined;
  }
}

function log_error(debug, msg) {
  let $ = debug.enabled;
  if ($) {
    return $io.println(($string.repeat("  ", debug.indent_level) + "❌ ") + msg);
  } else {
    return undefined;
  }
}

function format_parse_error(error) {
  if (error instanceof $parser.LexError) {
    return "Lexer error";
  } else if (error instanceof $parser.UnexpectedToken) {
    let expected = error.expected;
    return "Expected " + expected;
  } else {
    let expected = error.expected;
    return "Unexpected EOF, expected " + expected;
  }
}

function execute_inline_fragment(context, inline, object_type, field_context) {
  let $ = inline.type_condition;
  if ($ instanceof Some) {
    let type_name = $[0];
    let $1 = does_type_apply(context.schema, object_type.name, type_name);
    if ($1) {
      return execute_selection_set(
        context,
        inline.selection_set,
        object_type,
        field_context,
      );
    } else {
      return ok_result($types.to_dynamic($dict.new$()));
    }
  } else {
    return execute_selection_set(
      context,
      inline.selection_set,
      object_type,
      field_context,
    );
  }
}

function execute_selection_set(
  context,
  selection_set,
  object_type,
  field_context
) {
  let results = $list.map(
    selection_set.selections,
    (selection) => {
      return execute_selection(context, selection, object_type, field_context);
    },
  );
  let errors = $list.flat_map(results, (r) => { return r.errors; });
  let data_list = $list.filter_map(
    results,
    (r) => { return $option.to_result(r.data, undefined); },
  );
  let has_none_data = $list.any(
    results,
    (r) => { return $option.is_none(r.data); },
  );
  if (has_none_data) {
    return new ExecutionResult(new None(), errors);
  } else {
    if (data_list instanceof $Empty) {
      if (errors instanceof $Empty) {
        return ok_result($types.to_dynamic($dict.new$()));
      } else {
        return new ExecutionResult(new None(), errors);
      }
    } else {
      return new ExecutionResult(new Some(merge_results(data_list)), errors);
    }
  }
}

function execute_selection(context, selection, object_type, field_context) {
  if (selection instanceof $ast.FieldSelection) {
    let field = selection.field;
    return execute_field(context, field, object_type, field_context);
  } else if (selection instanceof $ast.FragmentSpread) {
    let spread = selection.fragment_spread;
    return execute_fragment_spread(context, spread, object_type, field_context);
  } else {
    let inline = selection.inline_fragment;
    return execute_inline_fragment(context, inline, object_type, field_context);
  }
}

function execute_operation(context, operation) {
  let root_type = get_root_type(context.schema, operation);
  let selection_set = get_selection_set(operation);
  let _pipe = root_type;
  let _pipe$1 = $option.map(
    _pipe,
    (obj_type) => {
      let field_ctx = new FieldContext(
        context.root_value,
        "root",
        $dict.new$(),
        toList([]),
      );
      return execute_selection_set(context, selection_set, obj_type, field_ctx);
    },
  );
  return $option.unwrap(
    _pipe$1,
    validation_error(
      "Schema does not define a root type for this operation",
      toList([]),
    ),
  );
}

function execute_definition(context, definition) {
  if (definition instanceof $ast.OperationDefinition) {
    let operation = definition.operation;
    return execute_operation(context, operation);
  } else {
    return validation_error(
      "Fragment definitions not yet supported",
      toList([]),
    );
  }
}

export function execute(
  schema_def,
  document,
  root_value,
  execution_context,
  variable_values
) {
  let fragments = extract_fragments(document);
  let context = new QueryExecutionContext(
    schema_def,
    root_value,
    execution_context,
    variable_values,
    fragments,
  );
  let _pipe = document.definitions;
  let _pipe$1 = $list.find(
    _pipe,
    (def) => {
      if (def instanceof $ast.OperationDefinition) {
        return true;
      } else {
        return false;
      }
    },
  );
  let _pipe$2 = $result.map(
    _pipe$1,
    (_capture) => { return execute_definition(context, _capture); },
  );
  return $result.unwrap(
    _pipe$2,
    validation_error("Document must contain at least one operation", toList([])),
  );
}

function execute_fragment_spread(context, spread, object_type, field_context) {
  let $ = $dict.get(context.fragments, spread.name);
  if ($ instanceof Ok) {
    let fragment = $[0];
    let $1 = does_type_apply(
      context.schema,
      object_type.name,
      fragment.type_condition,
    );
    if ($1) {
      return execute_selection_set(
        context,
        fragment.selection_set,
        object_type,
        field_context,
      );
    } else {
      return ok_result($types.to_dynamic($dict.new$()));
    }
  } else {
    return validation_error(
      ("Fragment '" + spread.name) + "' is not defined",
      field_context.path,
    );
  }
}

function execute_abstract_type(
  context,
  sub_selection_set,
  field_args,
  field_path,
  resolved_value,
  resolve_type
) {
  return require_type_resolver(
    resolve_type,
    field_path,
    (resolver) => {
      return require_resolved_type(
        resolver,
        resolved_value,
        field_path,
        (type_name) => {
          return require_object_type(
            context.schema,
            type_name,
            field_path,
            (concrete_type) => {
              let sub_ctx = new FieldContext(
                new Some(resolved_value),
                "",
                field_args,
                field_path,
              );
              return execute_selection_set(
                context,
                sub_selection_set,
                concrete_type,
                sub_ctx,
              );
            },
          );
        },
      );
    },
  );
}

function execute_sub_selection(
  context,
  sub_selection_set,
  field_def,
  field_args,
  field_path,
  resolved_value
) {
  let $ = get_field_type_definition(context.schema, field_def.field_type);
  if ($ instanceof Ok) {
    let $1 = $[0];
    if ($1 instanceof $schema.ObjectTypeDef) {
      let sub_type = $1.object_type;
      let sub_ctx = new FieldContext(
        new Some(resolved_value),
        field_def.name,
        field_args,
        field_path,
      );
      return execute_selection_set(
        context,
        sub_selection_set,
        sub_type,
        sub_ctx,
      );
    } else if ($1 instanceof $schema.InterfaceTypeDef) {
      let iface = $1.interface_type;
      return execute_abstract_type(
        context,
        sub_selection_set,
        field_args,
        field_path,
        resolved_value,
        iface.resolve_type,
      );
    } else if ($1 instanceof $schema.UnionTypeDef) {
      let union = $1.union_type;
      return execute_abstract_type(
        context,
        sub_selection_set,
        field_args,
        field_path,
        resolved_value,
        union.resolve_type,
      );
    } else {
      return type_error(
        "Cannot execute selection set on non-object type",
        field_path,
      );
    }
  } else {
    let msg = $[0];
    return type_error(msg, field_path);
  }
}

/**
 * Execute selection set on a single list item
 * 
 * @ignore
 */
function execute_list_item(
  context,
  selection_set,
  field_def,
  field_args,
  element,
  index,
  field_path,
  items_non_null
) {
  let item_path = $list.append(field_path, toList([$int.to_string(index)]));
  let $ = is_null(element);
  if ($) {
    if (items_non_null) {
      return null_value_error(
        "Cannot return null for non-null list item at index " + $int.to_string(
          index,
        ),
        item_path,
      );
    } else {
      return ok_result($types.to_dynamic(undefined));
    }
  } else {
    return execute_sub_selection(
      context,
      selection_set,
      field_def,
      field_args,
      item_path,
      element,
    );
  }
}

/**
 * Execute selection set on a list of items
 * 
 * @ignore
 */
function execute_list_sub_selection(
  context,
  sub_selection_set,
  field_def,
  field_args,
  response_name,
  field_path,
  resolved_value
) {
  let $ = get_list_elements(resolved_value);
  if ($ instanceof Some) {
    let elements = $[0];
    let inner_type = get_list_inner_type(field_def.field_type);
    let inner_field_def = new $schema.FieldDefinition(
      field_def.name,
      field_def.description,
      inner_type,
      field_def.arguments,
      field_def.resolver,
      field_def.is_deprecated,
      field_def.deprecation_reason,
    );
    let items_are_non_null = is_non_null_type(inner_type);
    let results = $list.index_map(
      elements,
      (element, index) => {
        return execute_list_item(
          context,
          sub_selection_set,
          inner_field_def,
          field_args,
          element,
          index,
          field_path,
          items_are_non_null,
        );
      },
    );
    return aggregate_list_results(results, field_def.field_type, response_name);
  } else {
    let sub_result = execute_sub_selection(
      context,
      sub_selection_set,
      field_def,
      field_args,
      field_path,
      resolved_value,
    );
    return handle_sub_selection_result(
      sub_result,
      field_def.field_type,
      response_name,
    );
  }
}

/**
 * Handle selection set execution for a field with a resolved value
 * 
 * @ignore
 */
function handle_selection_set(
  context,
  sub_ss,
  field_def,
  field_args,
  response_name,
  field_path,
  resolved
) {
  let $ = is_list_field_type(field_def.field_type);
  if ($) {
    return execute_list_sub_selection(
      context,
      sub_ss,
      field_def,
      field_args,
      response_name,
      field_path,
      resolved,
    );
  } else {
    let sub_result = execute_sub_selection(
      context,
      sub_ss,
      field_def,
      field_args,
      field_path,
      resolved,
    );
    return handle_sub_selection_result(
      sub_result,
      field_def.field_type,
      response_name,
    );
  }
}

function handle_resolved_value(
  context,
  field,
  field_def,
  field_args,
  response_name,
  field_path,
  resolved
) {
  return with_non_null_check(
    resolved,
    field_def.field_type,
    response_name,
    field_path,
    (resolved_value) => {
      let $ = field.selection_set;
      if ($ instanceof Some) {
        let sub_ss = $[0];
        return handle_selection_set(
          context,
          sub_ss,
          field_def,
          field_args,
          response_name,
          field_path,
          resolved_value,
        );
      } else {
        return ok_result(make_field(response_name, resolved_value));
      }
    },
  );
}

function resolve_field(
  context,
  field,
  field_def,
  field_args,
  response_name,
  field_path,
  resolver,
  parent_value,
  directives
) {
  let resolver_info = new $schema.ResolverInfo(
    parent_value,
    field_args,
    context.execution_context,
    $types.to_dynamic($dict.new$()),
  );
  let $ = resolver(resolver_info);
  if ($ instanceof Ok) {
    let resolved = $[0];
    let $1 = apply_custom_directives(
      context.schema,
      directives,
      resolved,
      context.variable_values,
      field_path,
    );
    if ($1 instanceof Ok) {
      let transformed = $1[0];
      return handle_resolved_value(
        context,
        field,
        field_def,
        field_args,
        response_name,
        field_path,
        transformed,
      );
    } else {
      let msg = $1[0];
      return resolver_error(msg, field_path);
    }
  } else {
    let msg = $[0];
    return resolver_error(msg, field_path);
  }
}

function execute_regular_field(
  context,
  field,
  object_type,
  field_context,
  response_name,
  field_path
) {
  return require_field(
    object_type,
    field.name,
    field_path,
    (field_def) => {
      let field_args = coerce_arguments(
        field.arguments,
        field_def.arguments,
        context.variable_values,
      );
      let $ = field_def.resolver;
      if ($ instanceof Some) {
        let resolver = $[0];
        return resolve_field(
          context,
          field,
          field_def,
          field_args,
          response_name,
          field_path,
          resolver,
          field_context.parent_value,
          field.directives,
        );
      } else {
        return resolve_from_parent(
          field_context.parent_value,
          field.name,
          response_name,
          field_path,
        );
      }
    },
  );
}

function execute_field(context, field, object_type, field_context) {
  let $ = should_include_field(field.directives, context.variable_values);
  if ($) {
    let response_name = $option.unwrap(field.alias, field.name);
    let field_path = $list.append(field_context.path, toList([response_name]));
    let $1 = field.name;
    if ($1 === "__typename") {
      return ok_result(
        make_field(response_name, $types.to_dynamic(object_type.name)),
      );
    } else if ($1 === "__schema") {
      return execute_introspection_schema(context, field, response_name);
    } else if ($1 === "__type") {
      return execute_introspection_type(
        context,
        field,
        field_path,
        response_name,
      );
    } else {
      return execute_regular_field(
        context,
        field,
        object_type,
        field_context,
        response_name,
        field_path,
      );
    }
  } else {
    return ok_result($types.to_dynamic($dict.new$()));
  }
}

export function execute_query_with_variables(schema_def, query, variables) {
  let _pipe = $parser.parse(query);
  let _pipe$1 = $result.map(
    _pipe,
    (document) => {
      let ctx = $schema.execution_context($types.to_dynamic($dict.new$()));
      return execute(schema_def, document, new None(), ctx, variables);
    },
  );
  return $result.unwrap(
    _pipe$1,
    validation_error("Failed to parse query", toList([])),
  );
}

export function execute_query(schema_def, query) {
  return execute_query_with_variables(schema_def, query, $dict.new$());
}

export function execute_query_debug_with_variables(schema_def, query, variables) {
  let debug = new DebugContext(true, 0, 1);
  log_step(debug, "Starting GraphQL Query Execution");
  log_info(debug, "Query: " + query);
  log_step(debug, "Parsing Query");
  let $ = $parser.parse(query);
  if ($ instanceof Ok) {
    let document = $[0];
    log_success(debug, "Parse successful");
    log_info(
      debug,
      "Definitions: " + $int.to_string($list.length(document.definitions)),
    );
    log_step(debug, "Executing Query");
    let result = execute_query_with_variables(schema_def, query, variables);
    let $1 = result.data;
    if ($1 instanceof Some) {
      log_success(debug, "Query executed successfully!")
    } else {
      log_error(
        debug,
        ("Query execution failed: " + $int.to_string(
          $list.length(result.errors),
        )) + " errors",
      )
    }
    return result;
  } else {
    let error = $[0];
    log_error(debug, "Parse failed: " + format_parse_error(error));
    return validation_error(
      "Parse error: " + format_parse_error(error),
      toList([]),
    );
  }
}

export function execute_query_debug(schema_def, query) {
  return execute_query_debug_with_variables(schema_def, query, $dict.new$());
}
