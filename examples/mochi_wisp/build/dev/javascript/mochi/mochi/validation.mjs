import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $set from "../../gleam_stdlib/gleam/set.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../gleam.mjs";
import * as $ast from "../mochi/ast.mjs";
import * as $parser from "../mochi/parser.mjs";
import * as $schema from "../mochi/schema.mjs";

/**
 * Field does not exist on the type
 */
export class UnknownField extends $CustomType {
  constructor(field_name, type_name) {
    super();
    this.field_name = field_name;
    this.type_name = type_name;
  }
}
export const ValidationError$UnknownField = (field_name, type_name) =>
  new UnknownField(field_name, type_name);
export const ValidationError$isUnknownField = (value) =>
  value instanceof UnknownField;
export const ValidationError$UnknownField$field_name = (value) =>
  value.field_name;
export const ValidationError$UnknownField$0 = (value) => value.field_name;
export const ValidationError$UnknownField$type_name = (value) =>
  value.type_name;
export const ValidationError$UnknownField$1 = (value) => value.type_name;

/**
 * Required argument is missing
 */
export class MissingRequiredArgument extends $CustomType {
  constructor(field_name, argument_name) {
    super();
    this.field_name = field_name;
    this.argument_name = argument_name;
  }
}
export const ValidationError$MissingRequiredArgument = (field_name, argument_name) =>
  new MissingRequiredArgument(field_name, argument_name);
export const ValidationError$isMissingRequiredArgument = (value) =>
  value instanceof MissingRequiredArgument;
export const ValidationError$MissingRequiredArgument$field_name = (value) =>
  value.field_name;
export const ValidationError$MissingRequiredArgument$0 = (value) =>
  value.field_name;
export const ValidationError$MissingRequiredArgument$argument_name = (value) =>
  value.argument_name;
export const ValidationError$MissingRequiredArgument$1 = (value) =>
  value.argument_name;

/**
 * Unknown argument provided
 */
export class UnknownArgument extends $CustomType {
  constructor(field_name, argument_name) {
    super();
    this.field_name = field_name;
    this.argument_name = argument_name;
  }
}
export const ValidationError$UnknownArgument = (field_name, argument_name) =>
  new UnknownArgument(field_name, argument_name);
export const ValidationError$isUnknownArgument = (value) =>
  value instanceof UnknownArgument;
export const ValidationError$UnknownArgument$field_name = (value) =>
  value.field_name;
export const ValidationError$UnknownArgument$0 = (value) => value.field_name;
export const ValidationError$UnknownArgument$argument_name = (value) =>
  value.argument_name;
export const ValidationError$UnknownArgument$1 = (value) => value.argument_name;

/**
 * Fragment is not defined
 */
export class UndefinedFragment extends $CustomType {
  constructor(fragment_name) {
    super();
    this.fragment_name = fragment_name;
  }
}
export const ValidationError$UndefinedFragment = (fragment_name) =>
  new UndefinedFragment(fragment_name);
export const ValidationError$isUndefinedFragment = (value) =>
  value instanceof UndefinedFragment;
export const ValidationError$UndefinedFragment$fragment_name = (value) =>
  value.fragment_name;
export const ValidationError$UndefinedFragment$0 = (value) =>
  value.fragment_name;

/**
 * Fragment type condition is invalid (type doesn't exist)
 */
export class InvalidTypeCondition extends $CustomType {
  constructor(fragment_name, type_name) {
    super();
    this.fragment_name = fragment_name;
    this.type_name = type_name;
  }
}
export const ValidationError$InvalidTypeCondition = (fragment_name, type_name) =>
  new InvalidTypeCondition(fragment_name, type_name);
export const ValidationError$isInvalidTypeCondition = (value) =>
  value instanceof InvalidTypeCondition;
export const ValidationError$InvalidTypeCondition$fragment_name = (value) =>
  value.fragment_name;
export const ValidationError$InvalidTypeCondition$0 = (value) =>
  value.fragment_name;
export const ValidationError$InvalidTypeCondition$type_name = (value) =>
  value.type_name;
export const ValidationError$InvalidTypeCondition$1 = (value) =>
  value.type_name;

/**
 * Variable is not defined
 */
export class UndefinedVariable extends $CustomType {
  constructor(variable_name) {
    super();
    this.variable_name = variable_name;
  }
}
export const ValidationError$UndefinedVariable = (variable_name) =>
  new UndefinedVariable(variable_name);
export const ValidationError$isUndefinedVariable = (value) =>
  value instanceof UndefinedVariable;
export const ValidationError$UndefinedVariable$variable_name = (value) =>
  value.variable_name;
export const ValidationError$UndefinedVariable$0 = (value) =>
  value.variable_name;

/**
 * Variable is defined but not used
 */
export class UnusedVariable extends $CustomType {
  constructor(variable_name) {
    super();
    this.variable_name = variable_name;
  }
}
export const ValidationError$UnusedVariable = (variable_name) =>
  new UnusedVariable(variable_name);
export const ValidationError$isUnusedVariable = (value) =>
  value instanceof UnusedVariable;
export const ValidationError$UnusedVariable$variable_name = (value) =>
  value.variable_name;
export const ValidationError$UnusedVariable$0 = (value) => value.variable_name;

/**
 * Duplicate operation name
 */
export class DuplicateOperationName extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const ValidationError$DuplicateOperationName = (name) =>
  new DuplicateOperationName(name);
export const ValidationError$isDuplicateOperationName = (value) =>
  value instanceof DuplicateOperationName;
export const ValidationError$DuplicateOperationName$name = (value) =>
  value.name;
export const ValidationError$DuplicateOperationName$0 = (value) => value.name;

/**
 * Duplicate fragment name
 */
export class DuplicateFragmentName extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const ValidationError$DuplicateFragmentName = (name) =>
  new DuplicateFragmentName(name);
export const ValidationError$isDuplicateFragmentName = (value) =>
  value instanceof DuplicateFragmentName;
export const ValidationError$DuplicateFragmentName$name = (value) => value.name;
export const ValidationError$DuplicateFragmentName$0 = (value) => value.name;

export class AnonymousOperationNotAlone extends $CustomType {}
export const ValidationError$AnonymousOperationNotAlone = () =>
  new AnonymousOperationNotAlone();
export const ValidationError$isAnonymousOperationNotAlone = (value) =>
  value instanceof AnonymousOperationNotAlone;

/**
 * Subscription has multiple root fields
 */
export class SubscriptionMultipleRootFields extends $CustomType {
  constructor(operation_name) {
    super();
    this.operation_name = operation_name;
  }
}
export const ValidationError$SubscriptionMultipleRootFields = (operation_name) =>
  new SubscriptionMultipleRootFields(operation_name);
export const ValidationError$isSubscriptionMultipleRootFields = (value) =>
  value instanceof SubscriptionMultipleRootFields;
export const ValidationError$SubscriptionMultipleRootFields$operation_name = (value) =>
  value.operation_name;
export const ValidationError$SubscriptionMultipleRootFields$0 = (value) =>
  value.operation_name;

/**
 * Circular fragment reference
 */
export class CircularFragmentReference extends $CustomType {
  constructor(fragment_name, path) {
    super();
    this.fragment_name = fragment_name;
    this.path = path;
  }
}
export const ValidationError$CircularFragmentReference = (fragment_name, path) =>
  new CircularFragmentReference(fragment_name, path);
export const ValidationError$isCircularFragmentReference = (value) =>
  value instanceof CircularFragmentReference;
export const ValidationError$CircularFragmentReference$fragment_name = (value) =>
  value.fragment_name;
export const ValidationError$CircularFragmentReference$0 = (value) =>
  value.fragment_name;
export const ValidationError$CircularFragmentReference$path = (value) =>
  value.path;
export const ValidationError$CircularFragmentReference$1 = (value) =>
  value.path;

/**
 * Directive not allowed in location
 */
export class DirectiveNotAllowed extends $CustomType {
  constructor(directive_name, location) {
    super();
    this.directive_name = directive_name;
    this.location = location;
  }
}
export const ValidationError$DirectiveNotAllowed = (directive_name, location) =>
  new DirectiveNotAllowed(directive_name, location);
export const ValidationError$isDirectiveNotAllowed = (value) =>
  value instanceof DirectiveNotAllowed;
export const ValidationError$DirectiveNotAllowed$directive_name = (value) =>
  value.directive_name;
export const ValidationError$DirectiveNotAllowed$0 = (value) =>
  value.directive_name;
export const ValidationError$DirectiveNotAllowed$location = (value) =>
  value.location;
export const ValidationError$DirectiveNotAllowed$1 = (value) => value.location;

/**
 * Unknown directive
 */
export class UnknownDirective extends $CustomType {
  constructor(directive_name) {
    super();
    this.directive_name = directive_name;
  }
}
export const ValidationError$UnknownDirective = (directive_name) =>
  new UnknownDirective(directive_name);
export const ValidationError$isUnknownDirective = (value) =>
  value instanceof UnknownDirective;
export const ValidationError$UnknownDirective$directive_name = (value) =>
  value.directive_name;
export const ValidationError$UnknownDirective$0 = (value) =>
  value.directive_name;

/**
 * Selection set required on non-leaf field
 */
export class SelectionSetRequired extends $CustomType {
  constructor(field_name, type_name) {
    super();
    this.field_name = field_name;
    this.type_name = type_name;
  }
}
export const ValidationError$SelectionSetRequired = (field_name, type_name) =>
  new SelectionSetRequired(field_name, type_name);
export const ValidationError$isSelectionSetRequired = (value) =>
  value instanceof SelectionSetRequired;
export const ValidationError$SelectionSetRequired$field_name = (value) =>
  value.field_name;
export const ValidationError$SelectionSetRequired$0 = (value) =>
  value.field_name;
export const ValidationError$SelectionSetRequired$type_name = (value) =>
  value.type_name;
export const ValidationError$SelectionSetRequired$1 = (value) =>
  value.type_name;

/**
 * Selection set not allowed on leaf field (scalar/enum)
 */
export class SelectionSetNotAllowed extends $CustomType {
  constructor(field_name, type_name) {
    super();
    this.field_name = field_name;
    this.type_name = type_name;
  }
}
export const ValidationError$SelectionSetNotAllowed = (field_name, type_name) =>
  new SelectionSetNotAllowed(field_name, type_name);
export const ValidationError$isSelectionSetNotAllowed = (value) =>
  value instanceof SelectionSetNotAllowed;
export const ValidationError$SelectionSetNotAllowed$field_name = (value) =>
  value.field_name;
export const ValidationError$SelectionSetNotAllowed$0 = (value) =>
  value.field_name;
export const ValidationError$SelectionSetNotAllowed$type_name = (value) =>
  value.type_name;
export const ValidationError$SelectionSetNotAllowed$1 = (value) =>
  value.type_name;

export class ValidationContext extends $CustomType {
  constructor(schema, fragments, defined_variables, used_variables, errors, current_type, fragment_spread_path) {
    super();
    this.schema = schema;
    this.fragments = fragments;
    this.defined_variables = defined_variables;
    this.used_variables = used_variables;
    this.errors = errors;
    this.current_type = current_type;
    this.fragment_spread_path = fragment_spread_path;
  }
}
export const ValidationContext$ValidationContext = (schema, fragments, defined_variables, used_variables, errors, current_type, fragment_spread_path) =>
  new ValidationContext(schema,
  fragments,
  defined_variables,
  used_variables,
  errors,
  current_type,
  fragment_spread_path);
export const ValidationContext$isValidationContext = (value) =>
  value instanceof ValidationContext;
export const ValidationContext$ValidationContext$schema = (value) =>
  value.schema;
export const ValidationContext$ValidationContext$0 = (value) => value.schema;
export const ValidationContext$ValidationContext$fragments = (value) =>
  value.fragments;
export const ValidationContext$ValidationContext$1 = (value) => value.fragments;
export const ValidationContext$ValidationContext$defined_variables = (value) =>
  value.defined_variables;
export const ValidationContext$ValidationContext$2 = (value) =>
  value.defined_variables;
export const ValidationContext$ValidationContext$used_variables = (value) =>
  value.used_variables;
export const ValidationContext$ValidationContext$3 = (value) =>
  value.used_variables;
export const ValidationContext$ValidationContext$errors = (value) =>
  value.errors;
export const ValidationContext$ValidationContext$4 = (value) => value.errors;
export const ValidationContext$ValidationContext$current_type = (value) =>
  value.current_type;
export const ValidationContext$ValidationContext$5 = (value) =>
  value.current_type;
export const ValidationContext$ValidationContext$fragment_spread_path = (value) =>
  value.fragment_spread_path;
export const ValidationContext$ValidationContext$6 = (value) =>
  value.fragment_spread_path;

function collect_fragments(document) {
  return $list.fold(
    document.definitions,
    $dict.new$(),
    (acc, def) => {
      if (def instanceof $ast.FragmentDefinition) {
        let fragment = def.fragment;
        return $dict.insert(acc, fragment.name, fragment);
      } else {
        return acc;
      }
    },
  );
}

function init_context(schema, document) {
  let fragments = collect_fragments(document);
  return new ValidationContext(
    schema,
    fragments,
    $set.new$(),
    $set.new$(),
    toList([]),
    schema.query,
    toList([]),
  );
}

function add_error(ctx, error) {
  return new ValidationContext(
    ctx.schema,
    ctx.fragments,
    ctx.defined_variables,
    ctx.used_variables,
    listPrepend(error, ctx.errors),
    ctx.current_type,
    ctx.fragment_spread_path,
  );
}

function set_current_type(ctx, type_opt) {
  return new ValidationContext(
    ctx.schema,
    ctx.fragments,
    ctx.defined_variables,
    ctx.used_variables,
    ctx.errors,
    type_opt,
    ctx.fragment_spread_path,
  );
}

function track_defined_variables(ctx, var_defs) {
  let defined = $list.fold(
    var_defs,
    $set.new$(),
    (acc, var_def) => { return $set.insert(acc, var_def.variable); },
  );
  return new ValidationContext(
    ctx.schema,
    ctx.fragments,
    defined,
    $set.new$(),
    ctx.errors,
    ctx.current_type,
    ctx.fragment_spread_path,
  );
}

function validate_unused_variables(ctx) {
  let unused = $set.difference(ctx.defined_variables, ctx.used_variables);
  return $set.fold(
    unused,
    ctx,
    (ctx, var_name) => { return add_error(ctx, new UnusedVariable(var_name)); },
  );
}

function validate_subscription_single_root(ctx, selection_set, op_name) {
  let root_fields = $list.filter(
    selection_set.selections,
    (sel) => {
      if (sel instanceof $ast.FieldSelection) {
        let field = sel.field;
        return field.name !== "__typename";
      } else {
        return true;
      }
    },
  );
  let $ = $list.length(root_fields) > 1;
  if ($) {
    return add_error(ctx, new SubscriptionMultipleRootFields(op_name));
  } else {
    return ctx;
  }
}

/**
 * Require a current type exists in context
 * 
 * @ignore
 */
function require_current_type(ctx, next) {
  let $ = ctx.current_type;
  if ($ instanceof Some) {
    let obj_type = $[0];
    return next(obj_type);
  } else {
    return ctx;
  }
}

/**
 * Require a field definition exists on the object type
 * 
 * @ignore
 */
function require_field_def(ctx, obj_type, field_name, next) {
  let $ = $dict.get(obj_type.fields, field_name);
  if ($ instanceof Ok) {
    let field_def = $[0];
    return next(field_def);
  } else {
    return add_error(ctx, new UnknownField(field_name, obj_type.name));
  }
}

function is_required_type(field_type) {
  if (field_type instanceof $schema.NonNull) {
    return true;
  } else {
    return false;
  }
}

function validate_field_arguments(ctx, field, field_def) {
  let provided_args = $list.fold(
    field.arguments,
    $dict.new$(),
    (acc, arg) => { return $dict.insert(acc, arg.name, true); },
  );
  let ctx$1 = $list.fold(
    field.arguments,
    ctx,
    (ctx, arg) => {
      let $ = $dict.has_key(field_def.arguments, arg.name);
      if ($) {
        return ctx;
      } else {
        return add_error(ctx, new UnknownArgument(field.name, arg.name));
      }
    },
  );
  return $dict.fold(
    field_def.arguments,
    ctx$1,
    (ctx, arg_name, arg_def) => {
      let $ = is_required_type(arg_def.arg_type);
      if ($) {
        let $1 = $dict.has_key(provided_args, arg_name);
        if ($1) {
          return ctx;
        } else {
          return add_error(
            ctx,
            new MissingRequiredArgument(field.name, arg_name),
          );
        }
      } else {
        return ctx;
      }
    },
  );
}

function get_base_type_name(loop$field_type) {
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
      return name;
    }
  }
}

function is_leaf_type(schema, type_name) {
  if (type_name === "String") {
    return true;
  } else if (type_name === "Int") {
    return true;
  } else if (type_name === "Float") {
    return true;
  } else if (type_name === "Boolean") {
    return true;
  } else if (type_name === "ID") {
    return true;
  } else {
    let $ = $dict.get(schema.types, type_name);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 instanceof $schema.ScalarTypeDef) {
        return true;
      } else if ($1 instanceof $schema.EnumTypeDef) {
        return true;
      } else {
        return false;
      }
    } else {
      return false;
    }
  }
}

function get_object_type(schema, type_name) {
  if (type_name === "Query") {
    return schema.query;
  } else if (type_name === "Mutation") {
    return schema.mutation;
  } else if (type_name === "Subscription") {
    return schema.subscription;
  } else {
    let $ = $dict.get(schema.types, type_name);
    if ($ instanceof Ok) {
      let $1 = $[0];
      if ($1 instanceof $schema.ObjectTypeDef) {
        let obj = $1.object_type;
        return new Some(obj);
      } else {
        return new None();
      }
    } else {
      return new None();
    }
  }
}

function track_value_variables(ctx, value) {
  if (value instanceof $ast.ListValue) {
    let values = value.values;
    return $list.fold(
      values,
      ctx,
      (ctx, v) => { return track_value_variables(ctx, v); },
    );
  } else if (value instanceof $ast.ObjectValue) {
    let fields = value.fields;
    return $list.fold(
      fields,
      ctx,
      (ctx, f) => { return track_value_variables(ctx, f.value); },
    );
  } else if (value instanceof $ast.VariableValue) {
    let name = value.name;
    let _block;
    let $ = $set.contains(ctx.defined_variables, name);
    if ($) {
      _block = new ValidationContext(
        ctx.schema,
        ctx.fragments,
        ctx.defined_variables,
        $set.insert(ctx.used_variables, name),
        ctx.errors,
        ctx.current_type,
        ctx.fragment_spread_path,
      );
    } else {
      _block = add_error(ctx, new UndefinedVariable(name));
    }
    let ctx$1 = _block;
    return ctx$1;
  } else {
    return ctx;
  }
}

function track_argument_variables(ctx, arguments$) {
  return $list.fold(
    arguments$,
    ctx,
    (ctx, arg) => { return track_value_variables(ctx, arg.value); },
  );
}

/**
 * Skip validation for introspection fields, just track variables
 * 
 * @ignore
 */
function skip_introspection_field(field_name, ctx, arguments$, next) {
  let $ = $string.starts_with(field_name, "__");
  if ($) {
    return track_argument_variables(ctx, arguments$);
  } else {
    return next(ctx);
  }
}

function validate_fragment_type_condition(ctx, fragment) {
  let type_name = fragment.type_condition;
  let type_exists = (((type_name === "Query") || (type_name === "Mutation")) || (type_name === "Subscription")) || $dict.has_key(
    ctx.schema.types,
    type_name,
  );
  if (type_exists) {
    return ctx;
  } else {
    return add_error(ctx, new InvalidTypeCondition(fragment.name, type_name));
  }
}

function validate_fragment_definitions(ctx, document) {
  return $list.fold(
    document.definitions,
    ctx,
    (ctx, def) => {
      if (def instanceof $ast.FragmentDefinition) {
        let fragment = def.fragment;
        return validate_fragment_type_condition(ctx, fragment);
      } else {
        return ctx;
      }
    },
  );
}

function get_fragment_spreads(selection_set) {
  return $list.flat_map(
    selection_set.selections,
    (sel) => {
      if (sel instanceof $ast.FieldSelection) {
        let field = sel.field;
        let $ = field.selection_set;
        if ($ instanceof Some) {
          let ss = $[0];
          return get_fragment_spreads(ss);
        } else {
          return toList([]);
        }
      } else if (sel instanceof $ast.FragmentSpread) {
        let spread = sel.fragment_spread;
        return toList([spread.name]);
      } else {
        let inline = sel.inline_fragment;
        return get_fragment_spreads(inline.selection_set);
      }
    },
  );
}

function check_fragment_cycle(ctx, fragment_name, path) {
  let $ = $list.contains(path, fragment_name);
  if ($) {
    return add_error(ctx, new CircularFragmentReference(fragment_name, path));
  } else {
    let $1 = $dict.get(ctx.fragments, fragment_name);
    if ($1 instanceof Ok) {
      let fragment = $1[0];
      let new_path = listPrepend(fragment_name, path);
      let spread_names = get_fragment_spreads(fragment.selection_set);
      return $list.fold(
        spread_names,
        ctx,
        (ctx, spread_name) => {
          return check_fragment_cycle(ctx, spread_name, new_path);
        },
      );
    } else {
      return ctx;
    }
  }
}

function validate_fragment_cycles(ctx, _) {
  return $dict.fold(
    ctx.fragments,
    ctx,
    (ctx, name, _) => { return check_fragment_cycle(ctx, name, toList([])); },
  );
}

function get_operations(document) {
  return $list.filter_map(
    document.definitions,
    (def) => {
      if (def instanceof $ast.OperationDefinition) {
        let operation = def.operation;
        return new Ok(operation);
      } else {
        return new Error(undefined);
      }
    },
  );
}

function validate_lone_anonymous_operation(ctx, document) {
  let operations = get_operations(document);
  let has_anonymous = $list.any(
    operations,
    (op) => {
      if (op instanceof $ast.Operation) {
        let $ = op.name;
        if ($ instanceof None) {
          return true;
        } else {
          return false;
        }
      } else {
        return true;
      }
    },
  );
  let $ = has_anonymous && ($list.length(operations) > 1);
  if ($) {
    return add_error(ctx, new AnonymousOperationNotAlone());
  } else {
    return ctx;
  }
}

function check_duplicates(ctx, names, error_fn) {
  let $ = $list.fold(
    names,
    [$set.new$(), ctx],
    (acc, name) => {
      let seen;
      let ctx$1;
      seen = acc[0];
      ctx$1 = acc[1];
      let $1 = $set.contains(seen, name);
      if ($1) {
        return [seen, add_error(ctx$1, error_fn(name))];
      } else {
        return [$set.insert(seen, name), ctx$1];
      }
    },
  );
  let ctx$1;
  ctx$1 = $[1];
  return ctx$1;
}

function validate_unique_operation_names(ctx, document) {
  let operations = get_operations(document);
  let names = $list.filter_map(
    operations,
    (op) => {
      if (op instanceof $ast.Operation) {
        let $ = op.name;
        if ($ instanceof Some) {
          let name = $[0];
          return new Ok(name);
        } else {
          return new Error(undefined);
        }
      } else {
        return new Error(undefined);
      }
    },
  );
  return check_duplicates(
    ctx,
    names,
    (var0) => { return new DuplicateOperationName(var0); },
  );
}

function validate_unique_fragment_names(ctx, document) {
  let names = $list.filter_map(
    document.definitions,
    (def) => {
      if (def instanceof $ast.FragmentDefinition) {
        let fragment = def.fragment;
        return new Ok(fragment.name);
      } else {
        return new Error(undefined);
      }
    },
  );
  return check_duplicates(
    ctx,
    names,
    (var0) => { return new DuplicateFragmentName(var0); },
  );
}

/**
 * Format a validation error as a human-readable string
 */
export function format_error(error) {
  if (error instanceof UnknownField) {
    let field_name = error.field_name;
    let type_name = error.type_name;
    return ((("Cannot query field \"" + field_name) + "\" on type \"") + type_name) + "\"";
  } else if (error instanceof MissingRequiredArgument) {
    let field_name = error.field_name;
    let arg_name = error.argument_name;
    return ((("Field \"" + field_name) + "\" argument \"") + arg_name) + "\" of type is required but not provided";
  } else if (error instanceof UnknownArgument) {
    let field_name = error.field_name;
    let arg_name = error.argument_name;
    return ((("Unknown argument \"" + arg_name) + "\" on field \"") + field_name) + "\"";
  } else if (error instanceof UndefinedFragment) {
    let name = error.fragment_name;
    return ("Unknown fragment \"" + name) + "\"";
  } else if (error instanceof InvalidTypeCondition) {
    let fragment_name = error.fragment_name;
    let type_name = error.type_name;
    return ((("Fragment \"" + fragment_name) + "\" cannot condition on non-existent type \"") + type_name) + "\"";
  } else if (error instanceof UndefinedVariable) {
    let name = error.variable_name;
    return ("Variable \"$" + name) + "\" is not defined";
  } else if (error instanceof UnusedVariable) {
    let name = error.variable_name;
    return ("Variable \"$" + name) + "\" is never used";
  } else if (error instanceof DuplicateOperationName) {
    let name = error.name;
    return ("There can be only one operation named \"" + name) + "\"";
  } else if (error instanceof DuplicateFragmentName) {
    let name = error.name;
    return ("There can be only one fragment named \"" + name) + "\"";
  } else if (error instanceof AnonymousOperationNotAlone) {
    return "This anonymous operation must be the only defined operation";
  } else if (error instanceof SubscriptionMultipleRootFields) {
    let name = error.operation_name;
    return ("Subscription " + (() => {
      if (name instanceof Some) {
        let n = name[0];
        return ("\"" + n) + "\"";
      } else {
        return "";
      }
    })()) + " must select only one top level field";
  } else if (error instanceof CircularFragmentReference) {
    let name = error.fragment_name;
    return ("Cannot spread fragment \"" + name) + "\" within itself";
  } else if (error instanceof DirectiveNotAllowed) {
    let directive_name = error.directive_name;
    let location = error.location;
    return (("Directive \"@" + directive_name) + "\" may not be used on ") + location;
  } else if (error instanceof UnknownDirective) {
    let name = error.directive_name;
    return ("Unknown directive \"@" + name) + "\"";
  } else if (error instanceof SelectionSetRequired) {
    let field_name = error.field_name;
    let type_name = error.type_name;
    return ((("Field \"" + field_name) + "\" of type \"") + type_name) + "\" must have a selection of subfields";
  } else {
    let field_name = error.field_name;
    let type_name = error.type_name;
    return ((("Field \"" + field_name) + "\" must not have a selection since type \"") + type_name) + "\" has no subfields";
  }
}

/**
 * Format all validation errors as a single string
 */
export function format_errors(errors) {
  let _pipe = errors;
  let _pipe$1 = $list.map(_pipe, format_error);
  return $string.join(_pipe$1, "\n");
}

function validate_field_selection_set(ctx, field, field_def) {
  let inner_type_name = get_base_type_name(field_def.field_type);
  let is_leaf = is_leaf_type(ctx.schema, inner_type_name);
  let $ = field.selection_set;
  if (is_leaf) {
    if ($ instanceof Some) {
      return add_error(
        ctx,
        new SelectionSetNotAllowed(field.name, inner_type_name),
      );
    } else {
      return ctx;
    }
  } else if ($ instanceof Some) {
    let ss = $[0];
    let inner_type = get_object_type(ctx.schema, inner_type_name);
    let _pipe = ctx;
    let _pipe$1 = set_current_type(_pipe, inner_type);
    return validate_selection_set(_pipe$1, ss);
  } else {
    return add_error(ctx, new SelectionSetRequired(field.name, inner_type_name));
  }
}

function validate_selection_set(ctx, selection_set) {
  return $list.fold(
    selection_set.selections,
    ctx,
    (ctx, selection) => { return validate_selection(ctx, selection); },
  );
}

function validate_selection(ctx, selection) {
  if (selection instanceof $ast.FieldSelection) {
    let field = selection.field;
    return validate_field(ctx, field);
  } else if (selection instanceof $ast.FragmentSpread) {
    let spread = selection.fragment_spread;
    return validate_fragment_spread(ctx, spread);
  } else {
    let inline = selection.inline_fragment;
    return validate_inline_fragment(ctx, inline);
  }
}

function validate_field(ctx, field) {
  return skip_introspection_field(
    field.name,
    ctx,
    field.arguments,
    (ctx) => {
      return require_current_type(
        ctx,
        (obj_type) => {
          return require_field_def(
            ctx,
            obj_type,
            field.name,
            (field_def) => {
              let _pipe = ctx;
              let _pipe$1 = validate_field_arguments(_pipe, field, field_def);
              let _pipe$2 = track_argument_variables(_pipe$1, field.arguments);
              return validate_field_selection_set(_pipe$2, field, field_def);
            },
          );
        },
      );
    },
  );
}

function validate_operation(ctx, operation) {
  if (operation instanceof $ast.Operation) {
    let op_type = operation.operation_type;
    let op_name = operation.name;
    let var_defs = operation.variable_definitions;
    let selection_set = operation.selection_set;
    let _block;
    if (op_type instanceof $ast.Query) {
      _block = ctx.schema.query;
    } else if (op_type instanceof $ast.Mutation) {
      _block = ctx.schema.mutation;
    } else {
      _block = ctx.schema.subscription;
    }
    let root_type = _block;
    let ctx$1 = set_current_type(ctx, root_type);
    let ctx$2 = track_defined_variables(ctx$1, var_defs);
    let _block$1;
    if (op_type instanceof $ast.Subscription) {
      _block$1 = validate_subscription_single_root(
        ctx$2,
        selection_set,
        op_name,
      );
    } else {
      _block$1 = ctx$2;
    }
    let ctx$3 = _block$1;
    let ctx$4 = validate_selection_set(ctx$3, selection_set);
    return validate_unused_variables(ctx$4);
  } else {
    let selection_set = operation.selection_set;
    let ctx$1 = set_current_type(ctx, ctx.schema.query);
    return validate_selection_set(ctx$1, selection_set);
  }
}

function validate_document_operations(ctx, document) {
  return $list.fold(
    document.definitions,
    ctx,
    (ctx, def) => {
      if (def instanceof $ast.OperationDefinition) {
        let operation = def.operation;
        return validate_operation(ctx, operation);
      } else {
        return ctx;
      }
    },
  );
}

/**
 * Validate a document against a schema
 */
export function validate(document, schema) {
  let ctx = init_context(schema, document);
  let ctx$1 = validate_unique_operation_names(ctx, document);
  let ctx$2 = validate_lone_anonymous_operation(ctx$1, document);
  let ctx$3 = validate_unique_fragment_names(ctx$2, document);
  let ctx$4 = validate_document_operations(ctx$3, document);
  let ctx$5 = validate_fragment_definitions(ctx$4, document);
  let ctx$6 = validate_fragment_cycles(ctx$5, document);
  let $ = ctx$6.errors;
  if ($ instanceof $Empty) {
    return new Ok(document);
  } else {
    let errors = $;
    return new Error($list.reverse(errors));
  }
}

/**
 * Validate a single query string against a schema (convenience function)
 */
export function validate_query(query, schema) {
  let $ = $parser.parse(query);
  if ($ instanceof Ok) {
    let document = $[0];
    return validate(document, schema);
  } else {
    return new Error(toList([]));
  }
}

function validate_fragment_spread(ctx, spread) {
  let $ = $dict.get(ctx.fragments, spread.name);
  if ($ instanceof Ok) {
    let fragment = $[0];
    let $1 = $list.contains(ctx.fragment_spread_path, spread.name);
    if ($1) {
      return ctx;
    } else {
      let fragment_type = get_object_type(ctx.schema, fragment.type_condition);
      let ctx$1 = set_current_type(ctx, fragment_type);
      let ctx$2 = new ValidationContext(
        ctx$1.schema,
        ctx$1.fragments,
        ctx$1.defined_variables,
        ctx$1.used_variables,
        ctx$1.errors,
        ctx$1.current_type,
        listPrepend(spread.name, ctx$1.fragment_spread_path),
      );
      let ctx$3 = validate_selection_set(ctx$2, fragment.selection_set);
      return new ValidationContext(
        ctx$3.schema,
        ctx$3.fragments,
        ctx$3.defined_variables,
        ctx$3.used_variables,
        ctx$3.errors,
        ctx$3.current_type,
        $list.drop(ctx$3.fragment_spread_path, 1),
      );
    }
  } else {
    return add_error(ctx, new UndefinedFragment(spread.name));
  }
}

function validate_inline_fragment(ctx, inline) {
  let $ = inline.type_condition;
  if ($ instanceof Some) {
    let type_name = $[0];
    let inner_type = get_object_type(ctx.schema, type_name);
    let ctx$1 = set_current_type(ctx, inner_type);
    return validate_selection_set(ctx$1, inline.selection_set);
  } else {
    return validate_selection_set(ctx, inline.selection_set);
  }
}
