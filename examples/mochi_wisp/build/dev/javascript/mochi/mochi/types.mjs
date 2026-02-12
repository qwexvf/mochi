import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import { Ok, Error, toList, prepend as listPrepend, CustomType as $CustomType } from "../gleam.mjs";
import * as $schema from "../mochi/schema.mjs";
import { identity as to_dynamic } from "../mochi_ffi.mjs";

export { to_dynamic };

export class TypeBuilder extends $CustomType {
  constructor(name, description, fields) {
    super();
    this.name = name;
    this.description = description;
    this.fields = fields;
  }
}
export const TypeBuilder$TypeBuilder = (name, description, fields) =>
  new TypeBuilder(name, description, fields);
export const TypeBuilder$isTypeBuilder = (value) =>
  value instanceof TypeBuilder;
export const TypeBuilder$TypeBuilder$name = (value) => value.name;
export const TypeBuilder$TypeBuilder$0 = (value) => value.name;
export const TypeBuilder$TypeBuilder$description = (value) => value.description;
export const TypeBuilder$TypeBuilder$1 = (value) => value.description;
export const TypeBuilder$TypeBuilder$fields = (value) => value.fields;
export const TypeBuilder$TypeBuilder$2 = (value) => value.fields;

export class TypeField extends $CustomType {
  constructor(name, description, field_type, extractor) {
    super();
    this.name = name;
    this.description = description;
    this.field_type = field_type;
    this.extractor = extractor;
  }
}
export const TypeField$TypeField = (name, description, field_type, extractor) =>
  new TypeField(name, description, field_type, extractor);
export const TypeField$isTypeField = (value) => value instanceof TypeField;
export const TypeField$TypeField$name = (value) => value.name;
export const TypeField$TypeField$0 = (value) => value.name;
export const TypeField$TypeField$description = (value) => value.description;
export const TypeField$TypeField$1 = (value) => value.description;
export const TypeField$TypeField$field_type = (value) => value.field_type;
export const TypeField$TypeField$2 = (value) => value.field_type;
export const TypeField$TypeField$extractor = (value) => value.extractor;
export const TypeField$TypeField$3 = (value) => value.extractor;

export class EnumBuilder extends $CustomType {
  constructor(name, description, values) {
    super();
    this.name = name;
    this.description = description;
    this.values = values;
  }
}
export const EnumBuilder$EnumBuilder = (name, description, values) =>
  new EnumBuilder(name, description, values);
export const EnumBuilder$isEnumBuilder = (value) =>
  value instanceof EnumBuilder;
export const EnumBuilder$EnumBuilder$name = (value) => value.name;
export const EnumBuilder$EnumBuilder$0 = (value) => value.name;
export const EnumBuilder$EnumBuilder$description = (value) => value.description;
export const EnumBuilder$EnumBuilder$1 = (value) => value.description;
export const EnumBuilder$EnumBuilder$values = (value) => value.values;
export const EnumBuilder$EnumBuilder$2 = (value) => value.values;

export class EnumValue extends $CustomType {
  constructor(name, description, is_deprecated, deprecation_reason) {
    super();
    this.name = name;
    this.description = description;
    this.is_deprecated = is_deprecated;
    this.deprecation_reason = deprecation_reason;
  }
}
export const EnumValue$EnumValue = (name, description, is_deprecated, deprecation_reason) =>
  new EnumValue(name, description, is_deprecated, deprecation_reason);
export const EnumValue$isEnumValue = (value) => value instanceof EnumValue;
export const EnumValue$EnumValue$name = (value) => value.name;
export const EnumValue$EnumValue$0 = (value) => value.name;
export const EnumValue$EnumValue$description = (value) => value.description;
export const EnumValue$EnumValue$1 = (value) => value.description;
export const EnumValue$EnumValue$is_deprecated = (value) => value.is_deprecated;
export const EnumValue$EnumValue$2 = (value) => value.is_deprecated;
export const EnumValue$EnumValue$deprecation_reason = (value) =>
  value.deprecation_reason;
export const EnumValue$EnumValue$3 = (value) => value.deprecation_reason;

/**
 * Create a new type builder
 */
export function object(name) {
  return new TypeBuilder(name, new None(), toList([]));
}

/**
 * Add description to type
 */
export function description(builder, desc) {
  return new TypeBuilder(builder.name, new Some(desc), builder.fields);
}

/**
 * Add a string field
 */
export function string(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.string_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a string field with description
 */
export function string_with_desc(builder, name, desc, extractor) {
  let field = new TypeField(
    name,
    new Some(desc),
    $schema.string_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add an ID field
 */
export function id(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.non_null($schema.id_type()),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add an int field
 */
export function int(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.int_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add an int field with description
 */
export function int_with_desc(builder, name, desc, extractor) {
  let field = new TypeField(
    name,
    new Some(desc),
    $schema.int_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a float field
 */
export function float(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.float_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a boolean field
 */
export function bool(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.boolean_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add an optional string field
 */
export function optional_string(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.string_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add an optional int field
 */
export function optional_int(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.int_type(),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a list of strings field
 */
export function list_string(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.list_type($schema.string_type()),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a list of ints field
 */
export function list_int(builder, name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.list_type($schema.int_type()),
    (a) => { return to_dynamic(extractor(a)); },
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a related object field
 */
export function object_field(builder, name, type_name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.named_type(type_name),
    extractor,
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a list of related objects field
 */
export function list_object(builder, name, type_name, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.list_type($schema.named_type(type_name)),
    extractor,
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

/**
 * Add a non-null field
 */
export function non_null_field(builder, name, field_type, extractor) {
  let field = new TypeField(
    name,
    new None(),
    $schema.non_null(field_type),
    extractor,
  );
  return new TypeBuilder(
    builder.name,
    builder.description,
    listPrepend(field, builder.fields),
  );
}

function to_field_def(f, decoder) {
  let resolver = (info) => {
    let $ = info.parent;
    if ($ instanceof Some) {
      let parent_dyn = $[0];
      let $1 = decoder(parent_dyn);
      if ($1 instanceof Ok) {
        let parent = $1[0];
        return new Ok(f.extractor(parent));
      } else {
        return $1;
      }
    } else {
      return new Error("No parent value");
    }
  };
  let _block;
  let _pipe = $schema.field_def(f.name, f.field_type);
  _block = $schema.resolver(_pipe, resolver);
  let base = _block;
  let $ = f.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return $schema.field_description(base, desc);
  } else {
    return base;
  }
}

/**
 * Build the TypeBuilder into an ObjectType with a decoder
 */
export function build(builder, decoder) {
  let schema_fields = $list.map(
    $list.reverse(builder.fields),
    (f) => { return to_field_def(f, decoder); },
  );
  let base_obj = $schema.object(builder.name);
  let _block;
  let $ = builder.description;
  if ($ instanceof Some) {
    let d = $[0];
    _block = $schema.description(base_obj, d);
  } else {
    _block = base_obj;
  }
  let with_desc = _block;
  return $list.fold(
    schema_fields,
    with_desc,
    (obj, field) => { return $schema.field(obj, field); },
  );
}

/**
 * Create a new enum builder
 */
export function enum_type(name) {
  return new EnumBuilder(name, new None(), toList([]));
}

/**
 * Add description to enum
 */
export function enum_description(builder, desc) {
  return new EnumBuilder(builder.name, new Some(desc), builder.values);
}

/**
 * Add an enum value
 */
export function value(builder, name) {
  return new EnumBuilder(
    builder.name,
    builder.description,
    listPrepend(
      new EnumValue(name, new None(), false, new None()),
      builder.values,
    ),
  );
}

/**
 * Add an enum value with description
 */
export function value_with_desc(builder, name, desc) {
  return new EnumBuilder(
    builder.name,
    builder.description,
    listPrepend(
      new EnumValue(name, new Some(desc), false, new None()),
      builder.values,
    ),
  );
}

/**
 * Add a deprecated enum value
 */
export function deprecated_value(builder, name) {
  return new EnumBuilder(
    builder.name,
    builder.description,
    listPrepend(
      new EnumValue(name, new None(), true, new None()),
      builder.values,
    ),
  );
}

/**
 * Add a deprecated enum value with reason
 */
export function deprecated_value_with_reason(builder, name, reason) {
  return new EnumBuilder(
    builder.name,
    builder.description,
    listPrepend(
      new EnumValue(name, new None(), true, new Some(reason)),
      builder.values,
    ),
  );
}

/**
 * Build the enum type
 */
export function build_enum(builder) {
  let enum_values = $list.fold(
    $list.reverse(builder.values),
    $dict.new$(),
    (acc, v) => {
      let value_def = new $schema.EnumValueDefinition(
        v.name,
        v.description,
        to_dynamic(v.name),
        v.is_deprecated,
        v.deprecation_reason,
      );
      return $dict.insert(acc, v.name, value_def);
    },
  );
  return new $schema.EnumType(builder.name, builder.description, enum_values);
}
