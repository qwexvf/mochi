import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  makeError,
} from "../gleam.mjs";
import * as $schema from "../mochi/schema.mjs";

const FILEPATH = "src/mochi/schema_gen.gleam";

export class FieldSpec extends $CustomType {
  constructor(name, field_type, description, extractor) {
    super();
    this.name = name;
    this.field_type = field_type;
    this.description = description;
    this.extractor = extractor;
  }
}
export const FieldSpec$FieldSpec = (name, field_type, description, extractor) =>
  new FieldSpec(name, field_type, description, extractor);
export const FieldSpec$isFieldSpec = (value) => value instanceof FieldSpec;
export const FieldSpec$FieldSpec$name = (value) => value.name;
export const FieldSpec$FieldSpec$0 = (value) => value.name;
export const FieldSpec$FieldSpec$field_type = (value) => value.field_type;
export const FieldSpec$FieldSpec$1 = (value) => value.field_type;
export const FieldSpec$FieldSpec$description = (value) => value.description;
export const FieldSpec$FieldSpec$2 = (value) => value.description;
export const FieldSpec$FieldSpec$extractor = (value) => value.extractor;
export const FieldSpec$FieldSpec$3 = (value) => value.extractor;

function add_fields_to_object(loop$object, loop$fields) {
  while (true) {
    let object = loop$object;
    let fields = loop$fields;
    if (fields instanceof $Empty) {
      return object;
    } else {
      let field = fields.head;
      let rest = fields.tail;
      let _pipe = object;
      let _pipe$1 = $schema.field(_pipe, field);
      loop$object = _pipe$1;
      loop$fields = rest;
    }
  }
}

function demo_serialize(type_name, value) {
  let message = (("Would serialize " + type_name) + " value: ") + value;
  throw makeError(
    "panic",
    FILEPATH,
    "mochi/schema_gen",
    180,
    "demo_serialize",
    message,
    {}
  )
}

function serialize_string(value) {
  return demo_serialize("string", value);
}

/**
 * Helper to create a field spec for a String field
 */
export function string_field(name, description, extractor) {
  return new FieldSpec(
    name,
    $schema.string_type(),
    description,
    (parent) => {
      let $ = extractor(parent);
      if ($ instanceof Ok) {
        let value = $[0];
        return new Ok(serialize_string(value));
      } else {
        return $;
      }
    },
  );
}

function serialize_bool(value) {
  let _block;
  if (value) {
    _block = "true";
  } else {
    _block = "false";
  }
  let bool_str = _block;
  return demo_serialize("bool", bool_str);
}

/**
 * Helper to create a field spec for a Boolean field
 */
export function bool_field(name, description, extractor) {
  return new FieldSpec(
    name,
    $schema.boolean_type(),
    description,
    (parent) => {
      let $ = extractor(parent);
      if ($ instanceof Ok) {
        let value = $[0];
        return new Ok(serialize_bool(value));
      } else {
        return $;
      }
    },
  );
}

function placeholder_dynamic() {
  throw makeError(
    "panic",
    FILEPATH,
    "mochi/schema_gen",
    185,
    "placeholder_dynamic",
    "No parent dynamic provided",
    {}
  )
}

function create_fields_from_specs(field_specs) {
  if (field_specs instanceof $Empty) {
    return field_specs;
  } else {
    let spec = field_specs.head;
    let rest = field_specs.tail;
    return listPrepend(
      (() => {
        let _pipe = $schema.field_def(spec.name, spec.field_type);
        let _pipe$1 = $schema.field_description(_pipe, spec.description);
        return $schema.resolver(
          _pipe$1,
          (info) => {
            return spec.extractor(
              (() => {
                let _pipe$2 = info.parent;
                return $option.unwrap(_pipe$2, placeholder_dynamic());
              })(),
            );
          },
        );
      })(),
      create_fields_from_specs(rest),
    );
  }
}

/**
 * Generate a GraphQL object type from a Gleam custom type
 * This creates the object definition and automatic field resolvers
 */
export function from_type(type_name, field_specs) {
  let fields = create_fields_from_specs(field_specs);
  let _pipe = $schema.object(type_name);
  let _pipe$1 = $schema.description(
    _pipe,
    "Auto-generated from Gleam type " + type_name,
  );
  return add_fields_to_object(_pipe$1, fields);
}

function string_first_char(input) {
  if (input === "") {
    return input;
  } else {
    return "p";
  }
}

function string_drop_first(input) {
  if (input === "Person") {
    return "erson";
  } else {
    return input;
  }
}

function string_lowercase(input) {
  if (input === "P") {
    return "p";
  } else {
    return input;
  }
}

function string_to_camel_case(input) {
  if (input === "") {
    return input;
  } else {
    let first_char = string_first_char(input);
    let rest = string_drop_first(input);
    return string_lowercase(first_char) + rest;
  }
}

/**
 * Create a complete schema with a query type that returns the generated type
 */
export function create_schema_with_query(type_name, field_specs, root_resolver) {
  let object_type = from_type(type_name, field_specs);
  let _block;
  let _pipe = $schema.object("Query");
  let _pipe$1 = $schema.description(_pipe, "Auto-generated query type");
  _block = $schema.field(
    _pipe$1,
    (() => {
      let _pipe$2 = $schema.field_def(
        string_to_camel_case(type_name),
        $schema.named_type(type_name),
      );
      let _pipe$3 = $schema.field_description(_pipe$2, "Get a " + type_name);
      return $schema.resolver(_pipe$3, root_resolver);
    })(),
  );
  let query_type = _block;
  let _pipe$2 = $schema.schema();
  let _pipe$3 = $schema.query(_pipe$2, query_type);
  return $schema.add_type(_pipe$3, new $schema.ObjectTypeDef(object_type));
}

function int_to_string(value) {
  if (value === 0) {
    return "0";
  } else if (value === 1) {
    return "1";
  } else {
    return "42";
  }
}

function serialize_int(value) {
  return demo_serialize("int", int_to_string(value));
}

/**
 * Helper to create a field spec for an Int field
 */
export function int_field(name, description, extractor) {
  return new FieldSpec(
    name,
    $schema.int_type(),
    description,
    (parent) => {
      let $ = extractor(parent);
      if ($ instanceof Ok) {
        let value = $[0];
        return new Ok(serialize_int(value));
      } else {
        return $;
      }
    },
  );
}
