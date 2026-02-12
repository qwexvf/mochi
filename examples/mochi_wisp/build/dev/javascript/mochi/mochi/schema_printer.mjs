import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $io from "../../gleam_stdlib/gleam/io.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import { toList, Empty as $Empty, prepend as listPrepend } from "../gleam.mjs";
import * as $schema from "../mochi/schema.mjs";

/**
 * Print field type (handles NonNull, List, Named)
 * 
 * @ignore
 */
function print_field_type(field_type) {
  if (field_type instanceof $schema.NonNull) {
    let inner = field_type.inner;
    return print_field_type(inner) + "!";
  } else if (field_type instanceof $schema.List) {
    let inner = field_type.inner;
    return ("[" + print_field_type(inner)) + "]";
  } else {
    let name = field_type.name;
    return name;
  }
}

/**
 * Print argument definition
 * 
 * @ignore
 */
function print_argument_definition(arg) {
  let base = (arg.name + ": ") + print_field_type(arg.arg_type);
  let $ = arg.default_value;
  if ($ instanceof Some) {
    return (base + " = ") + "defaultValue";
  } else {
    return base;
  }
}

/**
 * Print schema definition if it has custom root types
 * 
 * @ignore
 */
function print_schema_definition(schema_def) {
  let parts = toList([]);
  let _block;
  let $ = schema_def.query;
  if ($ instanceof Some) {
    let query = $[0];
    if (query.name !== "Query") {
      _block = listPrepend("query: " + query.name, parts);
    } else {
      _block = parts;
    }
  } else {
    _block = parts;
  }
  let parts$1 = _block;
  let _block$1;
  let $1 = schema_def.mutation;
  if ($1 instanceof Some) {
    let mutation = $1[0];
    if (mutation.name !== "Mutation") {
      _block$1 = listPrepend("mutation: " + mutation.name, parts$1);
    } else {
      _block$1 = parts$1;
    }
  } else {
    _block$1 = parts$1;
  }
  let parts$2 = _block$1;
  let _block$2;
  let $2 = schema_def.subscription;
  if ($2 instanceof Some) {
    let subscription = $2[0];
    if (subscription.name !== "Subscription") {
      _block$2 = listPrepend("subscription: " + subscription.name, parts$2);
    } else {
      _block$2 = parts$2;
    }
  } else {
    _block$2 = parts$2;
  }
  let parts$3 = _block$2;
  if (parts$3 instanceof $Empty) {
    return "";
  } else {
    return ("schema {\n  " + $string.join($list.reverse(parts$3), "\n  ")) + "\n}";
  }
}

/**
 * Check if schema has custom root type names
 * 
 * @ignore
 */
function has_custom_root_types(schema_def) {
  let _block;
  let $ = schema_def.query;
  if ($ instanceof Some) {
    let query = $[0];
    _block = query.name !== "Query";
  } else {
    _block = false;
  }
  let query_custom = _block;
  let _block$1;
  let $1 = schema_def.mutation;
  if ($1 instanceof Some) {
    let mutation = $1[0];
    _block$1 = mutation.name !== "Mutation";
  } else {
    _block$1 = false;
  }
  let mutation_custom = _block$1;
  let _block$2;
  let $2 = schema_def.subscription;
  if ($2 instanceof Some) {
    let subscription = $2[0];
    _block$2 = subscription.name !== "Subscription";
  } else {
    _block$2 = false;
  }
  let subscription_custom = _block$2;
  return (query_custom || mutation_custom) || subscription_custom;
}

/**
 * Print description as a comment
 * 
 * @ignore
 */
function print_description(desc) {
  return ("\"\"\"" + desc) + "\"\"\"";
}

/**
 * Print scalar type: scalar DateTime
 * 
 * @ignore
 */
function print_scalar_type(scalar) {
  let result = "scalar " + scalar.name;
  let $ = scalar.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return (print_description(desc) + "\n") + result;
  } else {
    return result;
  }
}

/**
 * Print union type: union SearchResult = User | Post
 * 
 * @ignore
 */
function print_union_type(union) {
  let header = "union " + union.name;
  let _block;
  let $ = union.description;
  if ($ instanceof Some) {
    let desc = $[0];
    _block = (print_description(desc) + "\n") + header;
  } else {
    _block = header;
  }
  let header$1 = _block;
  let _block$1;
  let _pipe = $list.map(union.types, (t) => { return t.name; });
  _block$1 = $string.join(_pipe, " | ");
  let members = _block$1;
  if (members === "") {
    return header$1;
  } else {
    return (header$1 + " = ") + members;
  }
}

/**
 * Print description inline
 * 
 * @ignore
 */
function print_description_inline(desc) {
  return "# " + desc;
}

/**
 * Print field definition within a type
 * 
 * @ignore
 */
function print_field_definition(field, indent) {
  let base = indent + field.name;
  let _block;
  let $ = $dict.size(field.arguments);
  if ($ === 0) {
    _block = base;
  } else {
    let _block$1;
    let _pipe = $dict.to_list(field.arguments);
    let _pipe$1 = $list.map(
      _pipe,
      (entry) => { return print_argument_definition(entry[1]); },
    );
    _block$1 = $string.join(_pipe$1, ", ");
    let args = _block$1;
    _block = ((base + "(") + args) + ")";
  }
  let base$1 = _block;
  let base$2 = (base$1 + ": ") + print_field_type(field.field_type);
  let $1 = field.description;
  if ($1 instanceof Some) {
    let desc = $1[0];
    return (print_description_inline(desc) + "\n") + base$2;
  } else {
    return base$2;
  }
}

/**
 * Print object type: type User { ... }
 * 
 * @ignore
 */
function print_object_type(obj) {
  let header = "type " + obj.name;
  let _block;
  let $ = obj.description;
  if ($ instanceof Some) {
    let desc = $[0];
    _block = (print_description(desc) + "\n") + header;
  } else {
    _block = header;
  }
  let header$1 = _block;
  let _block$1;
  let $1 = $list.length(obj.interfaces);
  if ($1 === 0) {
    _block$1 = header$1;
  } else {
    _block$1 = (header$1 + " implements ") + $string.join(
      $list.map(obj.interfaces, (i) => { return i.name; }),
      " & ",
    );
  }
  let header$2 = _block$1;
  let _block$2;
  let _pipe = $dict.to_list(obj.fields);
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => { return print_field_definition(entry[1], "  "); },
  );
  _block$2 = $string.join(_pipe$1, "\n");
  let fields = _block$2;
  if (fields === "") {
    return header$2;
  } else {
    return ((header$2 + " {\n") + fields) + "\n}";
  }
}

/**
 * Print interface type: interface Node { ... }
 * 
 * @ignore
 */
function print_interface_type(interface$) {
  let header = "interface " + interface$.name;
  let _block;
  let $ = interface$.description;
  if ($ instanceof Some) {
    let desc = $[0];
    _block = (print_description(desc) + "\n") + header;
  } else {
    _block = header;
  }
  let header$1 = _block;
  let _block$1;
  let _pipe = $dict.to_list(interface$.fields);
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => { return print_field_definition(entry[1], "  "); },
  );
  _block$1 = $string.join(_pipe$1, "\n");
  let fields = _block$1;
  if (fields === "") {
    return header$1;
  } else {
    return ((header$1 + " {\n") + fields) + "\n}";
  }
}

/**
 * Print input field definition
 * 
 * @ignore
 */
function print_input_field_definition(field, indent) {
  let base = ((indent + field.name) + ": ") + print_field_type(field.field_type);
  let _block;
  let $ = field.default_value;
  if ($ instanceof Some) {
    _block = (base + " = ") + "defaultValue";
  } else {
    _block = base;
  }
  let base$1 = _block;
  let $1 = field.description;
  if ($1 instanceof Some) {
    let desc = $1[0];
    return (print_description_inline(desc) + "\n") + base$1;
  } else {
    return base$1;
  }
}

/**
 * Print input object type: input CreateUserInput { ... }
 * 
 * @ignore
 */
function print_input_object_type(input) {
  let header = "input " + input.name;
  let _block;
  let $ = input.description;
  if ($ instanceof Some) {
    let desc = $[0];
    _block = (print_description(desc) + "\n") + header;
  } else {
    _block = header;
  }
  let header$1 = _block;
  let _block$1;
  let _pipe = $dict.to_list(input.fields);
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => { return print_input_field_definition(entry[1], "  "); },
  );
  _block$1 = $string.join(_pipe$1, "\n");
  let fields = _block$1;
  if (fields === "") {
    return header$1;
  } else {
    return ((header$1 + " {\n") + fields) + "\n}";
  }
}

/**
 * Print enum value
 * 
 * @ignore
 */
function print_enum_value(enum_value, indent) {
  let base = indent + enum_value.name;
  let $ = enum_value.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return (print_description_inline(desc) + "\n") + base;
  } else {
    return base;
  }
}

/**
 * Print enum type: enum Status { ACTIVE INACTIVE }
 * 
 * @ignore
 */
function print_enum_type(enum$) {
  let header = "enum " + enum$.name;
  let _block;
  let $ = enum$.description;
  if ($ instanceof Some) {
    let desc = $[0];
    _block = (print_description(desc) + "\n") + header;
  } else {
    _block = header;
  }
  let header$1 = _block;
  let _block$1;
  let _pipe = $dict.to_list(enum$.values);
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => { return print_enum_value(entry[1], "  "); },
  );
  _block$1 = $string.join(_pipe$1, "\n");
  let values = _block$1;
  if (values === "") {
    return header$1;
  } else {
    return ((header$1 + " {\n") + values) + "\n}";
  }
}

/**
 * Print individual type definition
 */
export function print_type_definition(type_def) {
  if (type_def instanceof $schema.ObjectTypeDef) {
    let obj = type_def.object_type;
    return print_object_type(obj);
  } else if (type_def instanceof $schema.ScalarTypeDef) {
    let scalar = type_def.scalar_type;
    return print_scalar_type(scalar);
  } else if (type_def instanceof $schema.EnumTypeDef) {
    let enum$ = type_def.enum_type;
    return print_enum_type(enum$);
  } else if (type_def instanceof $schema.InterfaceTypeDef) {
    let interface$ = type_def.interface_type;
    return print_interface_type(interface$);
  } else if (type_def instanceof $schema.UnionTypeDef) {
    let union = type_def.union_type;
    return print_union_type(union);
  } else {
    let input = type_def.input_object_type;
    return print_input_object_type(input);
  }
}

/**
 * Print a schema to SDL format
 */
export function print_schema(schema_def) {
  let parts = toList([]);
  let _block;
  let $ = has_custom_root_types(schema_def);
  if ($) {
    _block = listPrepend(print_schema_definition(schema_def), parts);
  } else {
    _block = parts;
  }
  let parts$1 = _block;
  let _block$1;
  let _pipe = $dict.to_list(schema_def.types);
  let _pipe$1 = $list.map(
    _pipe,
    (entry) => { return print_type_definition(entry[1]); },
  );
  _block$1 = $list.filter(_pipe$1, (s) => { return s !== ""; });
  let type_parts = _block$1;
  let all_parts = $list.append(parts$1, type_parts);
  return $string.join(all_parts, "\n\n");
}

/**
 * Demo function to show schema printing capabilities
 */
export function demo_schema_printing() {
  $io.println("=== GeQL Schema Printer Demo ===");
  $io.println("");
  let _block;
  let _pipe = $schema.object("User");
  let _pipe$1 = $schema.description(_pipe, "A user in the system");
  let _pipe$2 = $schema.field(
    _pipe$1,
    (() => {
      let _pipe$2 = $schema.field_def("id", $schema.non_null($schema.id_type()));
      return $schema.field_description(_pipe$2, "Unique identifier");
    })(),
  );
  let _pipe$3 = $schema.field(
    _pipe$2,
    (() => {
      let _pipe$3 = $schema.field_def("name", $schema.string_type());
      return $schema.field_description(_pipe$3, "User's full name");
    })(),
  );
  _block = $schema.field(
    _pipe$3,
    (() => {
      let _pipe$4 = $schema.field_def(
        "email",
        $schema.non_null($schema.string_type()),
      );
      return $schema.field_description(_pipe$4, "User's email address");
    })(),
  );
  let user_type = _block;
  let _block$1;
  let _pipe$4 = $schema.object("Post");
  let _pipe$5 = $schema.description(_pipe$4, "A blog post");
  let _pipe$6 = $schema.field(
    _pipe$5,
    $schema.field_def("id", $schema.non_null($schema.id_type())),
  );
  let _pipe$7 = $schema.field(
    _pipe$6,
    $schema.field_def("title", $schema.non_null($schema.string_type())),
  );
  _block$1 = $schema.field(
    _pipe$7,
    $schema.field_def("content", $schema.string_type()),
  );
  let post_type = _block$1;
  let _block$2;
  let _pipe$8 = $schema.object("Query");
  let _pipe$9 = $schema.field(
    _pipe$8,
    (() => {
      let _pipe$9 = $schema.field_def("user", $schema.named_type("User"));
      return $schema.argument(
        _pipe$9,
        $schema.arg("id", $schema.non_null($schema.id_type())),
      );
    })(),
  );
  _block$2 = $schema.field(
    _pipe$9,
    $schema.field_def(
      "posts",
      $schema.list_type($schema.non_null($schema.named_type("Post"))),
    ),
  );
  let query_type = _block$2;
  let _block$3;
  let _pipe$10 = $schema.schema();
  let _pipe$11 = $schema.query(_pipe$10, query_type);
  let _pipe$12 = $schema.add_type(
    _pipe$11,
    new $schema.ObjectTypeDef(user_type),
  );
  _block$3 = $schema.add_type(_pipe$12, new $schema.ObjectTypeDef(post_type));
  let complete_schema = _block$3;
  $io.println("📄 Generated SDL:");
  $io.println("=================");
  $io.println(print_schema(complete_schema));
  $io.println("");
  return $io.println("✨ Schema printing complete!");
}
