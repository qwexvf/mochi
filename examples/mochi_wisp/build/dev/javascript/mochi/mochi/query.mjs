import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import {
  Ok,
  Error,
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../gleam.mjs";
import * as $schema from "../mochi/schema.mjs";

export class QueryDef extends $CustomType {
  constructor(name, description, args_decoder, resolver, result_encoder, arg_definitions, return_type) {
    super();
    this.name = name;
    this.description = description;
    this.args_decoder = args_decoder;
    this.resolver = resolver;
    this.result_encoder = result_encoder;
    this.arg_definitions = arg_definitions;
    this.return_type = return_type;
  }
}
export const QueryDef$QueryDef = (name, description, args_decoder, resolver, result_encoder, arg_definitions, return_type) =>
  new QueryDef(name,
  description,
  args_decoder,
  resolver,
  result_encoder,
  arg_definitions,
  return_type);
export const QueryDef$isQueryDef = (value) => value instanceof QueryDef;
export const QueryDef$QueryDef$name = (value) => value.name;
export const QueryDef$QueryDef$0 = (value) => value.name;
export const QueryDef$QueryDef$description = (value) => value.description;
export const QueryDef$QueryDef$1 = (value) => value.description;
export const QueryDef$QueryDef$args_decoder = (value) => value.args_decoder;
export const QueryDef$QueryDef$2 = (value) => value.args_decoder;
export const QueryDef$QueryDef$resolver = (value) => value.resolver;
export const QueryDef$QueryDef$3 = (value) => value.resolver;
export const QueryDef$QueryDef$result_encoder = (value) => value.result_encoder;
export const QueryDef$QueryDef$4 = (value) => value.result_encoder;
export const QueryDef$QueryDef$arg_definitions = (value) =>
  value.arg_definitions;
export const QueryDef$QueryDef$5 = (value) => value.arg_definitions;
export const QueryDef$QueryDef$return_type = (value) => value.return_type;
export const QueryDef$QueryDef$6 = (value) => value.return_type;

export class MutationDef extends $CustomType {
  constructor(name, description, args_decoder, resolver, result_encoder, arg_definitions, return_type) {
    super();
    this.name = name;
    this.description = description;
    this.args_decoder = args_decoder;
    this.resolver = resolver;
    this.result_encoder = result_encoder;
    this.arg_definitions = arg_definitions;
    this.return_type = return_type;
  }
}
export const MutationDef$MutationDef = (name, description, args_decoder, resolver, result_encoder, arg_definitions, return_type) =>
  new MutationDef(name,
  description,
  args_decoder,
  resolver,
  result_encoder,
  arg_definitions,
  return_type);
export const MutationDef$isMutationDef = (value) =>
  value instanceof MutationDef;
export const MutationDef$MutationDef$name = (value) => value.name;
export const MutationDef$MutationDef$0 = (value) => value.name;
export const MutationDef$MutationDef$description = (value) => value.description;
export const MutationDef$MutationDef$1 = (value) => value.description;
export const MutationDef$MutationDef$args_decoder = (value) =>
  value.args_decoder;
export const MutationDef$MutationDef$2 = (value) => value.args_decoder;
export const MutationDef$MutationDef$resolver = (value) => value.resolver;
export const MutationDef$MutationDef$3 = (value) => value.resolver;
export const MutationDef$MutationDef$result_encoder = (value) =>
  value.result_encoder;
export const MutationDef$MutationDef$4 = (value) => value.result_encoder;
export const MutationDef$MutationDef$arg_definitions = (value) =>
  value.arg_definitions;
export const MutationDef$MutationDef$5 = (value) => value.arg_definitions;
export const MutationDef$MutationDef$return_type = (value) => value.return_type;
export const MutationDef$MutationDef$6 = (value) => value.return_type;

export class SubscriptionDef extends $CustomType {
  constructor(name, description, args_decoder, topic_resolver, event_encoder, arg_definitions, return_type) {
    super();
    this.name = name;
    this.description = description;
    this.args_decoder = args_decoder;
    this.topic_resolver = topic_resolver;
    this.event_encoder = event_encoder;
    this.arg_definitions = arg_definitions;
    this.return_type = return_type;
  }
}
export const SubscriptionDef$SubscriptionDef = (name, description, args_decoder, topic_resolver, event_encoder, arg_definitions, return_type) =>
  new SubscriptionDef(name,
  description,
  args_decoder,
  topic_resolver,
  event_encoder,
  arg_definitions,
  return_type);
export const SubscriptionDef$isSubscriptionDef = (value) =>
  value instanceof SubscriptionDef;
export const SubscriptionDef$SubscriptionDef$name = (value) => value.name;
export const SubscriptionDef$SubscriptionDef$0 = (value) => value.name;
export const SubscriptionDef$SubscriptionDef$description = (value) =>
  value.description;
export const SubscriptionDef$SubscriptionDef$1 = (value) => value.description;
export const SubscriptionDef$SubscriptionDef$args_decoder = (value) =>
  value.args_decoder;
export const SubscriptionDef$SubscriptionDef$2 = (value) => value.args_decoder;
export const SubscriptionDef$SubscriptionDef$topic_resolver = (value) =>
  value.topic_resolver;
export const SubscriptionDef$SubscriptionDef$3 = (value) =>
  value.topic_resolver;
export const SubscriptionDef$SubscriptionDef$event_encoder = (value) =>
  value.event_encoder;
export const SubscriptionDef$SubscriptionDef$4 = (value) => value.event_encoder;
export const SubscriptionDef$SubscriptionDef$arg_definitions = (value) =>
  value.arg_definitions;
export const SubscriptionDef$SubscriptionDef$5 = (value) =>
  value.arg_definitions;
export const SubscriptionDef$SubscriptionDef$return_type = (value) =>
  value.return_type;
export const SubscriptionDef$SubscriptionDef$6 = (value) => value.return_type;

export class FieldDef extends $CustomType {
  constructor(name, description, parent_decoder, args_decoder, resolver, result_encoder, arg_definitions, return_type) {
    super();
    this.name = name;
    this.description = description;
    this.parent_decoder = parent_decoder;
    this.args_decoder = args_decoder;
    this.resolver = resolver;
    this.result_encoder = result_encoder;
    this.arg_definitions = arg_definitions;
    this.return_type = return_type;
  }
}
export const FieldDef$FieldDef = (name, description, parent_decoder, args_decoder, resolver, result_encoder, arg_definitions, return_type) =>
  new FieldDef(name,
  description,
  parent_decoder,
  args_decoder,
  resolver,
  result_encoder,
  arg_definitions,
  return_type);
export const FieldDef$isFieldDef = (value) => value instanceof FieldDef;
export const FieldDef$FieldDef$name = (value) => value.name;
export const FieldDef$FieldDef$0 = (value) => value.name;
export const FieldDef$FieldDef$description = (value) => value.description;
export const FieldDef$FieldDef$1 = (value) => value.description;
export const FieldDef$FieldDef$parent_decoder = (value) => value.parent_decoder;
export const FieldDef$FieldDef$2 = (value) => value.parent_decoder;
export const FieldDef$FieldDef$args_decoder = (value) => value.args_decoder;
export const FieldDef$FieldDef$3 = (value) => value.args_decoder;
export const FieldDef$FieldDef$resolver = (value) => value.resolver;
export const FieldDef$FieldDef$4 = (value) => value.resolver;
export const FieldDef$FieldDef$result_encoder = (value) => value.result_encoder;
export const FieldDef$FieldDef$5 = (value) => value.result_encoder;
export const FieldDef$FieldDef$arg_definitions = (value) =>
  value.arg_definitions;
export const FieldDef$FieldDef$6 = (value) => value.arg_definitions;
export const FieldDef$FieldDef$return_type = (value) => value.return_type;
export const FieldDef$FieldDef$7 = (value) => value.return_type;

export class ArgDef extends $CustomType {
  constructor(name, arg_type, description) {
    super();
    this.name = name;
    this.arg_type = arg_type;
    this.description = description;
  }
}
export const ArgDef$ArgDef = (name, arg_type, description) =>
  new ArgDef(name, arg_type, description);
export const ArgDef$isArgDef = (value) => value instanceof ArgDef;
export const ArgDef$ArgDef$name = (value) => value.name;
export const ArgDef$ArgDef$0 = (value) => value.name;
export const ArgDef$ArgDef$arg_type = (value) => value.arg_type;
export const ArgDef$ArgDef$1 = (value) => value.arg_type;
export const ArgDef$ArgDef$description = (value) => value.description;
export const ArgDef$ArgDef$2 = (value) => value.description;

export class NoArgs extends $CustomType {}
export const NoArgs$NoArgs = () => new NoArgs();
export const NoArgs$isNoArgs = (value) => value instanceof NoArgs;

export class SchemaBuilder extends $CustomType {
  constructor(queries, mutations, subscriptions, types, enums, interfaces, unions) {
    super();
    this.queries = queries;
    this.mutations = mutations;
    this.subscriptions = subscriptions;
    this.types = types;
    this.enums = enums;
    this.interfaces = interfaces;
    this.unions = unions;
  }
}
export const SchemaBuilder$SchemaBuilder = (queries, mutations, subscriptions, types, enums, interfaces, unions) =>
  new SchemaBuilder(queries,
  mutations,
  subscriptions,
  types,
  enums,
  interfaces,
  unions);
export const SchemaBuilder$isSchemaBuilder = (value) =>
  value instanceof SchemaBuilder;
export const SchemaBuilder$SchemaBuilder$queries = (value) => value.queries;
export const SchemaBuilder$SchemaBuilder$0 = (value) => value.queries;
export const SchemaBuilder$SchemaBuilder$mutations = (value) => value.mutations;
export const SchemaBuilder$SchemaBuilder$1 = (value) => value.mutations;
export const SchemaBuilder$SchemaBuilder$subscriptions = (value) =>
  value.subscriptions;
export const SchemaBuilder$SchemaBuilder$2 = (value) => value.subscriptions;
export const SchemaBuilder$SchemaBuilder$types = (value) => value.types;
export const SchemaBuilder$SchemaBuilder$3 = (value) => value.types;
export const SchemaBuilder$SchemaBuilder$enums = (value) => value.enums;
export const SchemaBuilder$SchemaBuilder$4 = (value) => value.enums;
export const SchemaBuilder$SchemaBuilder$interfaces = (value) =>
  value.interfaces;
export const SchemaBuilder$SchemaBuilder$5 = (value) => value.interfaces;
export const SchemaBuilder$SchemaBuilder$unions = (value) => value.unions;
export const SchemaBuilder$SchemaBuilder$6 = (value) => value.unions;

/**
 * Define a query with no arguments
 */
export function query(name, return_type, resolver, encoder) {
  return new QueryDef(
    name,
    new None(),
    (_) => { return new Ok(new NoArgs()); },
    (_, ctx) => { return resolver(ctx); },
    encoder,
    toList([]),
    return_type,
  );
}

/**
 * Define a query with arguments
 */
export function query_with_args(
  name,
  arg_defs,
  return_type,
  args_decoder,
  resolver,
  encoder
) {
  return new QueryDef(
    name,
    new None(),
    args_decoder,
    resolver,
    encoder,
    arg_defs,
    return_type,
  );
}

/**
 * Add description to a query
 */
export function query_description(q, desc) {
  return new QueryDef(
    q.name,
    new Some(desc),
    q.args_decoder,
    q.resolver,
    q.result_encoder,
    q.arg_definitions,
    q.return_type,
  );
}

/**
 * Define a mutation with arguments
 */
export function mutation(
  name,
  arg_defs,
  return_type,
  args_decoder,
  resolver,
  encoder
) {
  return new MutationDef(
    name,
    new None(),
    args_decoder,
    resolver,
    encoder,
    arg_defs,
    return_type,
  );
}

/**
 * Add description to a mutation
 */
export function mutation_description(m, desc) {
  return new MutationDef(
    m.name,
    new Some(desc),
    m.args_decoder,
    m.resolver,
    m.result_encoder,
    m.arg_definitions,
    m.return_type,
  );
}

/**
 * Define a subscription with no arguments
 */
export function subscription(name, return_type, topic, encoder) {
  return new SubscriptionDef(
    name,
    new None(),
    (_) => { return new Ok(new NoArgs()); },
    (_, _1) => { return new Ok(topic); },
    encoder,
    toList([]),
    return_type,
  );
}

/**
 * Define a subscription with arguments
 */
export function subscription_with_args(
  name,
  arg_defs,
  return_type,
  args_decoder,
  topic_resolver,
  encoder
) {
  return new SubscriptionDef(
    name,
    new None(),
    args_decoder,
    topic_resolver,
    encoder,
    arg_defs,
    return_type,
  );
}

/**
 * Add description to a subscription
 */
export function subscription_description(s, desc) {
  return new SubscriptionDef(
    s.name,
    new Some(desc),
    s.args_decoder,
    s.topic_resolver,
    s.event_encoder,
    s.arg_definitions,
    s.return_type,
  );
}

/**
 * Define a field on a type
 */
export function field(name, return_type, parent_decoder, resolver, encoder) {
  return new FieldDef(
    name,
    new None(),
    parent_decoder,
    (_) => { return new Ok(new NoArgs()); },
    (parent, _, ctx) => { return resolver(parent, ctx); },
    encoder,
    toList([]),
    return_type,
  );
}

/**
 * Define a field with arguments
 */
export function field_with_args(
  name,
  arg_defs,
  return_type,
  parent_decoder,
  args_decoder,
  resolver,
  encoder
) {
  return new FieldDef(
    name,
    new None(),
    parent_decoder,
    args_decoder,
    resolver,
    encoder,
    arg_defs,
    return_type,
  );
}

/**
 * Add description to a field
 */
export function field_description(f, desc) {
  return new FieldDef(
    f.name,
    new Some(desc),
    f.parent_decoder,
    f.args_decoder,
    f.resolver,
    f.result_encoder,
    f.arg_definitions,
    f.return_type,
  );
}

/**
 * Create an argument definition
 */
export function arg(name, arg_type) {
  return new ArgDef(name, arg_type, new None());
}

/**
 * Create an argument with description
 */
export function arg_with_desc(name, arg_type, description) {
  return new ArgDef(name, arg_type, new Some(description));
}

function add_args_to_field(field, args) {
  return $list.fold(
    args,
    field,
    (f, a) => {
      let _block;
      let $ = a.description;
      if ($ instanceof Some) {
        let desc = $[0];
        let _pipe = $schema.arg(a.name, a.arg_type);
        _block = $schema.arg_description(_pipe, desc);
      } else {
        _block = $schema.arg(a.name, a.arg_type);
      }
      let arg_def = _block;
      return $schema.argument(f, arg_def);
    },
  );
}

/**
 * Convert a QueryDef to a schema FieldDefinition
 */
export function query_to_field_def(q) {
  let resolver = (info) => {
    let $ = q.args_decoder(info.arguments);
    if ($ instanceof Ok) {
      let decoded_args = $[0];
      let $1 = q.resolver(decoded_args, info.context);
      if ($1 instanceof Ok) {
        let result = $1[0];
        return new Ok(q.result_encoder(result));
      } else {
        return $1;
      }
    } else {
      let e = $[0];
      return new Error("Failed to decode arguments: " + e);
    }
  };
  let _block;
  let _pipe = $schema.field_def(q.name, q.return_type);
  _block = $schema.resolver(_pipe, resolver);
  let base = _block;
  let with_args = add_args_to_field(base, q.arg_definitions);
  let $ = q.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return $schema.field_description(with_args, desc);
  } else {
    return with_args;
  }
}

/**
 * Convert a MutationDef to a schema FieldDefinition
 */
export function mutation_to_field_def(m) {
  let resolver = (info) => {
    let $ = m.args_decoder(info.arguments);
    if ($ instanceof Ok) {
      let decoded_args = $[0];
      let $1 = m.resolver(decoded_args, info.context);
      if ($1 instanceof Ok) {
        let result = $1[0];
        return new Ok(m.result_encoder(result));
      } else {
        return $1;
      }
    } else {
      let e = $[0];
      return new Error("Failed to decode arguments: " + e);
    }
  };
  let _block;
  let _pipe = $schema.field_def(m.name, m.return_type);
  _block = $schema.resolver(_pipe, resolver);
  let base = _block;
  let with_args = add_args_to_field(base, m.arg_definitions);
  let $ = m.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return $schema.field_description(with_args, desc);
  } else {
    return with_args;
  }
}

/**
 * Convert a SubscriptionDef to a schema FieldDefinition
 * Note: The actual subscription logic is handled by the subscription executor,
 * this just creates the schema definition for introspection and SDL generation
 */
export function subscription_to_field_def(s) {
  let resolver = (_) => {
    return new Error(
      "Subscriptions must be executed through the subscription executor",
    );
  };
  let _block;
  let _pipe = $schema.field_def(s.name, s.return_type);
  _block = $schema.resolver(_pipe, resolver);
  let base = _block;
  let with_args = add_args_to_field(base, s.arg_definitions);
  let $ = s.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return $schema.field_description(with_args, desc);
  } else {
    return with_args;
  }
}

/**
 * Convert a FieldDef to a schema FieldDefinition
 */
export function field_def_to_schema(f) {
  let resolver = (info) => {
    let $ = info.parent;
    if ($ instanceof Some) {
      let parent_dyn = $[0];
      let $1 = f.parent_decoder(parent_dyn);
      if ($1 instanceof Ok) {
        let parent = $1[0];
        let $2 = f.args_decoder(info.arguments);
        if ($2 instanceof Ok) {
          let decoded_args = $2[0];
          let $3 = f.resolver(parent, decoded_args, info.context);
          if ($3 instanceof Ok) {
            let result = $3[0];
            return new Ok(f.result_encoder(result));
          } else {
            return $3;
          }
        } else {
          let e = $2[0];
          return new Error("Failed to decode arguments: " + e);
        }
      } else {
        let e = $1[0];
        return new Error("Failed to decode parent: " + e);
      }
    } else {
      return new Error("No parent value provided");
    }
  };
  let _block;
  let _pipe = $schema.field_def(f.name, f.return_type);
  _block = $schema.resolver(_pipe, resolver);
  let base = _block;
  let with_args = add_args_to_field(base, f.arg_definitions);
  let $ = f.description;
  if ($ instanceof Some) {
    let desc = $[0];
    return $schema.field_description(with_args, desc);
  } else {
    return with_args;
  }
}

/**
 * Create a new schema builder
 */
export function new$() {
  return new SchemaBuilder(
    toList([]),
    toList([]),
    toList([]),
    toList([]),
    toList([]),
    toList([]),
    toList([]),
  );
}

/**
 * Add a query to the schema
 */
export function add_query(builder, q) {
  return new SchemaBuilder(
    listPrepend(query_to_field_def(q), builder.queries),
    builder.mutations,
    builder.subscriptions,
    builder.types,
    builder.enums,
    builder.interfaces,
    builder.unions,
  );
}

/**
 * Add a mutation to the schema
 */
export function add_mutation(builder, m) {
  return new SchemaBuilder(
    builder.queries,
    listPrepend(mutation_to_field_def(m), builder.mutations),
    builder.subscriptions,
    builder.types,
    builder.enums,
    builder.interfaces,
    builder.unions,
  );
}

/**
 * Add a subscription to the schema
 */
export function add_subscription(builder, s) {
  return new SchemaBuilder(
    builder.queries,
    builder.mutations,
    listPrepend(subscription_to_field_def(s), builder.subscriptions),
    builder.types,
    builder.enums,
    builder.interfaces,
    builder.unions,
  );
}

/**
 * Add a type definition
 */
export function add_type(builder, t) {
  return new SchemaBuilder(
    builder.queries,
    builder.mutations,
    builder.subscriptions,
    listPrepend(t, builder.types),
    builder.enums,
    builder.interfaces,
    builder.unions,
  );
}

/**
 * Add an enum type
 */
export function add_enum(builder, e) {
  return new SchemaBuilder(
    builder.queries,
    builder.mutations,
    builder.subscriptions,
    builder.types,
    listPrepend(e, builder.enums),
    builder.interfaces,
    builder.unions,
  );
}

/**
 * Add an interface type
 */
export function add_interface(builder, i) {
  return new SchemaBuilder(
    builder.queries,
    builder.mutations,
    builder.subscriptions,
    builder.types,
    builder.enums,
    listPrepend(i, builder.interfaces),
    builder.unions,
  );
}

/**
 * Add a union type
 */
export function add_union(builder, u) {
  return new SchemaBuilder(
    builder.queries,
    builder.mutations,
    builder.subscriptions,
    builder.types,
    builder.enums,
    builder.interfaces,
    listPrepend(u, builder.unions),
  );
}

/**
 * Build the final schema
 */
export function build(builder) {
  let query_type = $list.fold(
    builder.queries,
    $schema.object("Query"),
    (obj, field) => { return $schema.field(obj, field); },
  );
  let _block;
  let _pipe = $schema.schema();
  _block = $schema.query(_pipe, query_type);
  let base_schema = _block;
  let _block$1;
  let $ = builder.mutations;
  if ($ instanceof $Empty) {
    _block$1 = base_schema;
  } else {
    let mutation_type = $list.fold(
      builder.mutations,
      $schema.object("Mutation"),
      (obj, field) => { return $schema.field(obj, field); },
    );
    _block$1 = $schema.mutation(base_schema, mutation_type);
  }
  let with_mutation = _block$1;
  let _block$2;
  let $1 = builder.subscriptions;
  if ($1 instanceof $Empty) {
    _block$2 = with_mutation;
  } else {
    let subscription_type = $list.fold(
      builder.subscriptions,
      $schema.object("Subscription"),
      (obj, field) => { return $schema.field(obj, field); },
    );
    _block$2 = $schema.subscription(with_mutation, subscription_type);
  }
  let with_subscription = _block$2;
  let with_types = $list.fold(
    builder.types,
    with_subscription,
    (s, t) => { return $schema.add_type(s, new $schema.ObjectTypeDef(t)); },
  );
  let with_enums = $list.fold(
    builder.enums,
    with_types,
    (s, e) => { return $schema.add_type(s, new $schema.EnumTypeDef(e)); },
  );
  let with_interfaces = $list.fold(
    builder.interfaces,
    with_enums,
    (s, i) => { return $schema.add_type(s, new $schema.InterfaceTypeDef(i)); },
  );
  return $list.fold(
    builder.unions,
    with_interfaces,
    (s, u) => { return $schema.add_type(s, new $schema.UnionTypeDef(u)); },
  );
}
