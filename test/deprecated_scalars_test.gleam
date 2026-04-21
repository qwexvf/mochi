import gleam/dynamic/decode
import gleam/list
import gleam/option.{None, Some}
import gleeunit/should
import mochi/executor
import mochi/query
import mochi/schema
import mochi/types

pub type Post {
  Post(id: String, title: String, old_slug: String, created_at: String)
}

pub type Item {
  Item(id: String, external_id: String)
}

pub type Event {
  Event(id: String, ended_at: option.Option(String))
}

pub type Contact {
  Contact(id: String, email: String, website: String)
}

fn decode_post(_dyn) {
  Ok(Post("1", "Hello", "hello-world", "2024-01-15T10:30:00Z"))
}

fn build_post_schema() -> schema.Schema {
  let post_type =
    types.object("Post")
    |> types.id("id", fn(p: Post) { p.id })
    |> types.string("title", fn(p: Post) { p.title })
    |> types.string("oldSlug", fn(p: Post) { p.old_slug })
    |> types.deprecated("Use slug instead")
    |> types.datetime("createdAt", fn(p: Post) { p.created_at })
    |> types.build(decode_post)

  let post_query =
    query.query(
      "post",
      schema.Named("Post"),
      fn(_) {
        Ok(
          types.to_dynamic(Post(
            "1",
            "Hello",
            "hello-world",
            "2024-01-15T10:30:00Z",
          )),
        )
      },
      fn(p) { types.to_dynamic(p) },
    )

  query.new()
  |> query.add_query(post_query)
  |> query.add_type(post_type)
  |> query.with_common_scalars()
  |> query.build
}

pub fn deprecated_field_reflected_in_introspection_test() {
  let schema_def = build_post_schema()
  let result =
    executor.execute_query(
      schema_def,
      "{ __type(name: \"Post\") { fields(includeDeprecated: true) { name isDeprecated deprecationReason } } }",
    )

  should.equal(result.errors, [])
  let assert Some(data) = result.data

  let assert Ok(fields) =
    decode.run(
      data,
      decode.at(["__type", "fields"], decode.list(decode.dynamic)),
    )

  let assert Ok(slug) =
    list.find(fields, fn(f) {
      decode.run(f, decode.at(["name"], decode.string)) == Ok("oldSlug")
    })

  decode.run(slug, decode.at(["isDeprecated"], decode.bool))
  |> should.equal(Ok(True))

  decode.run(slug, decode.at(["deprecationReason"], decode.string))
  |> should.equal(Ok("Use slug instead"))
}

pub fn deprecated_field_excluded_by_default_in_introspection_test() {
  let schema_def = build_post_schema()
  let result =
    executor.execute_query(
      schema_def,
      "{ __type(name: \"Post\") { fields { name } } }",
    )

  should.equal(result.errors, [])
  let assert Some(data) = result.data

  let assert Ok(names) =
    decode.run(
      data,
      decode.at(
        ["__type", "fields"],
        decode.list(decode.at(["name"], decode.string)),
      ),
    )
  should.be_false(list.contains(names, "oldSlug"))
}

pub fn deprecated_no_reason_test() {
  let post_type =
    types.object("Post2")
    |> types.id("id", fn(p: Post) { p.id })
    |> types.string("oldField", fn(_: Post) { "" })
    |> types.deprecated_no_reason()
    |> types.build(decode_post)

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        "post2",
        schema.Named("Post2"),
        fn(_) {
          Ok(
            types.to_dynamic(Post(
              "1",
              "Hello",
              "hello-world",
              "2024-01-15T10:30:00Z",
            )),
          )
        },
        fn(p) { types.to_dynamic(p) },
      ),
    )
    |> query.add_type(post_type)
    |> query.build

  let result =
    executor.execute_query(
      schema_def,
      "{ __type(name: \"Post2\") { fields(includeDeprecated: true) { name isDeprecated deprecationReason } } }",
    )

  should.equal(result.errors, [])
  let assert Some(data) = result.data

  let assert Ok(fields) =
    decode.run(
      data,
      decode.at(["__type", "fields"], decode.list(decode.dynamic)),
    )

  let assert Ok(f) =
    list.find(fields, fn(f) {
      decode.run(f, decode.at(["name"], decode.string)) == Ok("oldField")
    })

  decode.run(f, decode.at(["isDeprecated"], decode.bool))
  |> should.equal(Ok(True))
}

pub fn with_common_scalars_registers_all_test() {
  let schema_def = build_post_schema()
  list.each(["DateTime", "Date", "UUID", "Email", "URL", "JSON"], fn(name) {
    let result =
      executor.execute_query(
        schema_def,
        "{ __type(name: \"" <> name <> "\") { name } }",
      )
    should.equal(result.errors, [])
    let assert Some(data) = result.data
    decode.run(data, decode.at(["__type", "name"], decode.string))
    |> should.equal(Ok(name))
  })
}

pub fn datetime_field_resolves_test() {
  let schema_def = build_post_schema()
  let result = executor.execute_query(schema_def, "{ post { id createdAt } }")

  should.equal(result.errors, [])
  let assert Some(data) = result.data
  decode.run(data, decode.at(["post", "createdAt"], decode.string))
  |> should.equal(Ok("2024-01-15T10:30:00Z"))
}

pub fn scalar_field_type_in_introspection_test() {
  let schema_def = build_post_schema()
  let result =
    executor.execute_query(
      schema_def,
      "{ __type(name: \"Post\") { fields { name type { name } } } }",
    )

  should.equal(result.errors, [])
  let assert Some(data) = result.data

  let assert Ok(fields) =
    decode.run(
      data,
      decode.at(["__type", "fields"], decode.list(decode.dynamic)),
    )

  let assert Ok(f) =
    list.find(fields, fn(f) {
      decode.run(f, decode.at(["name"], decode.string)) == Ok("createdAt")
    })

  decode.run(f, decode.at(["type", "name"], decode.string))
  |> should.equal(Ok("DateTime"))
}

pub fn uuid_field_type_helper_test() {
  let item_type =
    types.object("Item")
    |> types.id("id", fn(i: Item) { i.id })
    |> types.uuid("externalId", fn(i: Item) { i.external_id })
    |> types.build(fn(_) {
      Ok(Item("1", "550e8400-e29b-41d4-a716-446655440000"))
    })

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        "item",
        schema.Named("Item"),
        fn(_) {
          Ok(
            types.to_dynamic(Item("1", "550e8400-e29b-41d4-a716-446655440000")),
          )
        },
        fn(i) { types.to_dynamic(i) },
      ),
    )
    |> query.add_type(item_type)
    |> query.with_common_scalars()
    |> query.build

  let result = executor.execute_query(schema_def, "{ item { id externalId } }")
  should.equal(result.errors, [])
  let assert Some(data) = result.data
  decode.run(data, decode.at(["item", "externalId"], decode.string))
  |> should.equal(Ok("550e8400-e29b-41d4-a716-446655440000"))
}

pub fn optional_datetime_field_test() {
  let event_type =
    types.object("Event")
    |> types.id("id", fn(e: Event) { e.id })
    |> types.optional_datetime("endedAt", fn(e: Event) { e.ended_at })
    |> types.build(fn(_) { Ok(Event("1", None)) })

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        "event",
        schema.Named("Event"),
        fn(_) { Ok(types.to_dynamic(Event("1", None))) },
        fn(e) { types.to_dynamic(e) },
      ),
    )
    |> query.add_type(event_type)
    |> query.with_common_scalars()
    |> query.build

  let result = executor.execute_query(schema_def, "{ event { id endedAt } }")
  should.equal(result.errors, [])
  let assert Some(data) = result.data
  decode.run(
    data,
    decode.at(["event", "endedAt"], decode.optional(decode.string)),
  )
  |> should.equal(Ok(None))
}

pub fn email_and_url_field_helpers_test() {
  let contact_type =
    types.object("Contact")
    |> types.id("id", fn(c: Contact) { c.id })
    |> types.email("email", fn(c: Contact) { c.email })
    |> types.url("website", fn(c: Contact) { c.website })
    |> types.build(fn(_) {
      Ok(Contact("1", "alice@example.com", "https://example.com"))
    })

  let schema_def =
    query.new()
    |> query.add_query(
      query.query(
        "contact",
        schema.Named("Contact"),
        fn(_) {
          Ok(
            types.to_dynamic(Contact(
              "1",
              "alice@example.com",
              "https://example.com",
            )),
          )
        },
        fn(c) { types.to_dynamic(c) },
      ),
    )
    |> query.add_type(contact_type)
    |> query.with_common_scalars()
    |> query.build

  let result =
    executor.execute_query(schema_def, "{ contact { id email website } }")
  should.equal(result.errors, [])
  let assert Some(data) = result.data
  decode.run(data, decode.at(["contact", "email"], decode.string))
  |> should.equal(Ok("alice@example.com"))
  decode.run(data, decode.at(["contact", "website"], decode.string))
  |> should.equal(Ok("https://example.com"))
}

pub fn schema_scalar_type_helpers_test() {
  should.equal(schema.datetime_type(), schema.Named("DateTime"))
  should.equal(schema.date_type(), schema.Named("Date"))
  should.equal(schema.uuid_type(), schema.Named("UUID"))
  should.equal(schema.email_type(), schema.Named("Email"))
  should.equal(schema.url_type(), schema.Named("URL"))
  should.equal(schema.json_type(), schema.Named("JSON"))
}

pub fn deprecated_on_empty_builder_is_noop_test() {
  let builder =
    types.object("Empty")
    |> types.deprecated("ignored")
  should.equal(builder.fields, [])
}

pub fn non_deprecated_fields_still_resolve_test() {
  let schema_def = build_post_schema()
  let result = executor.execute_query(schema_def, "{ post { id title } }")
  should.equal(result.errors, [])
  let assert Some(data) = result.data
  decode.run(data, decode.at(["post", "title"], decode.string))
  |> should.equal(Ok("Hello"))
}
