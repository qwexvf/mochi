# Selection projection

A resolver can see which fields the client asked for, so it can fetch only the
columns it needs instead of `select *`.

The executor sets `selection` on the `ExecutionContext` before each resolver
call. It holds the fields the client selected **on the value that resolver
returns** — for `{ user { id name } }`, the `user` resolver sees `["id", "name"]`.

It is computed on first read, so a resolver that never asks pays nothing. Each
read recomputes, so bind it once when you need it more than twice:

```gleam
let selected = query.selection(ctx)   // List(SelectedField), computed once
```

## Reading the selection

One entry point, then plain functions over the list:

```gleam
import mochi/schema
import mochi/selection

resolve: fn(args, ctx) {
  let selected = schema.selection(ctx)

  selected |> selection.names                       // ["id", "name"]
  selected |> selection.has("posts")                // worth a join?
  selected |> selection.children("posts")           // the nested selection
  selected |> selection.find("posts")               // one entry per call
  selected |> selection.for_type("Cat")             // narrow a union
  selected |> selection.columns(always: ["id"])     // what to select
  ...
}
```

`schema.selection` recomputes on every call, so bind it once and pipe. That is the
whole API: everything else composes from these.

Deciding whether a join is worth doing:

```gleam
resolve: fn(ctx) {
  case schema.selection(ctx) |> selection.has("posts") {
    True -> fetch_users_with_posts()
    False -> fetch_users()
  }
}
```

## Projecting to columns

Each field declares the columns it needs, next to the field:

```gleam
types.object("User")
// undeclared: the column is the field name in snake_case
|> types.id("id", fn(u: UserRow) { u.id })                 // "id"
|> types.string("email", fn(u: UserRow) { u.email })       // "email"
|> types.string("memberSince", fn(u: UserRow) { u.created_at })
|> types.from_columns(["created_at"])                      // spelled differently
// one field, two columns
|> types.string("fullName", fn(u: UserRow) { u.first_name <> " " <> u.last_name })
|> types.from_columns(["first_name", "last_name"])
|> types.build(decode_user)
// a join, not a column
|> schema.field(query.to_field_def(posts_field) |> schema.field_no_columns)
```

The resolver then asks for the columns, with nothing to keep in sync:

```gleam
resolve: fn(args, ctx) {
  use id <- result.try(query.get_id(args, "id"))
  let columns = query.selected_columns(ctx, always: ["id"])
  // "{ user(id: 1) { fullName email } }"
  //   -> ["first_name", "last_name", "email", "id"]
  db.select(columns, from: "users", where: id)
}
```

`query.selected_columns` is sugar for the common case; it is exactly
`schema.selection(ctx) |> selection.columns(always: ...)`.

`always` is appended unconditionally. Put the primary key and any foreign key a
nested resolver needs there, otherwise a query that selects only computed fields
comes back with no key to join on.

For a parent that prefetches children in one statement, the nested field carries
its own columns:

```gleam
case schema.selection(ctx) |> selection.find("posts") {
  [posts, ..] -> {
    let post_columns = selection.columns(posts.children, always: ["user_id"])
    let limit = selection.int_argument(posts, "first") |> option.unwrap(10)
    prefetch_posts(post_columns, limit)
  }
  // nobody asked for posts — skip the join
  [] -> Ok(rows)
}
```

### Why the default is a guess, not nothing

An undeclared field asks for its own name in snake_case (`publishedAt` ->
`published_at`), never for no column at all. That is deliberate: a forgotten
declaration then produces a *loud* database error about a column that doesn't
exist, instead of a row silently missing the data a field was about to read.

`snake_case` collapses capital runs: `userID` -> `user_id`, `HTTPStatus` ->
`http_status`, and an already-snake name passes through.

### Catching a wrong declaration before a request does

`query.check_columns` compares every field of a type against the table's real
column list. Run it in a test, and a renamed column fails there:

```gleam
pub fn user_columns_match_table_test() {
  query.check_columns(my_schema, "User", table: [
    "id", "first_name", "last_name", "email", "created_at", "bio",
  ])
  |> should.equal(Ok(Nil))
}
```

A mismatch names the field, the column, and — when the field was never
declared — how to fix it:

```
User.memberSince needs column 'created_at', which the table does not have
```

Fields declared `no_columns` are ignored, since they claim nothing.

### When the mapping has to live outside the schema

There is no separate mapping API: `selection.names` gives the field names, so a
lookup of your own is three lines when the mapping genuinely cannot live in the
schema.

## Unions and interfaces

Fields selected inside a fragment remember which concrete types the fragment's
type condition covers — interfaces expand to their implementors, unions to their
members, and nested conditions intersect. A resolver that returns one of several
types narrows the selection to the type it is about to return:

```graphql
{
  animal {
    id
    ... on Cat { meows }
    ... on Dog { barks }
  }
}
```

```gleam
let selected = schema.selection(ctx)

selected |> selection.names                                    // every type
selected |> selection.for_type("Cat") |> selection.names       // ["id", "meows"]
selected |> selection.for_type("Dog") |> selection.names       // ["id", "barks"]
selected |> selection.for_type("Cat") |> selection.columns(always: ["id"])
```

`selection.names` stays the union across all possible types, which is the safe
default when you resolve every type through one query. Reach for the `_for`
`for_type` when a sibling type's fields would otherwise become columns your table
doesn't have — and note that an empty result is itself useful: if
`for_type(selected, "Cat")` is empty, nobody asked about cats and that table need
not be queried at all.

Each type's columns come from its own field declarations, so a union needs no
extra wiring.

## Arguments on selected fields

Each selected field carries the arguments written on it, with variables
substituted — enough to honour a nested `first:` when prefetching children:

```graphql
{ user { posts(first: 10) { id title } } }
```

```gleam
resolve: fn(ctx) {
  let limit =
    schema.selection(ctx)
    |> selection.find("posts")
    |> list.first
    |> result.map(selection.int_argument(_, "first"))
    |> result.unwrap(option.None)
    |> option.unwrap(20)
  fetch_user_with_posts(limit)
}
```

`selection.int_argument`, `string_argument` and `bool_argument` decode one
argument; `selection.argument` is the raw `Dynamic` behind them.

These are raw literal shapes, **not** coerced against the schema: no default
values are filled in, enum values arrive as strings, and an unbound variable is
absent rather than null. Coerced arguments still come from the resolver's own
`Args`.

## What the selection contains

- Schema field names, never response aliases — `{ who: name }` gives `"name"`.
- `@skip(if: true)` / `@include(if: false)` fields are already removed.
- Meta fields (`__typename`, `__schema`, `__type`) are excluded.
- Empty for leaf fields, and for a field whose resolver returns a scalar.
- Each entry carries the columns its field declared, resolved against the type
  the selection was written on.
- Entries are deduped by name **plus type condition plus arguments**, merging
  sub-selections and keeping first-seen order. So a field selected twice with
  different arguments stays two entries:

```graphql
{ posts(first: 1) { id } recent: posts(first: 5) { title } }
```

```gleam
let selected = schema.selection(ctx)
selected |> selection.find("posts")      // two entries, first: 1 and first: 5
selected |> selection.children("posts")  // pooled children: id, title
selected |> selection.names              // "posts" once — names are deduped
```

## Cost

Building the selection walks the query's selection set, not the data — so it is
bounded by document size (already capped by `mochi/security`), not by rows. It is
deferred until a resolver reads it, and recomputed per resolver call: a field
resolved once per row in a list rebuilds its own sub-selection per row. On a
synthetic query with 1100 no-op resolvers, reading the selection in every one
costs roughly 0.3µs per selected field; resolvers that ignore it pay one closure
allocation. If you read it in a resolver that runs per row, bind it once with
`schema.selection(ctx)` and pass the result around.

## Testing a projection

`schema.with_selection` sets the selection directly, so projection logic can be
tested without running a query:

```gleam
let ctx =
  schema.execution_context(Nil)
  |> schema.with_selection([schema.selected_field("name", [])])

query.selected_columns(ctx, mapping: [#("name", ["name"])], always: ["id"])
|> should.equal(["name", "id"])
```

`schema.selected_field(name, children)` builds an unconditional, argument-free
entry; use the full `schema.SelectedField(..)` constructor to set `only_for` or
`arguments`.
