# Notes for AI agents working on mochi

This file is a checklist of concrete mistakes prior agents have made on this
codebase. Read it before generating code. CLAUDE.md tells you what mochi
*is*; this file tells you what to *avoid*.

## API design

### Don't reach for `Dynamic` in public types

`Dynamic` is Gleam's "I don't know what's in here yet" escape hatch. In a
GraphQL library the *opposite* is true — the system constrains values
structurally:

- A resolver result is a JSON-shaped value (string / int / float / bool /
  null / list of those / object of those). It is not "any data."
- A user-provided context has a single concrete type per application. It
  is not "any data."
- An argument map has a known schema (the field's `ArgumentDefinition`s).
- A `DataLoader(K, V)` loads `V` for `K`. The K and V are known.

When you see `Dynamic` in a signature, the right move is almost always to
introduce a real type. Specifically:

| Old shape                              | Use instead                          |
| -------------------------------------- | ------------------------------------ |
| `fn(...) -> Result(Dynamic, _)` resolver | `fn(...) -> Result(output.Value, _)` |
| `user_context: Dynamic`                | parameterise: `Context(a)`           |
| `args: Dict(String, Dynamic)`          | opaque `Args` with typed getters     |
| `DataLoader` returning `Dynamic`       | `DataLoader(k, v)` (already done)    |
| extractor `fn(a) -> Dynamic`           | `fn(a) -> output.Value`              |

Yes — even when the executor *internally* still passes things around as
runtime values, the *public* surface should not say `Dynamic`. Provide
typed constructors and accessors; keep the unsafe cast inside one or two
places, behind a comment that explains the invariant.

### When `Dynamic` is acceptable

- Talking to a JSON encoder/decoder boundary (a user passes raw JSON in,
  we hand it to `gleam/dynamic/decode`).
- Inside an `@external` FFI signature.
- Internal implementation of a typed wrapper (the wrapper module itself
  uses `Dynamic`; everyone else uses the wrapper).

If you can't justify the `Dynamic` with one of those three, replace it.

## Naming

### Acronyms get cased like words

Gleam types are `UpperCamelCase`. Acronyms are *words*, not letter
sequences. So:

- `SdlDocument`, not `SDLDocument`
- `JsonValue`, not `JSONValue`
- `HttpRequest`, not `HTTPRequest`
- `IdField`, not `IDField`

This matches `gleam/dynamic/decode`'s style and what `gleam format`
expects. The compiler doesn't enforce it, but the rest of the ecosystem
follows the rule.

### Module names are `snake_case`

Acronyms inside module names: `sdl_parser.gleam`, not `SDL_parser.gleam`,
not `Sdl_parser.gleam`. `s_d_l_parser.gleam` is wrong too — they're
single tokens.

## Code patterns to avoid

### Don't write multi-decoder cascades

```gleam
// BAD — every value gets six decoder runs
case
  decode.run(v, decode.bool),
  decode.run(v, decode.int),
  decode.run(v, decode.float),
  decode.run(v, decode.string),
  decode.run(v, decode.list(decode.dynamic)),
  decode.run(v, decode.dict(...))
{ ... }
```

If you find yourself writing this, you're missing a typed value. Build a
small ADT (see `mochi/output.Value`) and walk it once. Use
`dynamic.classify` if you must inspect a runtime shape — at least it's
one pass instead of six.

### Don't grapheme-walk strings you control

```gleam
// BAD — O(n²) on Erlang for any non-trivial string
case string.pop_grapheme(s) {
  Ok(#(c, rest)) -> ...
}
```

`string.pop_grapheme` is for *external* text where Unicode boundaries
matter. If you produced the string yourself (e.g. JSON output), walk the
typed tree it came from instead. For accumulation use `string_tree`, not
repeated `<>`.

### Don't silently discard data

```gleam
// BAD — encoder swallows any value it can't classify
fn dynamic_to_json(v) {
  case ... {
    ...matches... -> ...
    _ -> json.null()  // <- data loss
  }
}
```

Return `Result`. Surface the path that failed. Callers who genuinely
want the lossy behaviour can `result.unwrap(..., default)` — that is
their explicit choice, not the library's silent default.

### Don't panic from libraries

`let assert` and `panic` belong in tests and in main(). Library code
returns `Result`. `assertion failed` is not a useful error for the user
of a library.

## Workflow

### Pin scope; resist the urge to refactor everything

This codebase had a recent round of "make the JSON layer correct"
work. The temptation was to also flatten the three schema APIs, hide
the SDL internals, and reorganise the executor — all in one diff. Don't.
Each of those is its own PR. A 5,000-line "improvement" is unreviewable.

### Run `gleam format` and `gleam check` before declaring done

Both are fast. Both catch silly mistakes. If `gleam check` is clean and
`gleam test` passes, you're allowed to claim a task is finished — not
before.

### When you change a public type, update every dependent package

The repo is a monorepo with `mochi`, `mochi_codegen`, `mochi_relay`,
`mochi_transport`, `mochi_upload`. After changing core, run
`gleam update mochi && gleam check && gleam test` in each plugin
package, and run `gleam check` in `examples/*`. Don't ship until they
all compile.

### Don't write comments that restate the code

`// Encode a Dynamic value to a JSON string` above
`pub fn encode(value: Dynamic) -> String` is noise. Doc comments should
explain *why* and *constraints*, not *what*.

## Specific traps in this codebase

- `types.to_dynamic` is `gleam_stdlib:identity` — it does no work but it
  also does no checking. Anything coerced through it is a landmine.
  Prefer typed constructors in `mochi/output`.
- `schema.UserContext` is opaque around `Dynamic`. Construct via
  `schema.user_context(value)`; read via `schema.read_user_context` or
  `schema.context_accessor`. Never re-introduce a public `Dynamic` slot
  on `ExecutionContext` — that was the mistake we just removed.
- Field arguments are `mochi/args.Args`, not `Dict(String, Dynamic)`.
  Resolvers should read with `args.get_string`, `args.get_int`, … (or
  the legacy `query.get_*` that wraps them). If you find yourself
  calling `dict.get(args, ...)` directly, escape with `args.to_dict`
  but treat that as a smell — there is almost certainly a typed
  accessor that fits.
- `ResolverInfo` carries both `args: Args` (the typed view) and
  `arguments: Dict(String, Dynamic)` (legacy). Use `args` in new code;
  `arguments` exists only to keep older resolver code compiling.
- The executor (`executor.gleam`, ~61 KB) has a lot of `to_dynamic`
  calls plumbing values through dicts. Don't grep-and-replace these
  blind — many sit at the boundary between user resolver returns and
  internal value plumbing. Change one layer at a time.

## Pending migrations

- `Resolver = fn(ResolverInfo) -> Result(Dynamic, String)` should
  become `… -> Result(output.Value, _)`. This finishes removing
  `Dynamic` from the resolver-result side. Touches every scalar builder
  in `types.gleam`, every `with_encoder` in `query.gleam`, and the
  executor's value-aggregation glue.
- Full `Schema(a)` parametrization through `Resolver(a)`,
  `ResolverInfo(a)`, `FieldDefinition(a)`, `ObjectType(a)` so the type
  system enforces matching context types between schema and execution.
  `UserContext` is a stepping stone — once the resolver return type is
  typed, it becomes mechanical to thread `a` through.
- Hide `mochi/ast`, `mochi/lexer`, `mochi/sdl_lexer`, `mochi/sdl_ast`,
  `mochi/sdl_parser` behind `mochi/internal/` (or document as
  unstable). Codegen is the only outside user; an internal import path
  is fine for it.
