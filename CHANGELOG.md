# Changelog

## 2.0.0

Tighten the public API and rewrite the lexer + JSON hot paths.

Thanks to [@lpil](https://github.com/lpil) for the reviews that drove
these changes.

### Breaking

- `json.encode` and `json.encode_pretty` return `Result(String, EncodeError)`.
  Unsupported runtime shapes (tuples, functions, references, …) surface as
  errors instead of silently encoding as `null`. The `encode_dict`,
  `encode_list`, `encode_string_value`, `encode_int`, `encode_float_value`,
  `encode_bool`, and `encode_null` helpers were unused and have been removed.
- SDL type names switched from screaming acronym to PascalCase: `SDLDocument` →
  `SdlDocument`, `SDLToken` → `SdlToken`, `SDLLexerError` → `SdlLexerError`,
  `SDLLexerState` → `SdlLexerState`, `SDLTokenWithPosition` →
  `SdlTokenWithPosition`, `SDLParser` → `SdlParser`, `SDLParseError` →
  `SdlParseError`, `SDLLexError` → `SdlLexError`, `SDLType` → `SdlType`,
  `SDLValue` → `SdlValue`.
- `schema.ExecutionContext.user_context` is now an opaque `UserContext`, not
  `Dynamic`. Construct via `schema.user_context(value)`. Read with
  `schema.read_user_context(uc, decoder)` or the existing
  `schema.context_accessor(decoder)`.
- `schema.execution_context(user_context: a)` is generic — pass any app type
  directly without `to_dynamic`.
- `schema.ResolverInfo` gained an `args: Args` field. Direct constructor
  calls must supply it.
- `schema.FieldDefinition.topic_fn` takes `mochi/args.Args` instead of
  `Dict(String, Dynamic)`.
- `query.query_with_args`, `query.mutation_with_args`,
  `query.subscription_with_args`, and `types.field_with_args` resolvers take
  `Args`. Same change for the `query.get_*` family — most callers compile
  unchanged because they were already calling `query.get_optional_int(args, "limit")`.
- `middleware.auth_middleware` extractor takes `UserContext`.
- Module paths moved under `mochi/internal/`: `mochi/ast`,
  `mochi/lexer`, `mochi/sdl_ast`, `mochi/sdl_lexer`, `mochi/sdl_parser`.

### Added

- `mochi/args` — opaque `Args` type plus typed `get_*` accessors and a
  structured `ArgError`.
- `mochi/output` — typed `Value` ADT used by the JSON encoder.
- `schema.UserContext`, `schema.user_context`, `schema.read_user_context`.

### Performance

- `mochi/internal/lexer` consumes a `BitArray` directly via prefix
  pattern-matching and captures lexemes by span-slicing instead of per-byte
  string concat. Tokenizing a kilobyte-class query goes from ~12 ms to
  ~30 µs on Erlang.
- `mochi/internal/sdl_lexer` follows the same playbook.
- `json.encode_pretty` walks the typed `output.Value` tree with `string_tree`
  rather than grapheme-popping its own JSON output (~1.5×).
- `scalars.date` and `scalars.uuid` use `bit_array.byte_size` and indexed
  byte access; no `string.to_graphemes` allocation.
- `schema_printer.print_value` and `introspection.serialize_default_value`
  drop their multi-decoder cascades for a single-pass `output.from_dynamic`.

## 1.0.0

First stable release. Same code as 0.5.0 cleanup; promoted to 1.0 to mark
the public API as stable for Hex.

### Changed
- Trimmed demo `main` and `create_demo_schema` from `mochi.gleam`; the
  module now only re-exports `parse`, `execute`, and `new_schema`.
- Removed `demo_schema_printing` from `mochi/schema_printer`.
- Dropped unused FFI files (`mochi_ffi.erl`, `mochi_random_ffi.erl`) and the
  internal `mochi_benchmark` script.

## 0.5.0

### Added
- `mochi/decoders` helpers for `build` callbacks (#6)
- Repeatable directives and the `VARIABLE_DEFINITION` directive location
- SDL type extensions (`extend type`) in `sdl_parser` and `mochi/schema`

### Fixed
- DataLoader dedup
- Stricter input coercion errors
- Propagate SDL lex errors and validate directive locations
- Reject empty type extensions per GraphQL spec §3.13
- Directives-only extensions, O(1) orphan lookup, deterministic ordering

### Changed
- Stabilised the public API for 1.0
- Removed `build_without_cache` and redundant encoders in tests

## 0.4.0

Initial public release.
