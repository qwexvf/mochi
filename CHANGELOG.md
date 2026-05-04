# Changelog

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
