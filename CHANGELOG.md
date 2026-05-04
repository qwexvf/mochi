# Changelog

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
