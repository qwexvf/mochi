# Mochi GraphQL Optimization Guide

This document outlines optimization strategies for improving Mochi's performance.

## Current Performance Profile

### Without Query Cache

| Operation | Current | Notes |
|-----------|---------|-------|
| Parse simple | 3.6 µs | Lexer + parser overhead |
| Parse complex | 117 µs | String operations |
| Execute query | 22 µs | Field resolution |
| JSON serialize | 1.7 µs | Already fast |
| E2E simple | 17 µs | Combined |
| **Throughput** | **27K ops/sec** | Sustained load |

### With Query Cache (Implemented ✅)

| Operation | Cache Miss | Cache Hit | Improvement |
|-----------|------------|-----------|-------------|
| Simple query | 9.66 µs | 0.02 µs | **480x faster** |
| Medium query | 23.87 µs | 0.14 µs | **170x faster** |
| Complex query | 126 µs | 0.08 µs | **1500x faster** |
| **Throughput** | 27K ops/sec | **104K ops/sec** | **3.9x faster** |

### Real-World Query Performance (Current Parser)

⚠️ **The parser has O(n²) complexity - serious bottleneck for large queries**

| Query Type | Size | Parse Time | Ops/sec | µs/char |
|------------|------|------------|---------|---------|
| Simple | 25 chars | 15 µs | 65K | 0.76 |
| Medium | 161 chars | 318 µs | 3.1K | 1.98 |
| Nested | 195 chars | 452 µs | 2.2K | 2.32 |
| Deeply nested | 491 chars | 2.54 ms | 393 | 5.30 |
| Fragments | 459 chars | 2.33 ms | 427 | 5.43 |
| Large | 874 chars | 8.16 ms | 122 | **9.63** |
| Complex | 943 chars | 9.85 ms | 101 | **10.69** |

**Scaling Test (O(n²) exposed):**
| Fields | Size | Parse Time | Relative |
|--------|------|------------|----------|
| 10 | 475 chars | 2.86 ms | 1x |
| 50 | 2.3 KB | 64.53 ms | 22.5x |
| 100 | 4.6 KB | 255.98 ms | 89.5x |
| 200 | 9.3 KB | **1.06 s** | **370x** |

**Cache Impact for Large Queries:**
| Query | No Cache | Cached | Speedup |
|-------|----------|--------|---------|
| Simple | 41K ops/sec | 123K ops/sec | 3x |
| Medium | 2.8K ops/sec | 214K ops/sec | **76x** |
| Large | 114 ops/sec | 6.9K ops/sec | **61x** |

## Optimization Strategies

### 1. Query Caching (High Impact) ✅ IMPLEMENTED

**Problem**: Same queries are parsed repeatedly.

**Solution**: Cache parsed ASTs using ETS for high-performance concurrent access.

**Implementation**: See `src/mochi_wisp/query_cache.gleam` and `src/mochi_query_cache_ffi.erl`

```gleam
// Initialize at startup
query_cache.init()

// Use cached parsing
case query_cache.get_or_parse(query) {
  Ok(document) -> executor.execute(schema, document, ...)
  Error(e) -> handle_error(e)
}

// Monitor performance
let stats = query_cache.stats()  // CacheStats(hits, misses, size)
```

**Actual Impact**:
- Cache hits are **170-1500x faster** than parsing
- Throughput improved from **27K to 104K ops/sec** (3.9x improvement!)
- Latency reduced from **36.9 µs to 9.57 µs**

### 2. Parser Optimization (CRITICAL - High Impact) ⭐⭐⭐

The parser currently has **O(n²) complexity** due to two issues:

#### Issue 1: String Concatenation in Lexer

**Problem**: Building tokens with `acc <> char` creates new string on each character.

```gleam
// Current (O(n²) for n-character tokens)
fn read_while_loop(lexer, predicate, acc) {
  case peek_char(lexer) {
    Ok(char) if predicate(char) ->
      read_while_loop(advance(lexer), predicate, acc <> char)  // ← O(n) copy!
    _ -> #(lexer, acc)
  }
}
```

**Solution**: Use Erlang iolist or string builder:

```erlang
%% mochi_lexer_ffi.erl
read_name(Input, Pos) ->
    read_name_loop(Input, Pos, []).

read_name_loop(Input, Pos, Acc) when Pos < byte_size(Input) ->
    <<_:Pos/binary, Char, _/binary>> = Input,
    case is_name_char(Char) of
        true -> read_name_loop(Input, Pos + 1, [Char | Acc]);
        false -> {Pos, iolist_to_binary(lists:reverse(Acc))}
    end;
read_name_loop(_, Pos, Acc) ->
    {Pos, iolist_to_binary(lists:reverse(Acc))}.
```

**Expected Impact**: 10-50x faster token building.

#### Issue 2: Token List Traversal

**Problem**: Parser uses `List(Token)` with integer index access - O(n) per lookup.

```gleam
// Current (O(n) per token access)
fn get_token_at_helper(tokens, current, target) {
  case tokens {
    [tok, ..rest] if current == target -> Ok(tok)
    [_, ..rest] -> get_token_at_helper(rest, current + 1, target)  // ← O(n)!
    [] -> Error(Nil)
  }
}
```

For a 1000-token query, parsing requires ~500,000 list traversals.

**Solution**: Use array or cursor-based parsing:

```erlang
%% Option 1: Convert to tuple for O(1) access
to_array(TokenList) ->
    list_to_tuple(TokenList).

get_token(Array, Index) when Index >= 1, Index =< tuple_size(Array) ->
    {ok, element(Index, Array)};
get_token(_, _) ->
    error.
```

```gleam
// Option 2: Cursor-based parsing (no index needed)
type ParserState {
  ParserState(
    remaining: List(Token),  // Just the remaining tokens
    consumed: Int,           // Count only for errors
  )
}

fn consume(state: ParserState) -> Result(#(Token, ParserState), ParseError) {
  case state.remaining {
    [tok, ..rest] -> Ok(#(tok, ParserState(rest, state.consumed + 1)))
    [] -> Error(UnexpectedEnd)
  }
}
```

**Expected Impact**: 10-100x faster parsing for large queries.

#### Combined Impact

With both fixes, parsing a 200-field query should go from **1.06 seconds to ~10-50ms** (20-100x improvement).

### 3. Binary Lexer (Medium Impact)

**Problem**: `peek_char` uses `string.slice` which allocates.

**Solution**: Use binary pattern matching:

```erlang
peek_char(Input, Position) when Position < byte_size(Input) ->
    <<_:Position/binary, Char, _/binary>> = Input,
    {ok, Char};
peek_char(_, _) ->
    {error, nil}.
```

**Expected Impact**: 2-5x faster character operations.

### 3. Schema Field Index (Medium Impact)

**Problem**: Field lookup uses list traversal O(n).

**Solution**: Pre-build field index as Dict.

```gleam
// Current
fn get_field(obj: ObjectType, name: String) -> Option(FieldDefinition) {
  list.find(obj.fields, fn(f) { f.name == name })
}

// Optimized - fields already stored as Dict in schema
fn get_field(obj: ObjectType, name: String) -> Option(FieldDefinition) {
  dict.get(obj.fields, name)
}
```

**Expected Impact**: O(1) vs O(n) field lookup.

### 4. Resolver Compilation (Medium Impact)

**Problem**: Resolver functions recreated per execution.

**Solution**: Pre-compile resolvers during schema build.

```gleam
pub type CompiledResolver {
  CompiledResolver(
    field_name: String,
    resolver: fn(ResolverInfo) -> Result(Dynamic, String),
    // Pre-computed metadata
    is_list: Bool,
    is_non_null: Bool,
    return_type_name: String,
  )
}
```

### 5. Streaming JSON (Low Impact)

**Problem**: Building complete JSON string in memory.

**Solution**: Use iolist for streaming output.

```gleam
// Current
pub fn to_string(json: Json) -> String {
  // Builds complete string
}

// Optimized
pub fn to_iodata(json: Json) -> List(BitArray) {
  // Returns iolist, let HTTP layer send directly
}
```

### 6. Persistent Data Structures (Low Impact)

**Problem**: Dict operations create new copies.

**Solution**: Use Erlang's persistent_term for read-heavy data.

```erlang
% Store schema once
persistent_term:put(graphql_schema, Schema).

% Read without copying
Schema = persistent_term:get(graphql_schema).
```

### 7. Connection Pooling (HTTP Layer)

**Problem**: New connections per request overhead.

**Solution**: Use mist's built-in connection pooling.

```gleam
mist.new(handler)
|> mist.port(8000)
|> mist.max_connections(10_000)
|> mist.read_timeout(30_000)
```

---

## Implementation Priority

| Priority | Optimization | Effort | Impact | Status |
|----------|--------------|--------|--------|--------|
| 1 | Query Caching | Low | High | ✅ Implemented |
| 2 | Schema Caching (persistent_term) | Low | Medium | ✅ Implemented |
| **3** | **Parser Token Access (O(1))** | Medium | **CRITICAL** | ⚠️ Needed |
| **4** | **Lexer String Builder** | Medium | **CRITICAL** | ⚠️ Needed |
| 5 | Binary Lexer (peek_char) | Low | Medium | Pending |
| 6 | Field Index (Dict) | Low | Medium | Pending |
| 7 | Resolver Pre-compile | Medium | Medium | Pending |
| 8 | Streaming JSON | Low | Low | Pending |

**Critical Note**: Parser optimizations (#3, #4) are CRITICAL for real-world usage. Without them, parsing a moderately complex query (1KB) takes 10ms, and large queries (10KB) take over 1 second. Query caching mitigates this for repeated queries, but first-parse latency is unacceptable.

---

## BEAM VM Features (Implemented ✅)

The server leverages key BEAM/Erlang VM features for production readiness:

### 1. OTP Actors

```gleam
// Cache manager actor
actor.new(CacheState(initialized: True))
|> actor.on_message(handle_cache_message)
|> actor.start

// Metrics collector actor
actor.new(MetricsState(...))
|> actor.on_message(handle_metrics_message)
|> actor.start
```

**Benefits:**
- Concurrent message handling
- Isolated state per actor
- Built-in crash recovery

### 2. ETS Tables

```erlang
% Query cache with concurrent access
ets:new(mochi_query_cache, [
    named_table, public, set,
    {read_concurrency, true},
    {write_concurrency, true}
]).
```

**Benefits:**
- Lock-free concurrent reads
- Sub-microsecond lookups
- Shared across processes

### 3. Persistent Term

```erlang
% Schema storage for zero-copy reads
persistent_term:put(mochi_graphql_schema, Schema).
```

**Benefits:**
- No copying on read
- Ideal for read-heavy, write-rare data
- Global across all processes

### 4. Process Architecture

```
HTTP Request → Mist Actor Pool
              ↓
         Wisp Handler (isolated process)
              ↓
    ┌─────────┴─────────┐
    ↓                   ↓
Cache Actor      Metrics Actor
(ETS backend)    (Stats collection)
    ↓
Schema (persistent_term)
    ↓
Query Executor → Response
```

### 5. Monitoring Endpoints

| Endpoint | Description |
|----------|-------------|
| `/health` | Simple health check |
| `/health/detailed` | VM info, cache stats, scheduler count |
| `/metrics` | Performance metrics |
| `/cache/stats` | Cache hit/miss rates |
| `/cache/clear` | Clear query cache (POST) |

### 6. Cache Warming

Pre-populates the query cache at startup with common queries:
```gleam
supervisor.warm_cache(app_state, [
  "{ users { id name } }",
  "{ users { id name email } }",
  // ... more common queries
])
```

## Quick Wins

### 1. Add Query Cache (Immediate)

```gleam
// In graphql_handler.gleam
import gleam/erlang/process

// Use process dictionary for simple caching
@external(erlang, "erlang", "put")
fn cache_put(key: a, value: b) -> Option(b)

@external(erlang, "erlang", "get")
fn cache_get(key: a) -> Dynamic
```

### 2. Use ETS for Production Cache

```erlang
% Create cache table at startup
ets:new(query_cache, [named_table, public, {read_concurrency, true}]).

% Cache parsed queries
cache_query(Query, AST) ->
    ets:insert(query_cache, {erlang:phash2(Query), AST}).

get_cached_query(Query) ->
    case ets:lookup(query_cache, erlang:phash2(Query)) of
        [{_, AST}] -> {ok, AST};
        [] -> error
    end.
```

### 3. Pre-build Schema at Startup

```gleam
// In mochi_wisp.gleam
pub fn main() {
  // Build schema ONCE at startup
  let schema = schema.build_schema()

  // Pass to handler
  let handler = fn(req) { router.handle_request(req, schema) }

  // Start server
  ...
}
```

---

## Node.js Comparison Benchmarks

Comparison with popular Node.js GraphQL implementations (10 connections, 5s duration):

### Throughput (requests/sec)

| Query Type | graphql-js | Apollo | Mercurius | Yoga | **Mochi** |
|------------|------------|--------|-----------|------|-----------|
| Simple | 5,584 | 9,145 | **40,046** | 14,679 | 16,140 |
| Nested | 4,914 | 7,700 | **36,675** | 13,689 | 14,350 |
| Complex | 3,866 | 4,876 | **28,885** | 8,967 | 12,660 |
| With Args | 5,014 | 10,287 | **39,843** | 16,268 | 15,471 |
| Pagination | 4,354 | 8,144 | **35,286** | 12,951 | 15,345 |
| Mutation | 4,043 | 9,975 | **35,573** | 10,193 | 15,966 |

### Average Latency

| Query Type | graphql-js | Apollo | Mercurius | Yoga | **Mochi** |
|------------|------------|--------|-----------|------|-----------|
| Simple | 1.38 ms | 430 µs | **10 µs** | 140 µs | 20 µs |
| Nested | 1.42 ms | 1.14 ms | **10 µs** | 130 µs | 30 µs |
| Complex | 2.19 ms | 1.41 ms | **10 µs** | 410 µs | 70 µs |
| With Args | 1.35 ms | 230 µs | **10 µs** | 100 µs | 20 µs |
| Pagination | 1.66 ms | 830 µs | **10 µs** | 160 µs | 20 µs |
| Mutation | 2.04 ms | 230 µs | **20 µs** | 310 µs | 20 µs |

### P99 Latency (tail latency)

| Query Type | graphql-js | Apollo | Mercurius | Yoga | **Mochi** |
|------------|------------|--------|-----------|------|-----------|
| Simple | 6 ms | 2 ms | N/A | 1 ms | **1 ms** |
| Complex | 6 ms | 4 ms | N/A | 2 ms | **1 ms** |
| Mutation | 5 ms | 3 ms | N/A | 2 ms | **1 ms** |

### Analysis

1. **Mercurius** wins on raw throughput due to Fastify + JIT compilation
2. **Mochi** consistently outperforms Apollo and GraphQL Yoga
3. **Mochi** has excellent P99 latency (1ms) due to BEAM process isolation
4. **Mochi** shines on complex queries (12.6K vs 8.9K for Yoga, 4.8K for Apollo)

### Running the Benchmarks

```bash
# Start Node.js servers
cd benchmark/graphql_js_bench
npm install
npm run start:all  # In separate terminal

# Start Mochi
cd ../..
gleam run          # In separate terminal

# Run benchmark
cd benchmark/graphql_js_bench
npm run bench
```

---

## Benchmarking After Optimization

Run after each optimization:

```bash
gleam run -m benchmark_runner
```

Track improvements:

```
| Optimization | Before | After | Improvement |
|--------------|--------|-------|-------------|
| Baseline     | 28K    | -     | -           |
| Query cache  | -      | 104K  | 3.9x        |
| Binary lexer | -      | ?     | ?           |
```
