# Mochi Wisp Example

A GraphQL server example using [Mochi](../../) with [Wisp](https://hexdocs.pm/wisp/) and [Mist](https://hexdocs.pm/mist/).

## Quick Start

```bash
# Run the server
gleam run

# Test with curl
curl -X POST http://localhost:8000/graphql \
  -H "Content-Type: application/json" \
  -d '{"query": "{ users { id name email role } }"}'
```

## API

### Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/graphql` | Execute GraphQL queries |
| OPTIONS | `/graphql` | CORS preflight |
| GET | `/health` | Health check |

### Schema

```graphql
type Query {
  users: [User!]!
  user(id: ID!): User
}

type User {
  id: ID!
  name: String!
  email: String!
  role: Role!
}

enum Role {
  ADMIN
  MEMBER
  GUEST
}
```

### Example Queries

```graphql
# Get all users
{ users { id name email role } }

# Get user by ID
{ user(id: "1") { id name email } }

# With variables
query GetUser($id: ID!) {
  user(id: $id) { id name email role }
}
```

## Running Tests

```bash
gleam test
```

## Benchmarks

### Internal Benchmark

```bash
gleam run -m benchmark_runner
```

### Mochi Performance Results

| Operation | Latency | Throughput |
|-----------|---------|------------|
| Parse simple query | 3.6 µs | 281K ops/sec |
| Parse medium query | 17.6 µs | 57K ops/sec |
| Parse complex query | 117 µs | 8.5K ops/sec |
| Cache HIT (simple) | 0.02 µs | 49M ops/sec |
| Cache HIT (complex) | 0.08 µs | 12M ops/sec |
| Execute users query | 22 µs | 45K ops/sec |
| Execute user by id | 27 µs | 37K ops/sec |
| JSON serialization | 1.7 µs | 574K ops/sec |
| **E2E simple query** | **17 µs** | **57K ops/sec** |
| **E2E complex query** | **80 µs** | **12K ops/sec** |
| **Sustained (no cache)** | 36 µs | 27K ops/sec |
| **Sustained (with cache)** | **9.6 µs** | **104K ops/sec** |

> **Note**: Query caching provides ~3.9x throughput improvement for repeated queries.

### HTTP Load Testing

```bash
# Start server
gleam run

# Run wrk benchmark
wrk -t4 -c100 -d30s -s bench.lua http://localhost:8000/graphql

# Test different query types
QUERY_TYPE=simple wrk -t4 -c100 -d10s -s bench.lua http://localhost:8000/graphql
QUERY_TYPE=complex wrk -t4 -c100 -d10s -s bench.lua http://localhost:8000/graphql
```

---

## Library Comparison

### Benchmark Setup

All benchmarks use:
- **Same schema**: Users with id, name, email, role
- **Same data**: 3 users (Alice, Bob, Charlie)
- **Same queries**: simple, medium, with_args, complex
- **Same tool**: wrk with 4 threads, 100 connections

### Running Comparisons

```bash
cd benchmark
./run_comparison.sh
```

### Results Comparison

> **Note**: Results may vary based on hardware. Run benchmarks on your own machine for accurate comparisons.

#### Requests per Second (higher is better)

| Library | Language | Simple Query | Medium Query | Complex Query | Notes |
|---------|----------|--------------|--------------|---------------|-------|
| **Mochi (cached)** | Gleam/BEAM | ~100,000 | ~80,000 | ~30,000 | With query cache |
| **Mochi (no cache)** | Gleam/BEAM | ~50,000 | ~28,000 | ~12,000 | Without cache |
| Absinthe | Elixir/BEAM | ~15,000 | ~12,000 | ~8,000 | |
| graphql-js | Node.js | ~20,000 | ~15,000 | ~10,000 | |
| Juniper | Rust | ~80,000 | ~60,000 | ~40,000 | |

#### Latency p99 (lower is better)

| Library | Simple Query | Medium Query | Complex Query |
|---------|--------------|--------------|---------------|
| **Mochi** | ~2ms | ~4ms | ~10ms |
| Absinthe | ~8ms | ~12ms | ~18ms |
| graphql-js | ~6ms | ~10ms | ~15ms |
| Juniper | ~1ms | ~2ms | ~4ms |

### Analysis

**Mochi strengths:**
- 🚀 **3-4x faster than Absinthe** on the same BEAM runtime (up to 7x with caching)
- ⚡ **ETS query cache**: 3.9x throughput improvement for repeated queries
- 📦 **Lightweight**: Pure Gleam with minimal dependencies
- 🔒 **Type-safe**: Compile-time schema validation
- 🎯 **Code-first**: No separate schema files needed

**Trade-offs:**
- Newer library, smaller ecosystem
- Fewer built-in features than Absinthe (subscriptions via PubSub, not Phoenix Channels)

**When to use Mochi:**
- High-throughput GraphQL APIs
- Gleam/BEAM projects prioritizing performance
- Microservices where latency matters
- Projects wanting type-safe, code-first GraphQL

**When to use Absinthe:**
- Existing Phoenix/Elixir projects
- Need mature ecosystem (dataloader, subscriptions, etc.)
- Complex authorization requirements
- Team more familiar with Elixir

### Running Individual Benchmarks

#### Mochi (Gleam)
```bash
cd /path/to/mochi_wisp
gleam run &
wrk -t4 -c100 -d10s -s benchmark/mochi_bench.lua http://localhost:8000/graphql
```

#### Absinthe (Elixir)
```bash
cd benchmark/absinthe_bench
mix deps.get
mix run --no-halt &
wrk -t4 -c100 -d10s -s ../absinthe_bench.lua http://localhost:4000/graphql
```

#### graphql-js (Node.js)
```bash
cd benchmark/graphql_js_bench
npm install
node server.js &
wrk -t4 -c100 -d10s -s ../graphql_js_bench.lua http://localhost:4001/graphql
```

---

## Logging

The server logs all requests with correlation IDs:

```
INFO  POST /graphql
INFO  [req_A1B2C3D4] GraphQL request received
DEBUG [req_A1B2C3D4] Query: { users { id name email } }
DEBUG [req_A1B2C3D4] Executing query...
DEBUG [req_A1B2C3D4] Query executed successfully
INFO  [req_A1B2C3D4] Response status: 200
```

Set log level to Debug for verbose output:
```gleam
logging.set_level(logging.Debug)
```

## Project Structure

```
mochi_wisp/
├── src/
│   ├── mochi_wisp.gleam              # Entry point
│   ├── benchmark_runner.gleam        # Benchmark runner
│   ├── mochi_wisp/
│   │   ├── router.gleam              # HTTP routing
│   │   ├── graphql_handler.gleam     # GraphQL handler
│   │   ├── schema.gleam              # GraphQL schema
│   │   └── benchmark.gleam           # Benchmark suite
│   ├── mochi_wisp_ffi.erl            # Erlang FFI
│   ├── mochi_wisp_ffi.mjs            # JavaScript FFI
│   └── mochi_wisp_benchmark_ffi.erl  # Benchmark FFI
├── test/
│   └── mochi_wisp_test.gleam         # Tests (63 tests)
├── benchmark/
│   ├── run_comparison.sh             # Comparison runner
│   ├── mochi_bench.lua               # Mochi wrk script
│   ├── absinthe_bench/               # Absinthe benchmark
│   └── graphql_js_bench/             # graphql-js benchmark
├── bench.lua                         # Simple wrk script
└── gleam.toml
```
