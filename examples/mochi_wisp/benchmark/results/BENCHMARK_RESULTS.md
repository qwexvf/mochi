# Mochi GraphQL Benchmark Results

## Test Environment

- **Platform**: Docker containers
- **Resources**: 2 CPU cores, 512MB RAM per container
- **Test Duration**: 20 seconds per benchmark
- **Concurrent Connections**: 100
- **Tool**: autocannon

## Servers Tested

| Server | Language/Runtime | Notes |
|--------|-----------------|-------|
| Mochi | Gleam/Erlang BEAM | Code-first GraphQL |
| Mercurius | Node.js + Fastify | graphql-jit enabled |
| Apollo Server | Node.js + Express | Industry standard |
| GraphQL Yoga | Node.js | Modern GraphQL server |
| graphql-js | Node.js + Express | Reference implementation |

## Results

### Simple Query
```graphql
{ users { id name } }
```

| Server | Req/sec | Latency (avg) | vs Mochi |
|--------|---------|---------------|----------|
| **Mercurius** | 27,952 | 3.17ms | 2.56x faster |
| **Mochi** | 10,911 | 8.66ms | baseline |
| Yoga | 7,767 | 12.38ms | 0.71x |
| Apollo | 5,203 | 18.72ms | 0.48x |
| graphql-js | 3,552 | 27.65ms | 0.33x |

### Medium Query
```graphql
{ users { id name email posts { id title } } }
```

| Server | Req/sec | Latency (avg) | vs Mochi |
|--------|---------|---------------|----------|
| **Mercurius** | 26,007 | 3.38ms | 2.55x faster |
| **Mochi** | 10,201 | 9.29ms | baseline |
| Yoga | 6,938 | 13.91ms | 0.68x |
| Apollo | 4,247 | 23.04ms | 0.42x |
| graphql-js | 2,827 | 34.86ms | 0.28x |

## Analysis

### Mochi Performance

- **Ranks #2** among all tested GraphQL servers
- **2.1x faster than Apollo Server** (the most popular GraphQL server)
- **3.1x faster than graphql-js** (the reference implementation)
- **Only behind Mercurius** due to graphql-jit compilation

### Why Mercurius is Faster

Mercurius uses [graphql-jit](https://github.com/zalando-incubator/graphql-jit) which:
1. Compiles GraphQL queries into optimized JavaScript functions
2. Eliminates AST traversal on subsequent requests
3. Inlines field resolvers directly

### Mochi Optimizations Applied

1. **O(n) Binary Lexer** - Erlang binary pattern matching instead of string concatenation
2. **Cursor-based Parser** - O(1) token access instead of list indexing
3. **ETS Query Cache** - High-performance concurrent caching
4. **Executor Optimizations**:
   - Set-based argument lookup (O(1) vs O(n))
   - Single-pass result aggregation
   - Reduced allocations

### BEAM VM Advantages

Despite being ~2.5x slower than Mercurius on raw throughput, Mochi/BEAM offers:
- **Fault tolerance** - Process isolation, supervisor trees
- **Low latency tail** - Consistent response times under load
- **Horizontal scaling** - Distributed Erlang for clustering
- **Hot code reloading** - Zero-downtime deployments

## Running the Benchmark

```bash
cd examples/mochi_wisp/benchmark

# Start all servers
docker compose up -d

# Run benchmark from host
./run-host-bench.sh

# Stop servers
docker compose down
```
