# Node.js GraphQL Benchmark Suite

Benchmarks comparing Mochi (Gleam) against popular Node.js GraphQL implementations.

## Servers Included

| Server | Port | Description |
|--------|------|-------------|
| graphql-js | 4001 | Pure graphql-js with graphql-http (minimal) |
| Apollo Server | 4002 | Full-featured Apollo Server 4 |
| Mercurius | 4003 | High-performance Fastify plugin with JIT |
| GraphQL Yoga | 4004 | Modern, feature-rich server |
| Mochi (Gleam) | 8000 | BEAM-powered GraphQL (this project) |

## Quick Start

```bash
# Install dependencies
npm install

# Start all Node.js servers (in separate terminals or use tmux)
npm run start:graphql-js    # Terminal 1
npm run start:apollo        # Terminal 2
npm run start:mercurius     # Terminal 3
npm run start:yoga          # Terminal 4

# Start Mochi server (from project root)
cd ../.. && gleam run       # Terminal 5

# Run benchmarks
npm run bench               # Full benchmark (10s each)
npm run bench:quick         # Quick benchmark (5s each)
```

## Query Types Tested

1. **Simple Query** - Basic field selection
2. **Medium Query** - One level of nesting
3. **Complex Query** - Deep nesting with relationships
4. **Query with Arguments** - Parameter handling
5. **Pagination Query** - Connection pattern
6. **Mutation** - Write operations

## Expected Results

Based on typical performance characteristics:

- **Mercurius** tends to be fastest for raw throughput (Fastify + JIT)
- **graphql-js** has low overhead as the reference implementation
- **Apollo Server** adds features at some performance cost
- **Mochi** leverages BEAM VM for concurrent request handling

## Schema

The benchmark uses a realistic schema with:

- Users (with roles: ADMIN, MEMBER, GUEST)
- Posts (with status: DRAFT, PUBLISHED, ARCHIVED)
- Comments
- Relationships between all entities

See `schema.js` for the full schema definition.

## Interpreting Results

- **Throughput (req/s)**: Higher is better
- **Avg Latency**: Lower is better
- **P99 Latency**: Lower is better (tail latency)

Note: Mochi's advantage comes from:
1. Query caching (repeated queries are very fast)
2. BEAM concurrency (handles many connections well)
3. Process isolation (one bad request doesn't affect others)
