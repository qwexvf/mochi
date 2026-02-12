# GraphQL Library Benchmarks

This directory contains benchmark scripts for comparing Mochi with other GraphQL libraries.

## Quick Comparison

Run the automated comparison script:

```bash
./run_comparison.sh
```

## Individual Benchmarks

### Mochi (Gleam)

```bash
cd /path/to/mochi_wisp
gleam run &
wrk -t4 -c100 -d10s -s ../benchmark/mochi_bench.lua http://localhost:8000/graphql
```

### Absinthe (Elixir)

```bash
cd benchmark/absinthe_bench
mix deps.get
mix phx.server &
wrk -t4 -c100 -d10s -s ../absinthe_bench.lua http://localhost:4000/graphql
```

### graphql-js (Node.js)

```bash
cd benchmark/graphql_js_bench
npm install
node server.js &
wrk -t4 -c100 -d10s -s ../graphql_js_bench.lua http://localhost:4001/graphql
```

## Schema

All benchmarks use an equivalent schema:

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

## Test Queries

| Query | Description |
|-------|-------------|
| simple | `{ users { id name } }` |
| medium | `{ users { id name email role } }` |
| with_args | `{ user(id: "1") { id name email } }` |
| complex | `{ users { id name } user(id: "1") { email role } }` |
