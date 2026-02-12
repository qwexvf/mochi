#!/bin/bash
# Run benchmarks from host against Docker containers

set -e

DURATION=20
CONNECTIONS=100

echo "========================================="
echo "GraphQL Server Benchmark (Docker)"
echo "========================================="
echo "Duration: ${DURATION}s | Connections: ${CONNECTIONS}"
echo ""

QUERY_SIMPLE='{"query": "{ users { id name } }"}'

declare -A SERVERS=(
  ["mochi"]=3001
  ["mercurius"]=3002
  ["apollo"]=3003
  ["yoga"]=3004
  ["graphql-js"]=3005
)

echo "## Simple Query: { users { id name } }"
echo ""
echo "| Server | Req/sec | Latency (avg) |"
echo "|--------|---------|---------------|"

for name in mochi mercurius apollo yoga graphql-js; do
  port=${SERVERS[$name]}
  url="http://localhost:${port}/graphql"

  # Warmup
  npx autocannon -d 3 -c 50 -m POST \
    -H "Content-Type: application/json" \
    -b "$QUERY_SIMPLE" \
    "$url" > /dev/null 2>&1 || true

  # Benchmark
  result=$(npx autocannon -d $DURATION -c $CONNECTIONS -m POST \
    -H "Content-Type: application/json" \
    -b "$QUERY_SIMPLE" \
    --json "$url" 2>/dev/null)

  # Extract requests.average (not latency.average)
  avg_rps=$(echo "$result" | grep -o '"requests":{[^}]*}' | grep -o '"average":[0-9.]*' | cut -d: -f2 | xargs printf "%.0f")
  # Extract latency.mean
  avg_lat=$(echo "$result" | grep -o '"latency":{[^}]*}' | grep -o '"mean":[0-9.]*' | cut -d: -f2 | xargs printf "%.2f")

  echo "| $name | $avg_rps | ${avg_lat}ms |"
done

echo ""
echo "========================================="
echo ""

QUERY_MEDIUM='{"query": "{ users { id name email posts { id title } } }"}'

echo "## Medium Query: { users { ... posts { ... } } }"
echo ""
echo "| Server | Req/sec | Latency (avg) |"
echo "|--------|---------|---------------|"

for name in mochi mercurius apollo yoga graphql-js; do
  port=${SERVERS[$name]}
  url="http://localhost:${port}/graphql"

  # Warmup
  npx autocannon -d 3 -c 50 -m POST \
    -H "Content-Type: application/json" \
    -b "$QUERY_MEDIUM" \
    "$url" > /dev/null 2>&1 || true

  # Benchmark
  result=$(npx autocannon -d $DURATION -c $CONNECTIONS -m POST \
    -H "Content-Type: application/json" \
    -b "$QUERY_MEDIUM" \
    --json "$url" 2>/dev/null)

  avg_rps=$(echo "$result" | grep -o '"requests":{[^}]*}' | grep -o '"average":[0-9.]*' | cut -d: -f2 | xargs printf "%.0f")
  avg_lat=$(echo "$result" | grep -o '"latency":{[^}]*}' | grep -o '"mean":[0-9.]*' | cut -d: -f2 | xargs printf "%.2f")

  echo "| $name | $avg_rps | ${avg_lat}ms |"
done

echo ""
echo "Done!"
