#!/bin/sh
set -e

echo "========================================="
echo "GraphQL Server Benchmark Suite"
echo "========================================="
echo ""

# Wait for all servers to be ready
echo "Waiting for servers to start..."
sleep 15

# Test connectivity
echo ""
echo "Testing server connectivity..."
for server in "mochi:8000" "mercurius:4000" "apollo:4000" "yoga:4000" "graphql-js:4000"; do
  host=$(echo $server | cut -d: -f1)
  port=$(echo $server | cut -d: -f2)
  printf "  %s: " "$host"

  # Try up to 10 times
  for i in 1 2 3 4 5 6 7 8 9 10; do
    if curl -s -f -X POST "http://${host}:${port}/graphql" \
       -H "Content-Type: application/json" \
       -d '{"query":"{ __typename }"}' > /dev/null 2>&1; then
      echo "OK"
      break
    fi
    if [ $i -eq 10 ]; then
      echo "FAILED"
    else
      sleep 2
    fi
  done
done

echo ""

# Benchmark configuration
DURATION=30
CONNECTIONS=100

RESULTS_FILE="/results/benchmark-$(date +%Y%m%d-%H%M%S).md"
mkdir -p /results

cat > $RESULTS_FILE << 'EOF'
# GraphQL Server Benchmark Results

## Environment
- Docker containers with 2 CPU cores, 512MB RAM each
- Duration: 30s per test
- Connections: 100 concurrent

EOF

echo "Date: $(date)" >> $RESULTS_FILE
echo "" >> $RESULTS_FILE

# Define servers
SERVERS="mochi:8000 mercurius:4000 apollo:4000 yoga:4000 graphql-js:4000"

run_test() {
  local host=$1
  local port=$2
  local query=$3

  local url="http://${host}:${port}/graphql"

  # Warmup (5 seconds)
  autocannon -d 5 -c 50 -m POST \
    -H "Content-Type: application/json" \
    -b "$query" \
    "$url" > /dev/null 2>&1 || true

  # Actual benchmark - capture full JSON output
  local tmpfile="/tmp/bench_${host}.json"
  autocannon -d $DURATION -c $CONNECTIONS -m POST \
    -H "Content-Type: application/json" \
    -b "$query" \
    --json "$url" > "$tmpfile" 2>/dev/null || true

  # Extract requests.average from JSON using grep and sed
  local avg_rps=$(cat "$tmpfile" | tr ',' '\n' | grep '"average"' | head -1 | sed 's/.*"average":\([0-9.]*\).*/\1/' | cut -d. -f1)

  if [ -z "$avg_rps" ] || [ "$avg_rps" = "0" ]; then
    # Fallback: try to get from the text output
    avg_rps=$(autocannon -d 10 -c $CONNECTIONS -m POST \
      -H "Content-Type: application/json" \
      -b "$query" \
      "$url" 2>&1 | grep "Req/Sec" | tail -1 | awk '{print $5}' | tr -d ',')
  fi

  if [ -z "$avg_rps" ]; then
    avg_rps="N/A"
  fi

  echo "$avg_rps"
  rm -f "$tmpfile"
}

# Simple Query
echo "## Simple Query: { users { id name } }" >> $RESULTS_FILE
echo "" >> $RESULTS_FILE
echo "| Server | Req/sec |" >> $RESULTS_FILE
echo "|--------|---------|" >> $RESULTS_FILE

QUERY='{"query": "{ users { id name } }"}'
echo "Running Simple Query benchmark..."

for server in $SERVERS; do
  host=$(echo $server | cut -d: -f1)
  port=$(echo $server | cut -d: -f2)
  printf "  Benchmarking %s... " "$host"
  rps=$(run_test "$host" "$port" "$QUERY")
  echo "| $host | $rps |" >> $RESULTS_FILE
  echo "$rps req/s"
done

echo "" >> $RESULTS_FILE

# Medium Query
echo "## Medium Query: { users { id name email posts { id title } } }" >> $RESULTS_FILE
echo "" >> $RESULTS_FILE
echo "| Server | Req/sec |" >> $RESULTS_FILE
echo "|--------|---------|" >> $RESULTS_FILE

QUERY='{"query": "{ users { id name email posts { id title } } }"}'
echo ""
echo "Running Medium Query benchmark..."

for server in $SERVERS; do
  host=$(echo $server | cut -d: -f1)
  port=$(echo $server | cut -d: -f2)
  printf "  Benchmarking %s... " "$host"
  rps=$(run_test "$host" "$port" "$QUERY")
  echo "| $host | $rps |" >> $RESULTS_FILE
  echo "$rps req/s"
done

echo "" >> $RESULTS_FILE

# Complex Query
echo "## Complex Query" >> $RESULTS_FILE
echo "" >> $RESULTS_FILE
echo "| Server | Req/sec |" >> $RESULTS_FILE
echo "|--------|---------|" >> $RESULTS_FILE

QUERY='{"query": "{ users { id name posts { id title comments { id body author { name } } } } }"}'
echo ""
echo "Running Complex Query benchmark..."

for server in $SERVERS; do
  host=$(echo $server | cut -d: -f1)
  port=$(echo $server | cut -d: -f2)
  printf "  Benchmarking %s... " "$host"
  rps=$(run_test "$host" "$port" "$QUERY")
  echo "| $host | $rps |" >> $RESULTS_FILE
  echo "$rps req/s"
done

echo ""
echo "========================================="
echo "Benchmark Complete!"
echo "========================================="
echo ""
cat $RESULTS_FILE
