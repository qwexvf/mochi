#!/bin/bash
# quick_bench.sh
# Quick benchmark for Mochi only
#
# Usage: ./quick_bench.sh [duration]

set -e

DURATION=${1:-5}

echo "Quick Mochi Benchmark (${DURATION}s per query type)"
echo "=============================================="
echo ""

# Check if server is running
if ! curl -s http://localhost:8000/health > /dev/null 2>&1; then
    echo "Error: Mochi server not running on port 8000"
    echo "Start with: gleam run"
    exit 1
fi

for query in simple medium with_args complex; do
    echo -n "Testing $query... "
    result=$(QUERY_TYPE=$query wrk -t2 -c50 -d${DURATION}s -s mochi_bench.lua http://localhost:8000/graphql 2>/dev/null)

    # Extract RPS from JSON output
    rps=$(echo "$result" | grep -o '"rps":[0-9.]*' | cut -d: -f2)
    p50=$(echo "$result" | grep -o '"p50_ms":[0-9.]*' | cut -d: -f2)

    printf "%.0f req/sec (p50: %.2f ms)\n" "$rps" "$p50"
done

echo ""
echo "Done!"
