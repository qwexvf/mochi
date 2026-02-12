#!/bin/bash
# run_comparison.sh
# Automated benchmark comparison for GraphQL libraries
#
# Usage: ./run_comparison.sh [duration]
#   duration: benchmark duration in seconds (default: 10)

set -e

DURATION=${1:-10}
THREADS=4
CONNECTIONS=100
RESULTS_FILE="benchmark_results.json"
SUMMARY_FILE="benchmark_summary.md"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  GraphQL Library Benchmark Comparison${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""
echo "Duration: ${DURATION}s per test"
echo "Threads: ${THREADS}"
echo "Connections: ${CONNECTIONS}"
echo ""

# Check for wrk
if ! command -v wrk &> /dev/null; then
    echo -e "${RED}Error: wrk is not installed${NC}"
    echo "Install with: sudo pacman -S wrk (Arch) or brew install wrk (macOS)"
    exit 1
fi

# Initialize results file
echo "[" > "$RESULTS_FILE"
FIRST_RESULT=true

# Query types to test
QUERY_TYPES=("simple" "medium" "with_args" "complex")

# Function to run benchmark
run_benchmark() {
    local library=$1
    local port=$2
    local script=$3

    for query_type in "${QUERY_TYPES[@]}"; do
        echo -e "${YELLOW}Testing ${library} - ${query_type}...${NC}"

        if [ "$FIRST_RESULT" = false ]; then
            echo "," >> "$RESULTS_FILE"
        fi
        FIRST_RESULT=false

        LIBRARY=$library QUERY_TYPE=$query_type wrk \
            -t${THREADS} \
            -c${CONNECTIONS} \
            -d${DURATION}s \
            -s "$script" \
            "http://localhost:${port}/graphql" 2>/dev/null >> "$RESULTS_FILE"
    done
}

# Benchmark Mochi
echo -e "${GREEN}=== Benchmarking Mochi (Gleam) ===${NC}"
if curl -s http://localhost:8000/health > /dev/null 2>&1; then
    run_benchmark "mochi" 8000 "mochi_bench.lua"
else
    echo -e "${RED}Mochi server not running on port 8000${NC}"
    echo "Start with: cd .. && gleam run"
fi

# Benchmark Absinthe (if running)
echo ""
echo -e "${GREEN}=== Benchmarking Absinthe (Elixir) ===${NC}"
if curl -s http://localhost:4000/health > /dev/null 2>&1; then
    run_benchmark "absinthe" 4000 "absinthe_bench.lua"
else
    echo -e "${YELLOW}Absinthe server not running on port 4000 (skipping)${NC}"
fi

# Benchmark graphql-js (if running)
echo ""
echo -e "${GREEN}=== Benchmarking graphql-js (Node.js) ===${NC}"
if curl -s http://localhost:4001/health > /dev/null 2>&1; then
    run_benchmark "graphql-js" 4001 "graphql_js_bench.lua"
else
    echo -e "${YELLOW}graphql-js server not running on port 4001 (skipping)${NC}"
fi

# Benchmark Juniper (if running)
echo ""
echo -e "${GREEN}=== Benchmarking Juniper (Rust) ===${NC}"
if curl -s http://localhost:4002/health > /dev/null 2>&1; then
    run_benchmark "juniper" 4002 "juniper_bench.lua"
else
    echo -e "${YELLOW}Juniper server not running on port 4002 (skipping)${NC}"
fi

# Close JSON array
echo "]" >> "$RESULTS_FILE"

# Generate summary
echo ""
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}  Generating Summary${NC}"
echo -e "${BLUE}========================================${NC}"

# Parse results and generate markdown table
cat > "$SUMMARY_FILE" << 'EOF'
# Benchmark Results

## Test Configuration
- Duration: 10s per test
- Threads: 4
- Connections: 100
- Queries: simple, medium, with_args, complex

## Results

| Library | Query | Requests/sec | p50 (ms) | p90 (ms) | p99 (ms) |
|---------|-------|--------------|----------|----------|----------|
EOF

# Parse JSON results (simple parsing)
if command -v jq &> /dev/null; then
    jq -r '.[] | "| \(.library) | \(.query) | \(.rps | floor) | \(.p50_ms | . * 1000 | floor / 1000) | \(.p90_ms | . * 1000 | floor / 1000) | \(.p99_ms | . * 1000 | floor / 1000) |"' "$RESULTS_FILE" >> "$SUMMARY_FILE" 2>/dev/null || true
fi

echo ""
echo -e "${GREEN}Results saved to:${NC}"
echo "  - $RESULTS_FILE (JSON)"
echo "  - $SUMMARY_FILE (Markdown)"
echo ""

# Print summary to console
if [ -f "$SUMMARY_FILE" ]; then
    cat "$SUMMARY_FILE"
fi
