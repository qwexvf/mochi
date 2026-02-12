-- bench.lua
-- wrk script for GraphQL HTTP load testing
--
-- Usage:
--   Start the server:  gleam run
--   Run the benchmark: wrk -t4 -c100 -d30s -s bench.lua http://localhost:8000/graphql
--
-- Options:
--   -t4    = 4 threads
--   -c100  = 100 connections
--   -d30s  = 30 second duration

wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"

-- Different query types for testing
local queries = {
  -- Simple query
  simple = '{"query": "{ users { id name } }"}',

  -- Medium complexity
  medium = '{"query": "{ users { id name email role } }"}',

  -- Query with arguments
  with_args = '{"query": "{ user(id: \\"1\\") { id name email } }"}',

  -- Multiple root fields
  complex = '{"query": "{ users { id name } user(id: \\"1\\") { email role } }"}',

  -- With variables
  variables = '{"query": "query GetUser($id: ID!) { user(id: $id) { id name email } }", "variables": {"id": "2"}}',
}

-- Select query type via environment or default to medium
local query_type = os.getenv("QUERY_TYPE") or "medium"
wrk.body = queries[query_type] or queries.medium

-- Track response statistics
local responses = {
  success = 0,
  errors = 0,
  graphql_errors = 0,
}

function response(status, headers, body)
  if status == 200 then
    responses.success = responses.success + 1
    -- Check for GraphQL errors in response
    if string.find(body, '"errors"') then
      responses.graphql_errors = responses.graphql_errors + 1
    end
  else
    responses.errors = responses.errors + 1
  end
end

function done(summary, latency, requests)
  local total = responses.success + responses.errors

  io.write("\n")
  io.write("==============================================\n")
  io.write("  GraphQL Benchmark Results\n")
  io.write("==============================================\n")
  io.write(string.format("Query type:        %s\n", query_type))
  io.write(string.format("Total requests:    %d\n", total))
  io.write(string.format("HTTP 200:          %d (%.1f%%)\n", responses.success, responses.success/total*100))
  io.write(string.format("HTTP errors:       %d\n", responses.errors))
  io.write(string.format("GraphQL errors:    %d\n", responses.graphql_errors))
  io.write("\n")
  io.write("Latency percentiles:\n")
  io.write(string.format("  50%%:  %.2f ms\n", latency:percentile(50)/1000))
  io.write(string.format("  90%%:  %.2f ms\n", latency:percentile(90)/1000))
  io.write(string.format("  99%%:  %.2f ms\n", latency:percentile(99)/1000))
  io.write("==============================================\n")
end
