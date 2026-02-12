-- graphql_js_bench.lua
-- wrk benchmark script for graphql-js (Node.js)

wrk.method = "POST"
wrk.headers["Content-Type"] = "application/json"
wrk.headers["Accept"] = "application/json"

local queries = {
  simple = '{"query": "{ users { id name } }"}',
  medium = '{"query": "{ users { id name email role } }"}',
  with_args = '{"query": "{ user(id: \\"1\\") { id name email } }"}',
  complex = '{"query": "{ users { id name } user(id: \\"1\\") { email role } }"}',
  variables = '{"query": "query GetUser($id: ID!) { user(id: $id) { id name email } }", "variables": {"id": "2"}}',
}

local query_type = os.getenv("QUERY_TYPE") or "medium"
wrk.body = queries[query_type] or queries.medium

local results = {
  success = 0,
  errors = 0,
  graphql_errors = 0,
}

function response(status, headers, body)
  if status == 200 then
    results.success = results.success + 1
    if string.find(body, '"errors"') then
      results.graphql_errors = results.graphql_errors + 1
    end
  else
    results.errors = results.errors + 1
  end
end

function done(summary, latency, requests)
  local total = results.success + results.errors
  local rps = total / (summary.duration / 1000000)
  local library = os.getenv("LIBRARY") or "graphql-js"

  io.write(string.format(
    '{"library":"%s","query":"%s","requests":%d,"rps":%.2f,"p50_ms":%.3f,"p90_ms":%.3f,"p99_ms":%.3f,"errors":%d,"graphql_errors":%d}',
    library,
    query_type,
    total,
    rps,
    latency:percentile(50)/1000,
    latency:percentile(90)/1000,
    latency:percentile(99)/1000,
    results.errors,
    results.graphql_errors
  ))
end
