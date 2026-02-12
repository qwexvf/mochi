// mochi_wisp_ffi.mjs
import { Ok, Error, None, Some } from "./gleam.mjs";
import { GraphQLRequest } from "./mochi_wisp/graphql_handler.mjs";

// Counter for generating unique request IDs
let requestCounter = 0;

// Generate a unique request ID for log correlation
export function generate_request_id() {
  requestCounter++;
  const timestamp = Date.now().toString(16).slice(-8);
  const counter = requestCounter.toString(16).padStart(4, '0');
  return `req_${timestamp}${counter}`;
}

// Parse GraphQL request from JSON body
// Returns: Ok(GraphQLRequest) | Error(String)
export function parse_graphql_request_full(body) {
  try {
    const json = JSON.parse(body);
    const query = json.query;

    if (typeof query !== 'string') {
      return new Error("Missing required field: 'query'");
    }

    const variables = json.variables && typeof json.variables === 'object' && !Array.isArray(json.variables)
      ? new Some(json.variables)
      : new None();

    const operationName = typeof json.operationName === 'string'
      ? new Some(json.operationName)
      : new None();

    return new Ok(new GraphQLRequest(query, variables, operationName));
  } catch (e) {
    return new Error("Invalid JSON in request body");
  }
}

// Convert Dynamic value to gleam_json Json type
// In JavaScript, gleam_json Json values are native JS values that get JSON.stringify'd
export function dynamic_to_json(data) {
  return toJsonValue(data);
}

function toJsonValue(value) {
  if (value === null || value === undefined) {
    return null;
  }
  if (typeof value === 'boolean') {
    return value;
  }
  if (typeof value === 'string') {
    return value;
  }
  if (typeof value === 'number') {
    return value;
  }
  if (Array.isArray(value)) {
    return value.map(toJsonValue);
  }
  if (typeof value === 'object') {
    const result = {};
    for (const [k, v] of Object.entries(value)) {
      result[k] = toJsonValue(v);
    }
    return result;
  }
  return null;
}

// Get a field from an object safely, returning Option(Dynamic)
// Returns: Some(value) | None
export function get_field_safe(data, field) {
  if (data && typeof data === 'object' && field in data) {
    return new Some(data[field]);
  }
  return new None();
}
