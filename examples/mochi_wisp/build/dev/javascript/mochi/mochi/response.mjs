import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
  isEqual,
} from "../gleam.mjs";
import * as $error from "../mochi/error.mjs";
import * as $executor from "../mochi/executor.mjs";
import * as $json from "../mochi/json.mjs";
import * as $types from "../mochi/types.mjs";

export class GraphQLResponse extends $CustomType {
  constructor(data, errors, extensions) {
    super();
    this.data = data;
    this.errors = errors;
    this.extensions = extensions;
  }
}
export const GraphQLResponse$GraphQLResponse = (data, errors, extensions) =>
  new GraphQLResponse(data, errors, extensions);
export const GraphQLResponse$isGraphQLResponse = (value) =>
  value instanceof GraphQLResponse;
export const GraphQLResponse$GraphQLResponse$data = (value) => value.data;
export const GraphQLResponse$GraphQLResponse$0 = (value) => value.data;
export const GraphQLResponse$GraphQLResponse$errors = (value) => value.errors;
export const GraphQLResponse$GraphQLResponse$1 = (value) => value.errors;
export const GraphQLResponse$GraphQLResponse$extensions = (value) =>
  value.extensions;
export const GraphQLResponse$GraphQLResponse$2 = (value) => value.extensions;

/**
 * Create a successful response with data
 */
export function success(data) {
  return new GraphQLResponse(new Some(data), new None(), new None());
}

/**
 * Create an error-only response
 */
export function failure(errors) {
  return new GraphQLResponse(new None(), new Some(errors), new None());
}

/**
 * Create a partial response with data and errors
 */
export function partial(data, errors) {
  return new GraphQLResponse(new Some(data), new Some(errors), new None());
}

/**
 * Add extensions to a response
 */
export function with_extensions(response, extensions) {
  return new GraphQLResponse(
    response.data,
    response.errors,
    new Some(extensions),
  );
}

/**
 * Add a single extension value
 */
export function with_extension(response, key, value) {
  let _block;
  let $ = response.extensions;
  if ($ instanceof Some) {
    let ext = $[0];
    _block = $dict.insert(ext, key, value);
  } else {
    _block = $dict.from_list(toList([[key, value]]));
  }
  let extensions = _block;
  return new GraphQLResponse(
    response.data,
    response.errors,
    new Some(extensions),
  );
}

/**
 * Add tracing data to response extensions
 */
export function with_tracing(response, start_time, end_time) {
  let tracing = $types.to_dynamic(
    $dict.from_list(
      toList([
        ["version", $types.to_dynamic(1)],
        ["startTime", $types.to_dynamic(start_time)],
        ["endTime", $types.to_dynamic(end_time)],
        ["duration", $types.to_dynamic(end_time - start_time)],
      ]),
    ),
  );
  return with_extension(response, "tracing", tracing);
}

/**
 * Convert a GraphQLResponse to a Dynamic representation for JSON serialization
 */
export function to_dynamic(response) {
  let parts = toList([]);
  let parts$1 = listPrepend(
    [
      "data",
      (() => {
        let $ = response.data;
        if ($ instanceof Some) {
          let d = $[0];
          return d;
        } else {
          return $types.to_dynamic(undefined);
        }
      })(),
    ],
    parts,
  );
  let _block;
  let $ = response.errors;
  if ($ instanceof Some) {
    let errors = $[0];
    _block = listPrepend(["errors", $error.errors_to_dynamic(errors)], parts$1);
  } else {
    _block = parts$1;
  }
  let parts$2 = _block;
  let _block$1;
  let $1 = response.extensions;
  if ($1 instanceof Some) {
    let ext = $1[0];
    _block$1 = listPrepend(["extensions", $types.to_dynamic(ext)], parts$2);
  } else {
    _block$1 = parts$2;
  }
  let parts$3 = _block$1;
  return $types.to_dynamic($dict.from_list(parts$3));
}

/**
 * Serialize response to JSON string (requires external JSON encoder)
 * This returns the Dynamic representation - use with a JSON library
 */
export function serialize(response) {
  return to_dynamic(response);
}

/**
 * Convert a GraphQLResponse to a JSON string
 */
export function to_json(response) {
  let _pipe = response;
  let _pipe$1 = to_dynamic(_pipe);
  return $json.encode(_pipe$1);
}

/**
 * Convert a GraphQLResponse to a pretty-printed JSON string
 */
export function to_json_pretty(response) {
  let _pipe = response;
  let _pipe$1 = to_dynamic(_pipe);
  return $json.encode_pretty(_pipe$1, 2);
}

/**
 * Convert an ExecutionError to a GraphQLError
 */
export function execution_error_to_graphql_error(err) {
  if (err instanceof $executor.ValidationError) {
    let message = err.message;
    let path = err.path;
    let _pipe = $error.error_at(message, path);
    return $error.with_category(_pipe, new $error.ValidationErrorCategory());
  } else if (err instanceof $executor.ResolverError) {
    let message = err.message;
    let path = err.path;
    let _pipe = $error.error_at(message, path);
    return $error.with_category(_pipe, new $error.ResolverErrorCategory());
  } else if (err instanceof $executor.TypeError) {
    let message = err.message;
    let path = err.path;
    let _pipe = $error.error_at(message, path);
    return $error.with_category(_pipe, new $error.TypeErrorCategory());
  } else {
    let message = err.message;
    let path = err.path;
    let _pipe = $error.error_at(message, path);
    let _pipe$1 = $error.with_code(_pipe, "NULL_VALUE_ERROR");
    return $error.with_category(_pipe$1, new $error.ResolverErrorCategory());
  }
}

/**
 * Create a response from an ExecutionResult
 */
export function from_execution_result(result) {
  let _block;
  let $ = result.errors;
  if ($ instanceof $Empty) {
    _block = new None();
  } else {
    let errs = $;
    _block = new Some($list.map(errs, execution_error_to_graphql_error));
  }
  let errors = _block;
  return new GraphQLResponse(result.data, errors, new None());
}

/**
 * Check if response has errors
 */
export function has_errors(response) {
  let $ = response.errors;
  if ($ instanceof Some) {
    let errors = $[0];
    return !isEqual(errors, toList([]));
  } else {
    return false;
  }
}

/**
 * Check if response has data
 */
export function has_data(response) {
  let $ = response.data;
  if ($ instanceof Some) {
    return true;
  } else {
    return false;
  }
}

/**
 * Get error count
 */
export function error_count(response) {
  let $ = response.errors;
  if ($ instanceof Some) {
    let errors = $[0];
    return $list.length(errors);
  } else {
    return 0;
  }
}

/**
 * Check if response is successful (has data, no errors)
 */
export function is_success(response) {
  return has_data(response) && !has_errors(response);
}

/**
 * Check if response is partial (has data and errors)
 */
export function is_partial(response) {
  return has_data(response) && has_errors(response);
}

function int_to_string(n) {
  if (n === 0) {
    return "0";
  } else if (n === 1) {
    return "1";
  } else if (n === 2) {
    return "2";
  } else if (n === 3) {
    return "3";
  } else if (n === 4) {
    return "4";
  } else if (n === 5) {
    return "5";
  } else if (n === 6) {
    return "6";
  } else if (n === 7) {
    return "7";
  } else if (n === 8) {
    return "8";
  } else if (n === 9) {
    return "9";
  } else {
    let $ = n < 0;
    if ($) {
      return "-" + int_to_string(- n);
    } else {
      return int_to_string(globalThis.Math.trunc(n / 10)) + int_to_string(
        n % 10,
      );
    }
  }
}

/**
 * Format response for debugging/logging
 */
export function format(response) {
  let _block;
  let $ = response.data;
  if ($ instanceof Some) {
    _block = "data: <present>";
  } else {
    _block = "data: null";
  }
  let data_str = _block;
  let _block$1;
  let $1 = response.errors;
  if ($1 instanceof Some) {
    let errors = $1[0];
    let count = $list.length(errors);
    if (count === 1) {
      _block$1 = "errors: [1 error]";
    } else {
      let n = count;
      _block$1 = ("errors: [" + int_to_string(n)) + " errors]";
    }
  } else {
    _block$1 = "errors: none";
  }
  let errors_str = _block$1;
  let _block$2;
  let $2 = response.extensions;
  if ($2 instanceof Some) {
    _block$2 = "extensions: <present>";
  } else {
    _block$2 = "extensions: none";
  }
  let ext_str = _block$2;
  return ((((("GraphQLResponse { " + data_str) + ", ") + errors_str) + ", ") + ext_str) + " }";
}
