import * as $dict from "../../gleam_stdlib/gleam/dict.mjs";
import * as $dynamic from "../../gleam_stdlib/gleam/dynamic.mjs";
import * as $int from "../../gleam_stdlib/gleam/int.mjs";
import * as $list from "../../gleam_stdlib/gleam/list.mjs";
import * as $option from "../../gleam_stdlib/gleam/option.mjs";
import { None, Some } from "../../gleam_stdlib/gleam/option.mjs";
import * as $string from "../../gleam_stdlib/gleam/string.mjs";
import {
  toList,
  Empty as $Empty,
  prepend as listPrepend,
  CustomType as $CustomType,
} from "../gleam.mjs";
import * as $types from "../mochi/types.mjs";

export class Location extends $CustomType {
  constructor(line, column) {
    super();
    this.line = line;
    this.column = column;
  }
}
export const Location$Location = (line, column) => new Location(line, column);
export const Location$isLocation = (value) => value instanceof Location;
export const Location$Location$line = (value) => value.line;
export const Location$Location$0 = (value) => value.line;
export const Location$Location$column = (value) => value.column;
export const Location$Location$1 = (value) => value.column;

export class FieldSegment extends $CustomType {
  constructor(name) {
    super();
    this.name = name;
  }
}
export const PathSegment$FieldSegment = (name) => new FieldSegment(name);
export const PathSegment$isFieldSegment = (value) =>
  value instanceof FieldSegment;
export const PathSegment$FieldSegment$name = (value) => value.name;
export const PathSegment$FieldSegment$0 = (value) => value.name;

export class IndexSegment extends $CustomType {
  constructor(index) {
    super();
    this.index = index;
  }
}
export const PathSegment$IndexSegment = (index) => new IndexSegment(index);
export const PathSegment$isIndexSegment = (value) =>
  value instanceof IndexSegment;
export const PathSegment$IndexSegment$index = (value) => value.index;
export const PathSegment$IndexSegment$0 = (value) => value.index;

export class GraphQLError extends $CustomType {
  constructor(message, locations, path, extensions) {
    super();
    this.message = message;
    this.locations = locations;
    this.path = path;
    this.extensions = extensions;
  }
}
export const GraphQLError$GraphQLError = (message, locations, path, extensions) =>
  new GraphQLError(message, locations, path, extensions);
export const GraphQLError$isGraphQLError = (value) =>
  value instanceof GraphQLError;
export const GraphQLError$GraphQLError$message = (value) => value.message;
export const GraphQLError$GraphQLError$0 = (value) => value.message;
export const GraphQLError$GraphQLError$locations = (value) => value.locations;
export const GraphQLError$GraphQLError$1 = (value) => value.locations;
export const GraphQLError$GraphQLError$path = (value) => value.path;
export const GraphQLError$GraphQLError$2 = (value) => value.path;
export const GraphQLError$GraphQLError$extensions = (value) => value.extensions;
export const GraphQLError$GraphQLError$3 = (value) => value.extensions;

export class ValidationErrorCategory extends $CustomType {}
export const ErrorCategory$ValidationErrorCategory = () =>
  new ValidationErrorCategory();
export const ErrorCategory$isValidationErrorCategory = (value) =>
  value instanceof ValidationErrorCategory;

export class ResolverErrorCategory extends $CustomType {}
export const ErrorCategory$ResolverErrorCategory = () =>
  new ResolverErrorCategory();
export const ErrorCategory$isResolverErrorCategory = (value) =>
  value instanceof ResolverErrorCategory;

export class TypeErrorCategory extends $CustomType {}
export const ErrorCategory$TypeErrorCategory = () => new TypeErrorCategory();
export const ErrorCategory$isTypeErrorCategory = (value) =>
  value instanceof TypeErrorCategory;

export class AuthenticationErrorCategory extends $CustomType {}
export const ErrorCategory$AuthenticationErrorCategory = () =>
  new AuthenticationErrorCategory();
export const ErrorCategory$isAuthenticationErrorCategory = (value) =>
  value instanceof AuthenticationErrorCategory;

export class AuthorizationErrorCategory extends $CustomType {}
export const ErrorCategory$AuthorizationErrorCategory = () =>
  new AuthorizationErrorCategory();
export const ErrorCategory$isAuthorizationErrorCategory = (value) =>
  value instanceof AuthorizationErrorCategory;

export class InternalErrorCategory extends $CustomType {}
export const ErrorCategory$InternalErrorCategory = () =>
  new InternalErrorCategory();
export const ErrorCategory$isInternalErrorCategory = (value) =>
  value instanceof InternalErrorCategory;

export class UserInputErrorCategory extends $CustomType {}
export const ErrorCategory$UserInputErrorCategory = () =>
  new UserInputErrorCategory();
export const ErrorCategory$isUserInputErrorCategory = (value) =>
  value instanceof UserInputErrorCategory;

/**
 * Create a simple error with just a message
 */
export function error(message) {
  return new GraphQLError(message, new None(), new None(), new None());
}

/**
 * Create an error with a path
 */
export function error_with_path(message, path) {
  return new GraphQLError(message, new None(), new Some(path), new None());
}

/**
 * Create an error from string path segments (convenience)
 */
export function error_at(message, path) {
  return new GraphQLError(
    message,
    new None(),
    new Some($list.map(path, (var0) => { return new FieldSegment(var0); })),
    new None(),
  );
}

/**
 * Add a location to an error
 */
export function at_location(err, line, column) {
  let new_loc = new Location(line, column);
  let _block;
  let $ = err.locations;
  if ($ instanceof Some) {
    let locs = $[0];
    _block = new Some($list.append(locs, toList([new_loc])));
  } else {
    _block = new Some(toList([new_loc]));
  }
  let locations = _block;
  return new GraphQLError(err.message, locations, err.path, err.extensions);
}

/**
 * Add locations to an error
 */
export function with_locations(err, locations) {
  return new GraphQLError(
    err.message,
    new Some(locations),
    err.path,
    err.extensions,
  );
}

/**
 * Set the path on an error
 */
export function with_path(err, path) {
  return new GraphQLError(
    err.message,
    err.locations,
    new Some(path),
    err.extensions,
  );
}

/**
 * Add an extension to an error
 */
export function with_extension(err, key, value) {
  let _block;
  let $ = err.extensions;
  if ($ instanceof Some) {
    let ext = $[0];
    _block = $dict.insert(ext, key, value);
  } else {
    _block = $dict.from_list(toList([[key, value]]));
  }
  let extensions = _block;
  return new GraphQLError(
    err.message,
    err.locations,
    err.path,
    new Some(extensions),
  );
}

/**
 * Set multiple extensions on an error
 */
export function with_extensions(err, extensions) {
  return new GraphQLError(
    err.message,
    err.locations,
    err.path,
    new Some(extensions),
  );
}

/**
 * Set error code in extensions
 */
export function with_code(err, code) {
  return with_extension(err, "code", $types.to_dynamic(code));
}

function path_segment_to_dynamic(segment) {
  if (segment instanceof FieldSegment) {
    let name = segment.name;
    return $types.to_dynamic(name);
  } else {
    let index = segment.index;
    return $types.to_dynamic(index);
  }
}

/**
 * Convert a GraphQLError to a Dynamic representation for JSON serialization
 */
export function to_dynamic(err) {
  let base = toList([["message", $types.to_dynamic(err.message)]]);
  let _block;
  let $ = err.locations;
  if ($ instanceof Some) {
    let locs = $[0];
    _block = listPrepend(
      [
        "locations",
        $types.to_dynamic(
          $list.map(
            locs,
            (loc) => {
              return $types.to_dynamic(
                $dict.from_list(
                  toList([
                    ["line", $types.to_dynamic(loc.line)],
                    ["column", $types.to_dynamic(loc.column)],
                  ]),
                ),
              );
            },
          ),
        ),
      ],
      base,
    );
  } else {
    _block = base;
  }
  let with_locations$1 = _block;
  let _block$1;
  let $1 = err.path;
  if ($1 instanceof Some) {
    let path_segments = $1[0];
    _block$1 = listPrepend(
      [
        "path",
        $types.to_dynamic($list.map(path_segments, path_segment_to_dynamic)),
      ],
      with_locations$1,
    );
  } else {
    _block$1 = with_locations$1;
  }
  let with_path$1 = _block$1;
  let _block$2;
  let $2 = err.extensions;
  if ($2 instanceof Some) {
    let ext = $2[0];
    _block$2 = listPrepend(["extensions", $types.to_dynamic(ext)], with_path$1);
  } else {
    _block$2 = with_path$1;
  }
  let final = _block$2;
  return $types.to_dynamic($dict.from_list(final));
}

/**
 * Convert multiple errors to a list
 */
export function errors_to_dynamic(errors) {
  return $types.to_dynamic($list.map(errors, to_dynamic));
}

function format_path(segments) {
  let _pipe = segments;
  let _pipe$1 = $list.map(
    _pipe,
    (s) => {
      if (s instanceof FieldSegment) {
        let name = s.name;
        return name;
      } else {
        let index = s.index;
        return ("[" + $int.to_string(index)) + "]";
      }
    },
  );
  return $string.join(_pipe$1, ".");
}

function format_locations(locations) {
  let loc_strs = $list.map(
    locations,
    (loc) => {
      return ((("(" + $int.to_string(loc.line)) + ":") + $int.to_string(
        loc.column,
      )) + ")";
    },
  );
  if (loc_strs instanceof $Empty) {
    return "";
  } else {
    return ("[" + $string.join(loc_strs, ", ")) + "]";
  }
}

/**
 * Format an error as a human-readable string
 */
export function format(err) {
  let msg = err.message;
  let _block;
  let $ = err.path;
  if ($ instanceof Some) {
    let path_segments = $[0];
    _block = (msg + " at ") + format_path(path_segments);
  } else {
    _block = msg;
  }
  let with_path$1 = _block;
  let _block$1;
  let $1 = err.locations;
  if ($1 instanceof Some) {
    let locs = $1[0];
    _block$1 = (with_path$1 + " ") + format_locations(locs);
  } else {
    _block$1 = with_path$1;
  }
  let with_locations$1 = _block$1;
  return with_locations$1;
}

function category_to_string(category) {
  if (category instanceof ValidationErrorCategory) {
    return "VALIDATION";
  } else if (category instanceof ResolverErrorCategory) {
    return "RESOLVER";
  } else if (category instanceof TypeErrorCategory) {
    return "TYPE";
  } else if (category instanceof AuthenticationErrorCategory) {
    return "AUTHENTICATION";
  } else if (category instanceof AuthorizationErrorCategory) {
    return "AUTHORIZATION";
  } else if (category instanceof InternalErrorCategory) {
    return "INTERNAL";
  } else {
    return "USER_INPUT";
  }
}

/**
 * Set error category in extensions
 */
export function with_category(err, category) {
  return with_extension(
    err,
    "category",
    $types.to_dynamic(category_to_string(category)),
  );
}

/**
 * Create a validation error
 */
export function validation_error(message, path) {
  let _pipe = error_at(message, path);
  return with_category(_pipe, new ValidationErrorCategory());
}

/**
 * Create a resolver error
 */
export function resolver_error(message, path) {
  let _pipe = error_at(message, path);
  return with_category(_pipe, new ResolverErrorCategory());
}

/**
 * Create a type error
 */
export function type_error(message, path) {
  let _pipe = error_at(message, path);
  return with_category(_pipe, new TypeErrorCategory());
}

/**
 * Create an authentication error
 */
export function authentication_error(message) {
  let _pipe = error(message);
  let _pipe$1 = with_category(_pipe, new AuthenticationErrorCategory());
  return with_code(_pipe$1, "UNAUTHENTICATED");
}

/**
 * Create an authorization error
 */
export function authorization_error(message, path) {
  let _pipe = error_at(message, path);
  let _pipe$1 = with_category(_pipe, new AuthorizationErrorCategory());
  return with_code(_pipe$1, "FORBIDDEN");
}

/**
 * Create a user input error
 */
export function user_input_error(message, field, path) {
  let _pipe = error_at(message, path);
  let _pipe$1 = with_category(_pipe, new UserInputErrorCategory());
  let _pipe$2 = with_code(_pipe$1, "BAD_USER_INPUT");
  return with_extension(_pipe$2, "field", $types.to_dynamic(field));
}

/**
 * Create an internal server error
 */
export function internal_error(message) {
  let _pipe = error(message);
  let _pipe$1 = with_category(_pipe, new InternalErrorCategory());
  return with_code(_pipe$1, "INTERNAL_SERVER_ERROR");
}

/**
 * Convert string path to PathSegment list
 */
export function path_from_strings(path) {
  return $list.map(path, (var0) => { return new FieldSegment(var0); });
}

/**
 * Add index segment to path
 */
export function append_index(path, index) {
  return $list.append(path, toList([new IndexSegment(index)]));
}

/**
 * Add field segment to path
 */
export function append_field(path, field) {
  return $list.append(path, toList([new FieldSegment(field)]));
}
