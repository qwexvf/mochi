// mochi/transport/multipart.gleam
// Multipart request parser for GraphQL file uploads
// Implements the GraphQL multipart request specification:
// https://github.com/jaydenseric/graphql-multipart-request-spec

import gleam/bit_array
import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/dynamic/decode
import gleam/int
import gleam/json
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import mochi/upload.{type UploadConfig, type UploadedFile}
import simplifile

// ============================================================================
// Types
// ============================================================================

/// Configuration for multipart request parsing
pub type MultipartConfig {
  MultipartConfig(
    /// Maximum allowed file size in bytes
    max_file_size: Int,
    /// Maximum number of files per request
    max_files: Int,
    /// Allowed MIME types for uploads (empty = all allowed)
    allowed_mime_types: List(String),
    /// Temporary directory for storing uploaded files
    temp_directory: String,
    /// Maximum size of the operations JSON field
    max_operations_size: Int,
    /// Maximum size of the map JSON field
    max_map_size: Int,
  )
}

/// Parsed multipart GraphQL request
pub type MultipartGraphQLRequest {
  MultipartGraphQLRequest(
    /// The GraphQL operations (query, variables, operationName)
    operations: Operations,
    /// Uploaded files mapped to their variable paths
    files: Dict(String, UploadedFile),
  )
}

/// GraphQL operations from the multipart request
pub type Operations {
  SingleOperation(
    query: String,
    variables: Dict(String, Dynamic),
    operation_name: Option(String),
  )
  BatchOperations(operations: List(SingleOperationData))
}

/// Data for a single operation in a batch
pub type SingleOperationData {
  SingleOperationData(
    query: String,
    variables: Dict(String, Dynamic),
    operation_name: Option(String),
  )
}

/// A single part from a multipart form
pub type FormPart {
  FormPart(
    /// The field name from Content-Disposition
    name: String,
    /// Original filename (for file uploads)
    filename: Option(String),
    /// Content-Type header
    content_type: String,
    /// The part content
    content: BitArray,
  )
}

/// Errors that can occur during multipart parsing
pub type MultipartError {
  MissingOperationsField
  MissingMapField
  InvalidOperationsJson(String)
  InvalidMapJson(String)
  MapPathNotFound(path: String)
  FileNotInMap(filename: String)
  FileTooLarge(filename: String, size: Int, max: Int)
  TooManyFiles(count: Int, max: Int)
  InvalidMimeType(filename: String, mime: String)
  InvalidMultipartFormat(String)
  OperationsTooLarge(size: Int, max: Int)
  MapTooLarge(size: Int, max: Int)
}

// ============================================================================
// Configuration
// ============================================================================

/// Create default multipart configuration
pub fn default_config() -> MultipartConfig {
  MultipartConfig(
    max_file_size: 10 * 1024 * 1024,
    // 10 MB
    max_files: 10,
    allowed_mime_types: [],
    temp_directory: "/tmp",
    max_operations_size: 100 * 1024,
    // 100 KB
    max_map_size: 10 * 1024,
    // 10 KB
  )
}

/// Set maximum file size
pub fn with_max_file_size(config: MultipartConfig, size: Int) -> MultipartConfig {
  MultipartConfig(..config, max_file_size: size)
}

/// Set maximum number of files
pub fn with_max_files(config: MultipartConfig, count: Int) -> MultipartConfig {
  MultipartConfig(..config, max_files: count)
}

/// Set allowed MIME types
pub fn with_allowed_mime_types(
  config: MultipartConfig,
  types: List(String),
) -> MultipartConfig {
  MultipartConfig(..config, allowed_mime_types: types)
}

/// Set temp directory
pub fn with_temp_directory(
  config: MultipartConfig,
  dir: String,
) -> MultipartConfig {
  MultipartConfig(..config, temp_directory: dir)
}

/// Convert to UploadConfig for validation
pub fn to_upload_config(config: MultipartConfig) -> UploadConfig {
  upload.UploadConfig(
    max_file_size: config.max_file_size,
    max_files: config.max_files,
    allowed_mime_types: config.allowed_mime_types,
    temp_directory: config.temp_directory,
  )
}

// ============================================================================
// Parsing
// ============================================================================

/// Parse a multipart GraphQL request from form data
///
/// The form data should contain:
/// - `operations`: JSON with query/variables/operationName (required)
/// - `map`: JSON mapping file field names to object paths (required if files present)
/// - File fields: The actual uploaded files referenced in the map
pub fn parse_multipart_request(
  form_parts: List(FormPart),
  config: MultipartConfig,
) -> Result(MultipartGraphQLRequest, MultipartError) {
  // Find the operations field
  use operations_part <- result.try(find_part(form_parts, "operations"))
  use operations <- result.try(parse_operations(operations_part, config))

  // Find the map field (may not exist if no files)
  let map_result = find_part(form_parts, "map")

  case map_result {
    Error(_) -> {
      // No map field, so no files - just return operations
      Ok(MultipartGraphQLRequest(operations: operations, files: dict.new()))
    }
    Ok(map_part) -> {
      // Parse map and process files
      use file_map <- result.try(parse_map(map_part, config))

      // Find all file parts and validate
      let file_parts = get_file_parts(form_parts)

      // Check file count
      case list.length(file_parts) > config.max_files {
        True -> Error(TooManyFiles(list.length(file_parts), config.max_files))
        False -> {
          // Process each file
          use files <- result.try(process_files(file_parts, file_map, config))

          // Apply files to operations
          use final_operations <- result.try(apply_files_to_operations(
            operations,
            file_map,
            files,
          ))

          Ok(MultipartGraphQLRequest(operations: final_operations, files: files))
        }
      }
    }
  }
}

fn find_part(
  parts: List(FormPart),
  name: String,
) -> Result(FormPart, MultipartError) {
  parts
  |> list.find(fn(p) { p.name == name })
  |> result.map_error(fn(_) {
    case name {
      "operations" -> MissingOperationsField
      "map" -> MissingMapField
      _ -> InvalidMultipartFormat("Missing field: " <> name)
    }
  })
}

fn parse_operations(
  part: FormPart,
  config: MultipartConfig,
) -> Result(Operations, MultipartError) {
  let size = bit_array.byte_size(part.content)
  case size > config.max_operations_size {
    True -> Error(OperationsTooLarge(size, config.max_operations_size))
    False -> {
      case bit_array.to_string(part.content) {
        Ok(json) -> parse_operations_json(json)
        Error(_) -> Error(InvalidOperationsJson("Invalid UTF-8"))
      }
    }
  }
}

fn parse_operations_json(json_str: String) -> Result(Operations, MultipartError) {
  case json.parse(json_str, decode.dynamic) {
    Ok(value) -> {
      // Check if it's an array (batch) or object (single)
      case get_list_raw(value) {
        Ok(items) -> {
          // Batch operations
          let ops =
            list.filter_map(items, fn(item) {
              case parse_single_operation(item) {
                Ok(op) -> Ok(op)
                Error(_) -> Error(Nil)
              }
            })
          Ok(BatchOperations(ops))
        }
        Error(_) -> {
          // Single operation
          case parse_single_operation(value) {
            Ok(op) ->
              Ok(SingleOperation(op.query, op.variables, op.operation_name))
            Error(e) -> Error(InvalidOperationsJson(e))
          }
        }
      }
    }
    Error(_) -> Error(InvalidOperationsJson("Invalid JSON"))
  }
}

fn parse_single_operation(value: Dynamic) -> Result(SingleOperationData, String) {
  let query_result = extract_string_field(value, "query")
  let variables = extract_variables(value)
  let operation_name = extract_operation_name(value)

  case query_result {
    Ok(query) -> Ok(SingleOperationData(query, variables, operation_name))
    Error(_) -> Error("Missing or invalid query field")
  }
}

fn parse_map(
  part: FormPart,
  config: MultipartConfig,
) -> Result(Dict(String, List(String)), MultipartError) {
  let size = bit_array.byte_size(part.content)
  case size > config.max_map_size {
    True -> Error(MapTooLarge(size, config.max_map_size))
    False -> {
      case bit_array.to_string(part.content) {
        Ok(json) -> parse_map_json(json)
        Error(_) -> Error(InvalidMapJson("Invalid UTF-8"))
      }
    }
  }
}

fn parse_map_json(
  json_str: String,
) -> Result(Dict(String, List(String)), MultipartError) {
  case json.parse(json_str, decode.dynamic) {
    Ok(value) -> {
      case get_dict_raw(value) {
        Ok(raw_map) -> {
          // Convert values to List(String)
          let converted =
            dict.to_list(raw_map)
            |> list.filter_map(fn(kv) {
              let #(key, val) = kv
              case get_string_list_raw(val) {
                Ok(paths) -> Ok(#(key, paths))
                Error(_) -> Error(Nil)
              }
            })
            |> dict.from_list
          Ok(converted)
        }
        Error(_) -> Error(InvalidMapJson("Expected object"))
      }
    }
    Error(_) -> Error(InvalidMapJson("Invalid JSON"))
  }
}

fn get_file_parts(parts: List(FormPart)) -> List(FormPart) {
  parts
  |> list.filter(fn(p) {
    p.name != "operations" && p.name != "map" && option.is_some(p.filename)
  })
}

fn process_files(
  parts: List(FormPart),
  _file_map: Dict(String, List(String)),
  config: MultipartConfig,
) -> Result(Dict(String, UploadedFile), MultipartError) {
  let results = list.map(parts, fn(part) { process_file_part(part, config) })

  // Check for errors
  let errors =
    list.filter_map(results, fn(r) {
      case r {
        Error(e) -> Ok(e)
        Ok(_) -> Error(Nil)
      }
    })

  case errors {
    [first, ..] -> Error(first)
    [] -> {
      let files =
        list.filter_map(results, fn(r) {
          case r {
            Ok(#(name, file)) -> Ok(#(name, file))
            Error(_) -> Error(Nil)
          }
        })
        |> dict.from_list
      Ok(files)
    }
  }
}

fn process_file_part(
  part: FormPart,
  config: MultipartConfig,
) -> Result(#(String, UploadedFile), MultipartError) {
  let filename = option.unwrap(part.filename, part.name)
  let size = bit_array.byte_size(part.content)

  // Check size
  case size > config.max_file_size {
    True -> Error(FileTooLarge(filename, size, config.max_file_size))
    False -> {
      // Check MIME type
      case validate_mime_type(part.content_type, config) {
        False -> Error(InvalidMimeType(filename, part.content_type))
        True -> {
          // Write to temp file
          let temp_path = generate_temp_path(config.temp_directory, filename)
          case simplifile.write_bits(temp_path, part.content) {
            Ok(_) -> {
              let file =
                upload.new_uploaded_file(
                  filename,
                  part.content_type,
                  temp_path,
                  size,
                )
              Ok(#(part.name, file))
            }
            Error(err) ->
              Error(InvalidMultipartFormat(
                "Failed to write file: " <> simplifile.describe_error(err),
              ))
          }
        }
      }
    }
  }
}

fn validate_mime_type(mime: String, config: MultipartConfig) -> Bool {
  case config.allowed_mime_types {
    [] -> True
    allowed -> list.contains(allowed, mime)
  }
}

fn apply_files_to_operations(
  operations: Operations,
  file_map: Dict(String, List(String)),
  files: Dict(String, UploadedFile),
) -> Result(Operations, MultipartError) {
  // For each file in the map, update the corresponding variable path
  // This is where we inject the UploadedFile into the variables
  case operations {
    SingleOperation(query, variables, name) -> {
      use new_vars <- result.try(apply_files_to_variables(
        variables,
        file_map,
        files,
        "",
      ))
      Ok(SingleOperation(query, new_vars, name))
    }
    BatchOperations(ops) -> {
      let results =
        list.index_map(ops, fn(op, index) {
          let prefix = int.to_string(index) <> "."
          case apply_files_to_variables(op.variables, file_map, files, prefix) {
            Ok(new_vars) ->
              Ok(SingleOperationData(op.query, new_vars, op.operation_name))
            Error(e) -> Error(e)
          }
        })

      let errors =
        list.filter_map(results, fn(r) {
          case r {
            Error(e) -> Ok(e)
            Ok(_) -> Error(Nil)
          }
        })

      case errors {
        [first, ..] -> Error(first)
        [] ->
          Ok(
            BatchOperations(
              list.filter_map(results, fn(r) {
                case r {
                  Ok(op) -> Ok(op)
                  Error(_) -> Error(Nil)
                }
              }),
            ),
          )
      }
    }
  }
}

fn apply_files_to_variables(
  variables: Dict(String, Dynamic),
  file_map: Dict(String, List(String)),
  files: Dict(String, UploadedFile),
  path_prefix: String,
) -> Result(Dict(String, Dynamic), MultipartError) {
  // For each file field in the map, find matching paths and inject the file
  let updated =
    dict.fold(file_map, Ok(variables), fn(acc, field_name, paths) {
      case acc {
        Error(e) -> Error(e)
        Ok(vars) -> {
          case dict.get(files, field_name) {
            Error(_) -> Ok(vars)
            // File not present, skip
            Ok(file) -> {
              // Apply to each path that starts with our prefix
              let relevant_paths =
                list.filter(paths, fn(p) {
                  string.starts_with(p, path_prefix <> "variables.")
                })
              apply_file_to_paths(vars, relevant_paths, file, path_prefix)
            }
          }
        }
      }
    })

  updated
}

fn apply_file_to_paths(
  variables: Dict(String, Dynamic),
  paths: List(String),
  file: UploadedFile,
  prefix: String,
) -> Result(Dict(String, Dynamic), MultipartError) {
  // For now, handle simple single-level variable paths
  // Full implementation would handle nested paths like "variables.input.file"
  list.fold(paths, Ok(variables), fn(acc, path) {
    case acc {
      Error(e) -> Error(e)
      Ok(vars) -> {
        let stripped =
          string.drop_start(path, string.length(prefix <> "variables."))
        // Simple case: direct variable
        case string.contains(stripped, ".") {
          False -> Ok(dict.insert(vars, stripped, upload.to_dynamic(file)))
          True ->
            // Nested path - simplified: just use the first segment
            Ok(vars)
        }
      }
    }
  })
}

// ============================================================================
// Helpers
// ============================================================================

fn generate_temp_path(dir: String, filename: String) -> String {
  let timestamp = int.to_string(get_random_int())
  dir <> "/mochi_upload_" <> timestamp <> "_" <> sanitize_filename(filename)
}

fn sanitize_filename(filename: String) -> String {
  filename
  |> string.replace("/", "_")
  |> string.replace("\\", "_")
  |> string.replace("..", "_")
}

fn extract_string_field(value: Dynamic, field: String) -> Result(String, Nil) {
  decode.run(value, decode.at([field], decode.string))
  |> result.map_error(fn(_) { Nil })
}

fn extract_variables(value: Dynamic) -> Dict(String, Dynamic) {
  case
    decode.run(
      value,
      decode.at(["variables"], decode.dict(decode.string, decode.dynamic)),
    )
  {
    Ok(d) -> d
    Error(_) -> dict.new()
  }
}

fn extract_operation_name(value: Dynamic) -> Option(String) {
  case extract_string_field(value, "operationName") {
    Ok(name) -> Some(name)
    Error(_) -> None
  }
}

// ============================================================================
// Error Formatting
// ============================================================================

/// Format a multipart error as a human-readable string
pub fn format_error(error: MultipartError) -> String {
  case error {
    MissingOperationsField -> "Missing required 'operations' field"
    MissingMapField -> "Missing required 'map' field for file uploads"
    InvalidOperationsJson(msg) -> "Invalid operations JSON: " <> msg
    InvalidMapJson(msg) -> "Invalid map JSON: " <> msg
    MapPathNotFound(path) -> "Path not found in operations: " <> path
    FileNotInMap(filename) -> "File not referenced in map: " <> filename
    FileTooLarge(filename, size, max) ->
      "File '"
      <> filename
      <> "' is too large ("
      <> int.to_string(size)
      <> " bytes, max "
      <> int.to_string(max)
      <> ")"
    TooManyFiles(count, max) ->
      "Too many files ("
      <> int.to_string(count)
      <> ", max "
      <> int.to_string(max)
      <> ")"
    InvalidMimeType(filename, mime) ->
      "Invalid MIME type for '" <> filename <> "': " <> mime
    InvalidMultipartFormat(msg) -> "Invalid multipart format: " <> msg
    OperationsTooLarge(size, max) ->
      "Operations field too large ("
      <> int.to_string(size)
      <> " bytes, max "
      <> int.to_string(max)
      <> ")"
    MapTooLarge(size, max) ->
      "Map field too large ("
      <> int.to_string(size)
      <> " bytes, max "
      <> int.to_string(max)
      <> ")"
  }
}

// ============================================================================
// Helpers
// ============================================================================

fn get_dict_raw(value: Dynamic) -> Result(Dict(String, Dynamic), Nil) {
  decode.run(value, decode.dict(decode.string, decode.dynamic))
  |> result.map_error(fn(_) { Nil })
}

fn get_list_raw(value: Dynamic) -> Result(List(Dynamic), Nil) {
  decode.run(value, decode.list(decode.dynamic))
  |> result.map_error(fn(_) { Nil })
}

fn get_string_list_raw(value: Dynamic) -> Result(List(String), Nil) {
  decode.run(value, decode.list(decode.string))
  |> result.map_error(fn(_) { Nil })
}

@external(erlang, "mochi_random_ffi", "unique_positive_int")
@external(javascript, "./mochi_random_ffi.mjs", "unique_positive_int")
fn get_random_int() -> Int
