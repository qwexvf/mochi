// mochi/upload.gleam
// File upload scalar and utilities for GraphQL file uploads

import gleam/dynamic.{type Dynamic}
import gleam/string
import mochi/schema.{type ScalarType}
import mochi/types
import simplifile

// ============================================================================
// Types
// ============================================================================

/// Represents an uploaded file in a GraphQL request
pub type UploadedFile {
  UploadedFile(
    /// Original filename from the upload
    filename: String,
    /// MIME type of the file
    mime_type: String,
    /// Path to the temporary file on disk
    path: String,
    /// File size in bytes
    size: Int,
  )
}

/// Configuration for file upload handling
pub type UploadConfig {
  UploadConfig(
    /// Maximum allowed file size in bytes
    max_file_size: Int,
    /// Maximum number of files per request
    max_files: Int,
    /// Allowed MIME types (empty list means all types allowed)
    allowed_mime_types: List(String),
    /// Temporary directory for uploaded files
    temp_directory: String,
  )
}

/// Errors that can occur during file upload processing
pub type UploadError {
  FileTooLarge(filename: String, size: Int, max_size: Int)
  TooManyFiles(count: Int, max_count: Int)
  InvalidMimeType(filename: String, mime_type: String)
  FileNotFound(filename: String)
  ReadError(filename: String, reason: String)
  CleanupError(path: String, reason: String)
}

// ============================================================================
// Configuration
// ============================================================================

/// Create default upload configuration
pub fn default_config() -> UploadConfig {
  UploadConfig(
    max_file_size: 10 * 1024 * 1024,
    // 10 MB
    max_files: 10,
    allowed_mime_types: [],
    temp_directory: "/tmp",
  )
}

/// Set maximum file size
pub fn with_max_file_size(config: UploadConfig, size: Int) -> UploadConfig {
  UploadConfig(..config, max_file_size: size)
}

/// Set maximum number of files
pub fn with_max_files(config: UploadConfig, count: Int) -> UploadConfig {
  UploadConfig(..config, max_files: count)
}

/// Set allowed MIME types
pub fn with_allowed_mime_types(
  config: UploadConfig,
  types: List(String),
) -> UploadConfig {
  UploadConfig(..config, allowed_mime_types: types)
}

/// Allow image MIME types
pub fn allow_images(config: UploadConfig) -> UploadConfig {
  with_allowed_mime_types(config, [
    "image/jpeg", "image/png", "image/gif", "image/webp", "image/svg+xml",
  ])
}

/// Allow document MIME types
pub fn allow_documents(config: UploadConfig) -> UploadConfig {
  with_allowed_mime_types(config, [
    "application/pdf",
    "application/msword",
    "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
    "text/plain",
    "text/csv",
  ])
}

/// Set temporary directory
pub fn with_temp_directory(config: UploadConfig, dir: String) -> UploadConfig {
  UploadConfig(..config, temp_directory: dir)
}

// ============================================================================
// Upload Scalar
// ============================================================================

/// Create the Upload scalar type for GraphQL schema
/// The Upload scalar represents a file upload following the GraphQL multipart request spec
pub fn upload_scalar() -> ScalarType {
  schema.scalar("Upload")
  |> schema.scalar_description(
    "The `Upload` scalar type represents a file upload. "
    <> "Use multipart/form-data content type to upload files.",
  )
  |> schema.serialize(serialize_upload)
  |> schema.parse_value(parse_upload_value)
  |> schema.parse_literal(parse_upload_literal)
}

fn serialize_upload(_value: Dynamic) -> Result(Dynamic, String) {
  // Uploads are input-only, they should not be serialized in responses
  // If someone tries to return an Upload, we return null
  Ok(types.to_dynamic(Nil))
}

fn parse_upload_value(value: Dynamic) -> Result(Dynamic, String) {
  // The value should already be an UploadedFile created by the multipart parser
  // Just pass it through
  Ok(value)
}

fn parse_upload_literal(_value: Dynamic) -> Result(Dynamic, String) {
  // Upload cannot be specified as a literal in a query
  Error("Upload scalar cannot be specified as a literal, use multipart request")
}

// ============================================================================
// UploadedFile Operations
// ============================================================================

/// Create a new UploadedFile
pub fn new_uploaded_file(
  filename: String,
  mime_type: String,
  path: String,
  size: Int,
) -> UploadedFile {
  UploadedFile(filename: filename, mime_type: mime_type, path: path, size: size)
}

/// Read all contents of an uploaded file
pub fn read_all(upload: UploadedFile) -> Result(BitArray, UploadError) {
  case simplifile.read_bits(upload.path) {
    Ok(bytes) -> Ok(bytes)
    Error(err) ->
      Error(ReadError(upload.filename, simplifile.describe_error(err)))
  }
}

/// Read the uploaded file as a string
pub fn read_string(upload: UploadedFile) -> Result(String, UploadError) {
  case simplifile.read(upload.path) {
    Ok(content) -> Ok(content)
    Error(err) ->
      Error(ReadError(upload.filename, simplifile.describe_error(err)))
  }
}

/// Delete the temporary uploaded file
pub fn cleanup(upload: UploadedFile) -> Result(Nil, UploadError) {
  case simplifile.delete(upload.path) {
    Ok(_) -> Ok(Nil)
    Error(err) ->
      Error(CleanupError(upload.path, simplifile.describe_error(err)))
  }
}

/// Move the uploaded file to a permanent location
pub fn move_to(upload: UploadedFile, destination: String) -> Result(Nil, String) {
  case simplifile.rename(upload.path, destination) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(simplifile.describe_error(err))
  }
}

/// Copy the uploaded file to a location
pub fn copy_to(upload: UploadedFile, destination: String) -> Result(Nil, String) {
  case simplifile.copy_file(upload.path, destination) {
    Ok(_) -> Ok(Nil)
    Error(err) -> Error(simplifile.describe_error(err))
  }
}

// ============================================================================
// Validation
// ============================================================================

/// Validate an uploaded file against configuration
pub fn validate(
  upload: UploadedFile,
  config: UploadConfig,
) -> Result(UploadedFile, UploadError) {
  use <- validate_size(upload, config)
  use <- validate_mime_type(upload, config)
  Ok(upload)
}

fn validate_size(
  upload: UploadedFile,
  config: UploadConfig,
  next: fn() -> Result(UploadedFile, UploadError),
) -> Result(UploadedFile, UploadError) {
  case upload.size > config.max_file_size {
    True ->
      Error(FileTooLarge(upload.filename, upload.size, config.max_file_size))
    False -> next()
  }
}

fn validate_mime_type(
  upload: UploadedFile,
  config: UploadConfig,
  next: fn() -> Result(UploadedFile, UploadError),
) -> Result(UploadedFile, UploadError) {
  case config.allowed_mime_types {
    [] -> next()
    // Empty list means all types allowed
    allowed ->
      case list.contains(allowed, upload.mime_type) {
        True -> next()
        False -> Error(InvalidMimeType(upload.filename, upload.mime_type))
      }
  }
}

import gleam/list

/// Validate multiple uploads against configuration
pub fn validate_all(
  uploads: List(UploadedFile),
  config: UploadConfig,
) -> Result(List(UploadedFile), UploadError) {
  // Check total count
  case list.length(uploads) > config.max_files {
    True -> Error(TooManyFiles(list.length(uploads), config.max_files))
    False -> {
      // Validate each file
      let results = list.map(uploads, validate(_, config))
      let errors =
        list.filter_map(results, fn(r) {
          case r {
            Error(e) -> Ok(e)
            Ok(_) -> Error(Nil)
          }
        })
      case errors {
        [first, ..] -> Error(first)
        [] -> Ok(uploads)
      }
    }
  }
}

// ============================================================================
// Error Formatting
// ============================================================================

/// Format an upload error as a human-readable string
pub fn format_error(error: UploadError) -> String {
  case error {
    FileTooLarge(filename, size, max) ->
      "File '"
      <> filename
      <> "' is too large ("
      <> format_bytes(size)
      <> "), maximum allowed is "
      <> format_bytes(max)
    TooManyFiles(count, max) ->
      "Too many files uploaded ("
      <> string.inspect(count)
      <> "), maximum allowed is "
      <> string.inspect(max)
    InvalidMimeType(filename, mime) ->
      "File '" <> filename <> "' has invalid MIME type: " <> mime
    FileNotFound(filename) -> "Uploaded file '" <> filename <> "' not found"
    ReadError(filename, reason) ->
      "Error reading file '" <> filename <> "': " <> reason
    CleanupError(path, reason) ->
      "Error cleaning up file '" <> path <> "': " <> reason
  }
}

fn format_bytes(bytes: Int) -> String {
  case bytes {
    b if b < 1024 -> string.inspect(b) <> " bytes"
    b if b < 1_048_576 -> string.inspect(b / 1024) <> " KB"
    b -> string.inspect(b / 1_048_576) <> " MB"
  }
}

// ============================================================================
// Dynamic Type Helpers
// ============================================================================

/// Convert an UploadedFile to Dynamic
pub fn to_dynamic(upload: UploadedFile) -> Dynamic {
  types.to_dynamic(upload)
}

/// Try to extract an UploadedFile from a Dynamic value
pub fn from_dynamic(value: Dynamic) -> Result(UploadedFile, String) {
  // In practice, the multipart parser creates the UploadedFile
  // and passes it directly, so this is mainly for type coercion
  Ok(unsafe_coerce(value))
}

@external(erlang, "gleam_stdlib", "identity")
@external(javascript, "./mochi_coerce_ffi.mjs", "identity")
fn unsafe_coerce(value: Dynamic) -> UploadedFile
