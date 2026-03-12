// Tests for mochi/upload.gleam - File upload handling
import gleam/list
import gleeunit/should
import mochi/upload

// ============================================================================
// Configuration Tests
// ============================================================================

pub fn default_config_test() {
  let config = upload.default_config()
  should.equal(config.max_file_size, 10 * 1024 * 1024)
  should.equal(config.max_files, 10)
  should.equal(config.allowed_mime_types, [])
  should.equal(config.temp_directory, "/tmp")
}

pub fn with_max_file_size_test() {
  let config =
    upload.default_config()
    |> upload.with_max_file_size(5 * 1024 * 1024)
  should.equal(config.max_file_size, 5 * 1024 * 1024)
}

pub fn with_max_files_test() {
  let config =
    upload.default_config()
    |> upload.with_max_files(5)
  should.equal(config.max_files, 5)
}

pub fn with_allowed_mime_types_test() {
  let config =
    upload.default_config()
    |> upload.with_allowed_mime_types(["image/png", "image/jpeg"])
  should.equal(config.allowed_mime_types, ["image/png", "image/jpeg"])
}

pub fn allow_images_test() {
  let config =
    upload.default_config()
    |> upload.allow_images()
  should.be_true(list.contains(config.allowed_mime_types, "image/jpeg"))
  should.be_true(list.contains(config.allowed_mime_types, "image/png"))
  should.be_true(list.contains(config.allowed_mime_types, "image/gif"))
  should.be_true(list.contains(config.allowed_mime_types, "image/webp"))
}

pub fn allow_documents_test() {
  let config =
    upload.default_config()
    |> upload.allow_documents()
  should.be_true(list.contains(config.allowed_mime_types, "application/pdf"))
  should.be_true(list.contains(config.allowed_mime_types, "text/plain"))
  should.be_true(list.contains(config.allowed_mime_types, "text/csv"))
}

pub fn with_temp_directory_test() {
  let config =
    upload.default_config()
    |> upload.with_temp_directory("/var/tmp")
  should.equal(config.temp_directory, "/var/tmp")
}

// ============================================================================
// UploadedFile Creation Tests
// ============================================================================

pub fn new_uploaded_file_test() {
  let file = upload.new_uploaded_file("test.txt", "text/plain", "/tmp/abc123", 1024)
  should.equal(file.filename, "test.txt")
  should.equal(file.mime_type, "text/plain")
  should.equal(file.path, "/tmp/abc123")
  should.equal(file.size, 1024)
}

// ============================================================================
// Validation Tests - Size
// ============================================================================

pub fn validate_file_size_ok_test() {
  let config = upload.default_config() |> upload.with_max_file_size(1024)
  let file = upload.new_uploaded_file("small.txt", "text/plain", "/tmp/x", 500)
  let result = upload.validate(file, config)
  should.be_ok(result)
}

pub fn validate_file_size_exact_limit_test() {
  let config = upload.default_config() |> upload.with_max_file_size(1024)
  let file = upload.new_uploaded_file("exact.txt", "text/plain", "/tmp/x", 1024)
  let result = upload.validate(file, config)
  should.be_ok(result)
}

pub fn validate_file_too_large_test() {
  let config = upload.default_config() |> upload.with_max_file_size(1024)
  let file = upload.new_uploaded_file("big.txt", "text/plain", "/tmp/x", 2048)
  let result = upload.validate(file, config)
  should.be_error(result)
  case result {
    Error(upload.FileTooLarge(filename, size, max)) -> {
      should.equal(filename, "big.txt")
      should.equal(size, 2048)
      should.equal(max, 1024)
    }
    _ -> should.fail()
  }
}

// ============================================================================
// Validation Tests - MIME Type
// ============================================================================

pub fn validate_mime_type_allowed_test() {
  let config =
    upload.default_config()
    |> upload.with_allowed_mime_types(["image/png", "image/jpeg"])
  let file = upload.new_uploaded_file("photo.png", "image/png", "/tmp/x", 100)
  let result = upload.validate(file, config)
  should.be_ok(result)
}

pub fn validate_mime_type_not_allowed_test() {
  let config =
    upload.default_config()
    |> upload.with_allowed_mime_types(["image/png", "image/jpeg"])
  let file = upload.new_uploaded_file("doc.pdf", "application/pdf", "/tmp/x", 100)
  let result = upload.validate(file, config)
  should.be_error(result)
  case result {
    Error(upload.InvalidMimeType(filename, mime)) -> {
      should.equal(filename, "doc.pdf")
      should.equal(mime, "application/pdf")
    }
    _ -> should.fail()
  }
}

pub fn validate_all_mime_types_allowed_when_empty_test() {
  let config = upload.default_config()
  // Empty allowed_mime_types means all types allowed
  let file = upload.new_uploaded_file("anything.xyz", "application/x-custom", "/tmp/x", 100)
  let result = upload.validate(file, config)
  should.be_ok(result)
}

// ============================================================================
// Validation Tests - Multiple Files
// ============================================================================

pub fn validate_all_within_limit_test() {
  let config = upload.default_config() |> upload.with_max_files(3)
  let files = [
    upload.new_uploaded_file("a.txt", "text/plain", "/tmp/a", 100),
    upload.new_uploaded_file("b.txt", "text/plain", "/tmp/b", 100),
  ]
  let result = upload.validate_all(files, config)
  should.be_ok(result)
}

pub fn validate_all_too_many_files_test() {
  let config = upload.default_config() |> upload.with_max_files(2)
  let files = [
    upload.new_uploaded_file("a.txt", "text/plain", "/tmp/a", 100),
    upload.new_uploaded_file("b.txt", "text/plain", "/tmp/b", 100),
    upload.new_uploaded_file("c.txt", "text/plain", "/tmp/c", 100),
  ]
  let result = upload.validate_all(files, config)
  should.be_error(result)
  case result {
    Error(upload.TooManyFiles(count, max)) -> {
      should.equal(count, 3)
      should.equal(max, 2)
    }
    _ -> should.fail()
  }
}

pub fn validate_all_one_invalid_fails_all_test() {
  let config =
    upload.default_config()
    |> upload.with_max_file_size(1000)
  let files = [
    upload.new_uploaded_file("ok.txt", "text/plain", "/tmp/a", 100),
    upload.new_uploaded_file("toobig.txt", "text/plain", "/tmp/b", 5000),
  ]
  let result = upload.validate_all(files, config)
  should.be_error(result)
}

pub fn validate_all_empty_list_test() {
  let config = upload.default_config()
  let result = upload.validate_all([], config)
  should.be_ok(result)
}

// ============================================================================
// Error Formatting Tests
// ============================================================================

pub fn format_error_file_too_large_test() {
  let error = upload.FileTooLarge("big.txt", 15_000_000, 10_000_000)
  let formatted = upload.format_error(error)
  should.be_true(formatted != "")
  // Should contain the filename
  should.be_true(contains(formatted, "big.txt"))
}

pub fn format_error_too_many_files_test() {
  let error = upload.TooManyFiles(15, 10)
  let formatted = upload.format_error(error)
  should.be_true(contains(formatted, "15"))
  should.be_true(contains(formatted, "10"))
}

pub fn format_error_invalid_mime_type_test() {
  let error = upload.InvalidMimeType("file.exe", "application/x-msdownload")
  let formatted = upload.format_error(error)
  should.be_true(contains(formatted, "file.exe"))
  should.be_true(contains(formatted, "application/x-msdownload"))
}

pub fn format_error_file_not_found_test() {
  let error = upload.FileNotFound("missing.txt")
  let formatted = upload.format_error(error)
  should.be_true(contains(formatted, "missing.txt"))
}

pub fn format_error_read_error_test() {
  let error = upload.ReadError("broken.txt", "Permission denied")
  let formatted = upload.format_error(error)
  should.be_true(contains(formatted, "broken.txt"))
  should.be_true(contains(formatted, "Permission denied"))
}

pub fn format_error_cleanup_error_test() {
  let error = upload.CleanupError("/tmp/xyz", "File in use")
  let formatted = upload.format_error(error)
  should.be_true(contains(formatted, "/tmp/xyz"))
  should.be_true(contains(formatted, "File in use"))
}

// ============================================================================
// Dynamic Type Helpers Tests
// ============================================================================

pub fn to_dynamic_test() {
  let file = upload.new_uploaded_file("test.txt", "text/plain", "/tmp/x", 100)
  let _dyn = upload.to_dynamic(file)
  // Should not crash
  should.be_true(True)
}

pub fn from_dynamic_test() {
  let file = upload.new_uploaded_file("test.txt", "text/plain", "/tmp/x", 100)
  let dyn = upload.to_dynamic(file)
  let result = upload.from_dynamic(dyn)
  should.be_ok(result)
}

// ============================================================================
// Upload Scalar Tests
// ============================================================================

pub fn upload_scalar_creates_scalar_test() {
  let scalar = upload.upload_scalar()
  should.equal(scalar.name, "Upload")
  should.be_true(True)
}

// ============================================================================
// Helper Functions
// ============================================================================

import gleam/string

fn contains(haystack: String, needle: String) -> Bool {
  string.contains(haystack, needle)
}
