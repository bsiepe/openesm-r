library(testthat)
Sys.setenv(NOT_CRAN = "true")

test_that("get_cache_path constructs paths and creates directories", {
  temp_dir <- tempfile("cache")
  testthat::local_mocked_bindings(
    get_metadata_dir = function() fs::path(temp_dir, "metadata"),
    get_data_dir = function() fs::path(temp_dir, "data"),
    .package = "openesm"
  )
  
  # test metadata path
  meta_path <- get_cache_path("0001", "v1", "meta.json", type = "metadata")
  expected_meta_path <- fs::path(temp_dir, "metadata", "0001", "v1", "meta.json")
  expect_equal(meta_path, expected_meta_path)
  expect_true(fs::dir_exists(fs::path_dir(meta_path)))
  
  # test data path
  data_path <- get_cache_path("0002", "v2", "data.tsv", type = "data")
  expected_data_path <- fs::path(temp_dir, "data", "0002", "v2", "data.tsv")
  expect_equal(data_path, expected_data_path)
  expect_true(fs::dir_exists(fs::path_dir(data_path)))
  
  unlink(temp_dir, recursive = TRUE)
})


test_that("get_cache_dir returns correct paths", {
  temp_dir <- tempfile("cache")
  dir.create(temp_dir)
  
  testthat::local_mocked_bindings(
    get_cache_dir = function(type) {
      if (type == "metadata") {
        return(fs::path(temp_dir, "metadata"))
      } else if (type == "data") {
        return(fs::path(temp_dir, "data"))
      } else {
        stop("Unknown cache type")
      }
    }
  )
  meta_path <- get_cache_dir(type = "metadata")
  data_path <- get_cache_dir(type = "data")
  expect_equal(meta_path, fs::path(temp_dir, "metadata"))
  expect_equal(data_path, fs::path(temp_dir, "data"))
  expect_equal(get_metadata_dir(), fs::path(temp_dir, "metadata"))
  expect_equal(get_data_dir(), fs::path(temp_dir, "data"))
  
  unlink(temp_dir, recursive = TRUE)
})


test_that("cache_info works when cache exists", {
  # Mock get_cache_dir to point to a controlled temporary directory
  temp_dir <- tempfile("testcache")
  dir.create(temp_dir)
  # Create a dummy file to give the cache some size
  writeLines("some data", file.path(temp_dir, "dummy.txt"))
  
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  # Capture the output and check for expected messages
  output <- cli::cli_fmt(cache_info())
  expect_true(any(grepl("Cache location:", output)))
  expect_true(any(grepl("Cache size:", output)))
  
  unlink(temp_dir, recursive = TRUE)
})

test_that("cache_info works when cache does not exist", {
  temp_dir <- tempfile("nonexistent")
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  # Capture the output and check for the specific message
  output <- cli::cli_fmt(cache_info())
  expect_true(any(grepl("Cache directory does not exist yet", output)))
})

test_that("clear_cache works non-interactively with force = TRUE", {
  temp_dir <- tempfile("testcache")
  dir.create(temp_dir)
  writeLines("test", file.path(temp_dir, "file.txt"))
  
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_dir
  )
  
  expect_true(fs::dir_exists(temp_dir))
  
  # Capture output and check for success message
  output <- cli::cli_fmt(clear_cache(force = TRUE))
  expect_true(any(grepl("Cache cleared", output)))
  
  expect_false(fs::dir_exists(temp_dir))
})

test_that("clear_cache handles non-existent directory gracefully", {
  testthat::local_mocked_bindings(
    get_cache_dir = function() tempfile("nonexistent")
  )
  
  # Capture output and check for the correct informational message
  output <- cli::cli_fmt(clear_cache(force = TRUE))
  expect_true(any(grepl("Cache directory does not exist", output)))
})

test_that("read_json_safe handles errors correctly", {
  # test with a non-existent file, which should trigger an error
  expect_error(
    read_json_safe("a_file_that_does_not_exist.json"),
    "Failed to read JSON file"
  )
  
  # test with a malformed file
  malformed_file <- tempfile()
  # write invalid json (single quotes instead of double)
  writeLines("{'key': 'value'}", malformed_file)
  expect_error(
    read_json_safe(malformed_file),
    "Failed to read JSON file"
  )
  unlink(malformed_file)
})

test_that("get_cache_dir returns correct path without type parameter", {
  # test basic functionality
  cache_dir <- get_cache_dir()
  expect_type(cache_dir, "character")
  
  expect_true(grepl("openesm", cache_dir))
  
  # directory should exist after calling the function
  expect_true(fs::dir_exists(cache_dir))
})

test_that("get_cache_dir returns correct path with type parameter", {
  # test with a type parameter
  cache_dir <- get_cache_dir(type = "datasets")
  
  # should be a character string
  expect_type(cache_dir, "character")
  
  # should contain both "openesm" and "datasets" in the path
  expect_true(grepl("openesm", cache_dir))
  expect_true(grepl("datasets", cache_dir))
  
  # directory should exist after calling the function
  expect_true(fs::dir_exists(cache_dir))
  
  # test with another type
  cache_dir_meta <- get_cache_dir(type = "metadata")
  expect_true(grepl("metadata", cache_dir_meta))
  expect_true(fs::dir_exists(cache_dir_meta))
})

test_that("get_cache_dir creates nested directories correctly", {
  # test with nested path
  cache_dir <- get_cache_dir(type = "deep/nested/path")
  
  # should contain the nested structure
  expect_true(grepl("deep", cache_dir))
  expect_true(grepl("nested", cache_dir))
  expect_true(grepl("path", cache_dir))
  
  # directory should exist
  expect_true(fs::dir_exists(cache_dir))
})

test_that("download_metadata_from_zenodo handles errors gracefully", {
  # test with invalid version that doesn't exist
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  # this should fail for a version that doesn't exist
  expect_error(
    download_metadata_from_zenodo(version = "999.999.999", dest_dir = temp_dir),
    "Version.*not found"
  )
  
  unlink(temp_dir, recursive = TRUE)
})

test_that("download_metadata_from_zenodo error handling works", {
  temp_dir <- tempfile()
  fs::dir_create(temp_dir)
  
  # Test that the function exists and has proper structure
  expect_true(exists("download_metadata_from_zenodo"))
  expect_type(download_metadata_from_zenodo, "closure")
  
  # Test error handling with mocked functions
  testthat::local_mocked_bindings(
    resolve_zenodo_version = function(doi, version, sandbox) {
      if (version == "invalid_version") {
        stop("Version not found")
      }
      return("1.0.0")
    },
    .package = "openesm"
  )
  
  # trigger the resolve_zenodo_version error
  expect_error(
    download_metadata_from_zenodo(version = "invalid_version", dest_dir = temp_dir),
    "Version not found"
  )
  
  unlink(temp_dir, recursive = TRUE)
})


Sys.setenv(NOT_CRAN = "")
