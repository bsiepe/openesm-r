library(testthat)

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
