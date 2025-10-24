library(testthat)

test_that("list_datasets downloads and uses cache correctly", {
  skip_on_cran()
  skip_on_ci()
  # use temporary directory for cache
  temp_cache <- tempfile("cache")
  dir.create(temp_cache, recursive = TRUE)
  
  # get the mock data as a JSON string first
  mock_json_string <- create_mock_dataset_json()
  # then parse it into the list structure that the real function would return
  mock_parsed_list <- jsonlite::fromJSON(mock_json_string, simplifyVector = FALSE)
  
  download_called <- 0
  messages_captured <- character()
  
  testthat::local_mocked_bindings(
    get_cache_dir = function(type = NULL) {
      if (is.null(type)) {
        temp_cache
      } else {
        file.path(temp_cache, type)
      }
    },
    resolve_zenodo_version = function(doi, version, sandbox = FALSE, max_attempts = 15) {
      # return a mock version string
      "v1.0.0"
    },
    download_metadata_from_zenodo = function(version, dest_dir, max_attempts = 15) {
      download_called <<- download_called + 1
      # simulate download by writing the string to the path
      index_path <- file.path(dest_dir, "datasets.json")
      dir.create(dirname(index_path), recursive = TRUE, showWarnings = FALSE)
      writeLines(mock_json_string, index_path)
      return(index_path)
    },
    msg_info = function(msg) {
      messages_captured <<- c(messages_captured, msg)
    },
    # this mock should return a parsed list, not raw string
    read_json_safe = function(path) mock_parsed_list
  )
  
  # First call should download (mock download only here)
  result1 <- list_datasets()
  expect_equal(download_called, 1)
  expect_true(any(grepl("Downloading fresh metadata index from Zenodo", messages_captured)))
  expect_s3_class(result1, "tbl_df")
  expect_equal(nrow(result1), 2)
  
  # Reset message capture
  messages_captured <- character()
  
  # Second call should use cache
  result2 <- list_datasets()
  expect_equal(download_called, 1)  # Should still be 1
  expect_true(any(grepl("Using cached dataset index", messages_captured)))
  expect_identical(result1, result2)
  
  # reset message capture
  messages_captured <- character()
  
  # Force refresh with cache_hours = 0
  result3 <- list_datasets(cache_hours = 0)
  expect_equal(download_called, 2)  # Should increment
  expect_true(any(grepl("Downloading fresh metadata index from Zenodo", messages_captured)))
  expect_identical(result1, result3)
  
  # Clean up
  unlink(temp_cache, recursive = TRUE)
})
