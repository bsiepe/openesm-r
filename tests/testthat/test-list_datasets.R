library(testthat)

test_that("list_datasets downloads and uses cache correctly", {
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
    get_cache_dir = function() temp_cache,
    msg_info = function(msg) {
      messages_captured <<- c(messages_captured, msg)
    },
    # this mock should return a parsed list, not raw string
    read_json_safe = function(path) mock_parsed_list,
    download_with_progress = function(url, path) {
      download_called <<- download_called + 1
      # simulate download by writing the string to the path
      writeLines(mock_json_string, path)
      TRUE
    }
  )
  
  # First call should download (mock download only here)
  result1 <- list_datasets()
  expect_equal(download_called, 1)
  expect_true(any(grepl("Downloading fresh dataset index", messages_captured)))
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
  expect_true(any(grepl("Downloading fresh dataset index", messages_captured)))
  expect_identical(result1, result3)
  
  # Clean up
  unlink(temp_cache, recursive = TRUE)
})
