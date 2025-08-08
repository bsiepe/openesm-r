library(testthat)
library(tibble)
library(dplyr)
library(purrr)

# Create mock dataset JSON matching openESM structure
create_mock_dataset_json <- function() {
  list(
    datasets = list(
      list(
        dataset_id = "0001_test",
        first_author = "Test",
        year = 2022L,
        reference_a = "Test et al. (2022)",
        reference_b = "Journal of Test",
        paper_doi = "10.1234/example",
        link_to_zenodo = "https://zenodo.org/record/123456",
        link_to_data = "https://example.com/data",
        link_to_codebook = "https://example.com/codebook",
        link_to_code = "https://example.com/code",
        n_participants = 100L,
        n_time_points = 50L,
        n_beeps_per_day = "5",
        passive_data_available = "No",
        cross_sectional_available = "Yes",
        topics = list("affect", "wellbeing"),
        implicit_missingness = "Yes",
        raw_time_stamp = "2022-01-01",
        sampling_scheme = "Random",
        participants = "Adults",
        coding_file = "coding.txt",
        additional_comments = "Test dataset",
        features = list(
          list(variable = "mood", type = "numeric", scale = "1-7"),
          list(variable = "stress", type = "numeric", scale = "1-5")
        )
      ),
      list(
        dataset_id = "0002_smith",
        first_author = "Smith",
        year = 2023L,
        reference_a = "Smith et al. (2023)",
        reference_b = NULL,  # Test NULL handling
        paper_doi = "10.5678/example",
        link_to_zenodo = "https://zenodo.org/record/789012",
        link_to_data = NULL,
        link_to_codebook = NULL,
        link_to_code = NULL,
        n_participants = 50L,
        n_time_points = 30L,
        n_beeps_per_day = "3-5",
        passive_data_available = NULL,
        cross_sectional_available = "No",
        topics = "emotion",  
        implicit_missingness = NULL,
        raw_time_stamp = "2023-06-01",
        sampling_scheme = "Fixed",
        participants = "Students",
        coding_file = NULL,
        additional_comments = NULL,
        features = list()  # empty features for test
      )
    )
  )
}

test_that("process_raw_datasets_list correctly processes dataset JSON", {
  mock_data <- create_mock_dataset_json()
  result <- process_raw_datasets_list(mock_data)
  
  # basic structure
  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 23)
  
  # First dataset with complete data
  expect_equal(result$dataset_id[1], "0001_test")
  expect_equal(result$year[1], 2022)
  expect_equal(result$n_participants[1], 100)
  expect_equal(result$topics[1], "affect, wellbeing")  # List collapsed to string
  
  # features for first dataset
  features_1 <- result$features[[1]]
  expect_s3_class(features_1, "tbl_df")
  expect_equal(nrow(features_1), 2)
  expect_equal(features_1$variable, c("mood", "stress"))
  
  # Second dataset with NULLs
  expect_equal(result$dataset_id[2], "0002_smith")
  expect_true(is.na(result$reference_b[2]))  # NULL converted to NA
  expect_true(is.na(result$link_to_data[2]))
  expect_equal(result$topics[2], "emotion")  # Single value preserved
  
  # Empty features for second dataset
  features_2 <- result$features[[2]]
  expect_s3_class(features_2, "tbl_df")
  expect_equal(nrow(features_2), 0)
})

test_that("list_datasets downloads and uses cache correctly", {
  # use temporary directory for cache
  temp_cache <- tempfile("cache")
  dir.create(temp_cache, recursive = TRUE)
  
  mock_json_data <- create_mock_dataset_json()
  download_called <- 0
  messages_captured <- character()
  
  testthat::local_mocked_bindings(
    get_cache_dir = function() temp_cache,
    msg_info = function(msg) {
      messages_captured <<- c(messages_captured, msg)
    },
    read_json_safe = function(path) mock_json_data,
    download_with_progress = function(url, path) {
      download_called <<- download_called + 1
      # Create a dummy file to simulate download
      writeLines("{}", path)
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