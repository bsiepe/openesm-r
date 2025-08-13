library(testthat)

test_that("get_dataset downloads and processes a single dataset", {
  # Mock all external interactions
  testthat::local_mocked_bindings(
    list_datasets = function() {
      # parse the JSON string into a list before processing
      json_string <- create_mock_dataset_json()
      raw_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      # Now call the processor with the correct list object
      openesm:::process_raw_datasets_list(raw_list)
    },
    resolve_zenodo_version = function(...) "1.0.0",
    download_from_zenodo = function(..., dest_path) {
      # Simulate the downloaded file
      write.csv(data.frame(x = 1:5), dest_path)
    },
    get_cache_path = function(...) tempfile()
  )
  
  # Use capture_output to suppress the print method's output during the test
  dataset <- suppressMessages(capture.output(
    ds <- get_dataset("0001")
  ))
  
  expect_s3_class(ds, "openesm_dataset")
  expect_equal(ds$dataset_id, "0001")
  expect_equal(ds$version, "1.0.0")
  expect_equal(nrow(ds$data), 5)
})

test_that("get_multiple_datasets works correctly", {
  # mock the helper function directly
  testthat::local_mocked_bindings(
    get_multiple_datasets = function(dataset_ids, ...) {
      datasets <- lapply(dataset_ids, function(id) {
        structure(list(dataset_id = id), class = "openesm_dataset")
      })
      names(datasets) <- dataset_ids
      structure(datasets, class = c("openesm_dataset_list", "list"))
    }
  )
  datasets <- get_dataset(c("ds1", "ds2"))

  expect_s3_class(datasets, "openesm_dataset_list")
  expect_length(datasets, 2)
  expect_equal(names(datasets), c("ds1", "ds2"))
})

test_that("get_multiple_datasets can be called explicitly", {
  # Mock the actual dataset retrieval
  testthat::local_mocked_bindings(
    get_dataset = function(dataset_id, ...) {
      structure(list(dataset_id = dataset_id), class = "openesm_dataset")
    }
  )
  
  datasets <- get_multiple_datasets(c("ds1", "ds2"))
  
  expect_s3_class(datasets, "openesm_dataset_list")
  expect_length(datasets, 2)
  expect_equal(names(datasets), c("ds1", "ds2"))
})

test_that("get_dataset errors for non-existent dataset", {
  testthat::local_mocked_bindings(
    list_datasets = function() {
      json_string <- create_mock_dataset_json()
      raw_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
      openesm:::process_raw_datasets_list(raw_list)
    }
  )
  expect_error(get_dataset("non_existent_id"), "not found")
})

