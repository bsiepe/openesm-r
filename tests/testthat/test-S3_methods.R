library(testthat)

test_that("print.openesm_dataset produces correct output", {
  mock_dataset <- create_mock_openesm_dataset()
  output <- cli::cli_fmt(print(mock_dataset))
  
  # check for key components of the print output
  expect_true(any(grepl("Version:", output)))
  expect_true(any(grepl("Authors:", output)))
  expect_true(any(grepl("Data:", output)))
})

test_that("print.openesm_dataset_list produces correct output", {
  mock_list <- list(
    "0001_test" = create_mock_openesm_dataset(),
    "0002_test" = create_mock_openesm_dataset()
  )
  class(mock_list) <- c("openesm_dataset_list", "list")
  
  output <- cli::cli_fmt(print(mock_list))
  expect_true(any(grepl("Collection of", output)))
  expect_true(any(grepl("0001_test", output)))
  expect_true(any(grepl("0002_test", output)))
})

test_that("cite.openesm_dataset works correctly", {
  mock_dataset <- create_mock_openesm_dataset()
  
  # test the printed output
  output <- cli::cli_fmt(cite(mock_dataset))
  expect_true(any(grepl("To cite this dataset", output)))
  expect_true(any(grepl(mock_dataset$metadata$reference_a, output, fixed = TRUE)))
  
  # test the invisible return value
  returned_value <- suppressMessages(cite(mock_dataset))
  expect_type(returned_value, "character")
  expect_match(returned_value, mock_dataset$metadata$reference_a, fixed = TRUE)
})

test_that("notes.openesm_dataset works correctly", {
  mock_dataset <- create_mock_openesm_dataset()
  
  # test the printed output
  output <- cli::cli_fmt(notes(mock_dataset))
  expect_true(any(grepl("some individuals have more", output)))
  
  # test the invisible return value
  returned_value <- suppressMessages(notes(mock_dataset))
  expect_type(returned_value, "character")
  expect_length(returned_value, 1)
  expect_match(returned_value, "some individuals have more")
})


