library(testthat)
Sys.setenv(NOT_CRAN = "true")
# use a real, public concept DOI from the Zenodo sandbox for testing
# this record has multiple versions
test_doi <- "10.5072/zenodo.308201"

test_that("resolve_zenodo_version works with a live sandbox request", {
  skip_on_cran()
  skip_on_ci()
  # test resolving the latest version
  latest_version <- resolve_zenodo_version(test_doi, version = "latest", sandbox = TRUE)
  expect_equal(latest_version, "1.0.2")
  
  # test resolving a specific, existing version
  specific_version <- resolve_zenodo_version(test_doi, version = "1.0.0", sandbox = TRUE)
  expect_equal(specific_version, "1.0.0")
  
  # test error for a non-existent version
  expect_error(
    resolve_zenodo_version(test_doi, version = "v9.9.9", sandbox = TRUE),
    "Version v9.9.9 not found"
  )
})

test_that("download_from_zenodo works with a live sandbox request", {
  skip_on_cran()
  skip_on_ci()
  # create a temporary file path for the download
  temp_dest <- tempfile(fileext = ".tsv")
  
  # perform the download using a version we know exists
  result_path <- download_from_zenodo(
    zenodo_doi = test_doi,
    dataset_id = "0001",
    author_name = "test",
    version = "1.0.0",
    sandbox = TRUE,
    dest_path = temp_dest
  )
  
  # check that the file was downloaded and the path is correct
  expect_equal(result_path, temp_dest)
  expect_true(file.exists(temp_dest))
  expect_gt(file.info(temp_dest)$size, 0)
  
  # clean up the downloaded file
  unlink(temp_dest)
})

Sys.setenv(NOT_CRAN = "")
