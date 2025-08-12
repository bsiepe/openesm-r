# Function to list available datasets from the OpenESM metadata repository
#' List Available Datasets
#' #' Retrieves a list of available datasets from the OpenESM metadata repository.
#' It checks for a cached version of the dataset index and downloads a fresh copy if the
#' cache is older than the specified number of hours.
#' @param cache_hours Numeric, number of hours to consider the cache valid (default is 24).
#' @return A tibble with one row per dataset, including a nested tibble with
#'   detailed feature information.
#' @importFrom dplyr bind_rows mutate
#' @importFrom purrr map
#' @export
list_datasets <- function(cache_hours = 24) {
  # define the path to the cached master index file
  index_dir <- get_cache_dir(type = "metadata") # gets ~/.cache/R/openesm
  index_path <- file.path(index_dir, "datasets.json")

  # determine if we need to download a fresh copy
  use_cache <- FALSE
  if (fs::file_exists(index_path)) {
    file_age_hours <- difftime(Sys.time(), file.info(index_path)$mtime, units = "hours")
    if (file_age_hours < cache_hours) {
      msg_info("Using cached dataset index (less than {.val {cache_hours}} hours old).")
      use_cache <- TRUE
    }
  }

  if (!use_cache) {
    # otherwise, download a fresh copy
    msg_info("Downloading fresh dataset index from GitHub.")
    index_url <- "https://raw.githubusercontent.com/bsiepe/openesm-metadata/refs/heads/main/datasets.json"
    download_with_progress(index_url, index_path)
  }

  # read the file and process it
  raw_list <- read_json_safe(index_path)
  return(process_raw_datasets_list(raw_list))
}

#' Process Raw Datasets List
#'
#' Helper function to process the raw list from JSON into a tidy tibble.
#'
#' @param raw_list The raw list parsed from the datasets.json file.
#' @importFrom dplyr bind_rows
#' @importFrom purrr map_dfr
#' @importFrom tibble tibble
#' @return A tibble.
#' @noRd
process_raw_datasets_list <- function(raw_list) {
  datasets_list <- raw_list$datasets

  # iterate over each dataset, applying the same
  # processing function used by get_dataset()
  purrr::map_dfr(datasets_list, process_specific_metadata)
}
