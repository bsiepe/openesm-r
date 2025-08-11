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

  # use purrr::map_dfr to iterate over each dataset and build a tibble row by row
  # robust to missing or inconsistent fields
  purrr::map_dfr(datasets_list, function(ds) {
    # helper to safely get a value, converting NULL or empty list to NA
    get_val <- function(field, type = "character") {
      val <- ds[[field]]
      if (is.null(val) || (is.list(val) && length(val) == 0)) {
        if (type == "character") return(NA_character_)
        if (type == "integer") return(NA_integer_)
        return(NA)
      }
      # collapse lists/vectors into a single string if needed
      if (is.list(val) || length(val) > 1) {
        return(paste(val, collapse = ", "))
      }
      return(val)
    }

    # first, create the nested tibble for features
    features_tibble <- if (!is.null(ds$features) && length(ds$features) > 0) {
      dplyr::bind_rows(ds$features)
    } else {
      # if no features, create an empty tibble with the correct columns
      tibble::tibble()
    }

    # create the main tibble for this dataset
    tibble::tibble(
      dataset_id = get_val("dataset_id"),
      first_author = get_val("first_author"),
      year = get_val("year", "integer"),
      reference_a = get_val("reference_a"),
      reference_b = get_val("reference_b"),
      paper_doi = get_val("paper_doi"),
      zenodo_doi = get_val("zenodo_doi"),
      link_to_data = get_val("link_to_data"),
      link_to_codebook = get_val("link_to_codebook"),
      link_to_code = get_val("link_to_code"),
      n_participants = get_val("n_participants", "integer"),
      n_time_points = get_val("n_time_points", "integer"),
      n_beeps_per_day = get_val("n_beeps_per_day"),
      passive_data_available = get_val("passive_data_available"),
      cross_sectional_available = get_val("cross_sectional_available"),
      topics = get_val("topics"),
      implicit_missingness = get_val("implicit_missingness"),
      raw_time_stamp = get_val("raw_time_stamp"),
      sampling_scheme = get_val("sampling_scheme"),
      participants = get_val("participants"),
      coding_file = get_val("coding_file"),
      additional_comments = get_val("additional_comments"),
      features = list(features_tibble) # nest the features tibble
    )
  })
}
