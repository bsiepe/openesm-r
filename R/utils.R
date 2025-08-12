#' Internal utility functions for openesm package
#' @keywords internal

#' Get the openesm cache directory
#' 
#' @param subdir Optional subdirectory within the cache
#' @return Path to cache directory
#' @noRd
get_cache_dir <- function(type = NULL) {
  # user cache directory
  base_cache <- tools::R_user_dir("openesm", which = "cache")
  
  # if a type is specified, return the subdirectory
  if (!is.null(type)) {
    cache_dir <- fs::path(base_cache, type)
  } else {
    cache_dir <- base_cache
  }
  
  # ensure directory exists
  if (!fs::dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  }
  return(cache_dir)
}

#' Display information about the openesm cache
#'
#' Shows the location and total size of the local file cache.
#'
#' @importFrom cli cli_alert_info
#' @importFrom fs dir_exists dir_info fs_bytes
#' @export
cache_info <- function() {
  cache_dir <- get_cache_dir()
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist yet.")
    cli::cli_alert_info("It will be created at: {.path {cache_dir}}")
    return(invisible(NULL))
  }
  
  dir_contents <- fs::dir_info(cache_dir, recurse = TRUE)
  total_size <- sum(dir_contents$size)
  
  cli::cli_alert_info("Cache location: {.path {cache_dir}}")
  cli::cli_alert_info("Cache size: {.val {fs::fs_bytes(total_size)}}")
}

#' Clear the openesm cache
#'
#' Removes all cached openesm data from your local machine.
#'
#' @param force Logical, if TRUE, will not ask for confirmation before deleting.
#'   Defaults to FALSE.
#' @importFrom cli cli_abort cli_alert_info cli_alert_success
#' @importFrom fs dir_exists
#' @importFrom utils askYesNo
#' @export
clear_cache <- function(force = FALSE) {
  cache_dir <- get_cache_dir()
  if (!fs::dir_exists(cache_dir)) {
    cli::cli_alert_info("Cache directory does not exist. Nothing to clear.")
    return(invisible(NULL))
  }

  confirmed <- FALSE
  if (force) {
    confirmed <- TRUE
  } else if (interactive()) {
    cli::cli_alert_info("This will delete all cached data at {.path {cache_dir}}")
    # use the base R function for confirmation
    confirmed <- utils::askYesNo("Are you sure you want to proceed?", default = FALSE)
  } else {
    cli::cli_abort("Cannot ask for confirmation in a non-interactive session. Use {.code clear_cache(force = TRUE)}.")
  }

  # askYesNo can return NA if the user cancels, so check specifically for TRUE
  if (isTRUE(confirmed)) {
    unlink(cache_dir, recursive = TRUE)
    cli::cli_alert_success("Cache cleared.")
  } else {
    cli::cli_alert_info("Cache not cleared.")
  }

  return(invisible(NULL))
}

#' Get path to metadata cache
#' @noRd
get_metadata_dir <- function() {
  get_cache_dir(type = "metadata")
}

#' Get path to data cache
#' @noRd
get_data_dir <- function() {
  get_cache_dir(type = "data")
}

#' Check if running in interactive mode with CLI messaging
#' @noRd
is_interactive_cli <- function() {
  interactive() && !isTRUE(getOption("openesm.quiet"))
}

#' Create a consistent message format
#' @noRd
msg_info <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_info(..., .envir = .envir)
  }
}

#' Create a consistent warning format
#' @noRd
msg_warn <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_warning(..., .envir = .envir)
  }
}

#' Create a consistent success format
#' @noRd
msg_success <- function(..., .envir = parent.frame()) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_success(..., .envir = .envir)
  }
}


#' Read JSON with error handling
#' @noRd
read_json_safe <- function(path) {
  tryCatch({
    jsonlite::fromJSON(path, simplifyVector = FALSE)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to read JSON file",
      "x" = "Path: {.path {path}}",
      "i" = "Error: {e$message}"
    ))
  })
}

#' Download file with progress
#' @noRd
download_with_progress <- function(url, destfile) {
  if (is_interactive_cli()) {
    cli::cli_progress_step("Downloading from {.url {url}}")
  }
  
  tryCatch({
    response <- httr2::request(url) |>
      httr2::req_progress() |>
      httr2::req_perform()
    
    # Write to file
    writeBin(httr2::resp_body_raw(response), destfile)
    
    if (is_interactive_cli()) {
      cli::cli_progress_done()
    }
    
    TRUE
  }, error = function(e) {
    if (is_interactive_cli()) {
      cli::cli_progress_done(result = "failed")
    }
    cli::cli_abort(c(
      "Download failed",
      "x" = "URL: {.url {url}}",
      "i" = "Error: {e$message}"
    ))
  })
}

#' Construct dataset path
#' @noRd
get_cache_path <- function(dataset_id,
                           version,
                           filename,
                           type = c("metadata", "data")) {
  type <- match.arg(type)
  base_dir <- if (type == "metadata")
    get_metadata_dir()
  else
    get_data_dir()
  
  # simplified path
  path <- fs::path(base_dir, dataset_id, version, filename)
  
  # ensure the directory for the file exists before returning the path
  fs::dir_create(fs::path_dir(path))
  
  return(path)
}

#' Process Specific Dataset Metadata
#'
#' Helper function to process the raw list from a specific dataset's
#' metadata JSON into a clean, one-row tibble.
#'
#' @param raw_meta The raw list parsed from the metadata json file.
#' @return A one-row tibble.
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @noRd
process_specific_metadata <- function(raw_meta) {
  # helper to safely get a value, converting NULL or empty list to NA
  get_val <- function(field, type = "character") {
    val <- raw_meta[[field]]
    if (is.null(val) || (is.list(val) && length(val) == 0)) {
      if (type == "character") return(NA_character_)
      if (type == "integer") return(NA_integer_)
      return(NA)
    }
    if (is.list(val) || length(val) > 1) {
      return(paste(val, collapse = ", "))
    }
    return(val)
  }

  # create the nested tibble for features
  features_tibble <- if (!is.null(raw_meta$features) && length(raw_meta$features) > 0) {
    dplyr::bind_rows(raw_meta$features)
  } else {
    tibble::tibble()
  }

  # create a clean, one-row tibble of all metadata fields
  tibble::tibble(
    dataset_id = get_val("dataset_id"),
    first_author = get_val("first_author"),
    year = get_val("year", "integer"),
    reference_a = get_val("reference_a"),
    reference_b = get_val("reference_b"),
    paper_doi = get_val("paper_doi"),
    zenodo_doi = get_val("zenodo_doi"),
    license = get_val("license"),
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
    features = list(features_tibble)
  )
}

