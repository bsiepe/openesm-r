#' Internal utility functions for openesm package
#' @keywords internal

#' Get the openesm cache directory
#' 
#' @param subdir Optional subdirectory within the cache
#' @return Path to cache directory
#' @noRd
get_cache_dir <- function(type) {
  # user cache directory
  base_cache <- tools::R_user_dir("openesm", which = "cache")
  
  cache_dir <- fs::path(base_cache, type)
  
  # ensure directory exists
  if (!fs::dir_exists(cache_dir)) {
    fs::dir_create(cache_dir, recurse = TRUE)
  }
  
  return(cache_dir)
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