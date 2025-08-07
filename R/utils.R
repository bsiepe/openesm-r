#' Internal utility functions for openesm package
#' @keywords internal

#' Get the openesm cache directory
#' 
#' @param subdir Optional subdirectory within the cache
#' @return Path to cache directory
#' @noRd
get_cache_dir <- function(subdir = NULL) {
  base_dir <- tools::R_user_dir("openesm", which = "cache")
  
  if (!is.null(subdir)) {
    base_dir <- file.path(base_dir, subdir)
  }
  
  # ensure directory exists
  fs::dir_create(base_dir, recurse = TRUE)
  
  return(base_dir)
}

#' Get path to metadata cache
#' @noRd
get_metadata_dir <- function() {
  get_cache_dir("metadata")
}

#' Get path to data cache
#' @noRd
get_data_dir <- function() {
  get_cache_dir("data")
}

#' Check if running in interactive mode with CLI messaging
#' @noRd
is_interactive_cli <- function() {
  interactive() && !isTRUE(getOption("openesm.quiet"))
}

#' Create a consistent message format
#' @noRd
msg_info <- function(...) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_info(...)
  }
}

#' Create a consistent warning format
#' @noRd
msg_warn <- function(...) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_warning(...)
  }
}

#' Create a consistent success format
#' @noRd
msg_success <- function(...) {
  if (!isTRUE(getOption("openesm.quiet"))) {
    cli::cli_alert_success(...)
  }
}

#' Format bytes to human readable
#' @noRd
format_bytes <- function(bytes) {
  if (bytes < 1024) {
    return(paste0(bytes, " B"))
  } else if (bytes < 1024^2) {
    return(paste0(round(bytes / 1024, 1), " KB"))
  } else if (bytes < 1024^3) {
    return(paste0(round(bytes / 1024^2, 1), " MB"))
  } else {
    return(paste0(round(bytes / 1024^3, 2), " GB"))
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
download_with_progress <- function(url, destfile, quiet = FALSE) {
  if (!quiet) {
    cli::cli_progress_step("Downloading from {.url {url}}")
  }
  
  tryCatch({
    response <- httr2::request(url) |>
      httr2::req_progress() |>
      httr2::req_perform()
    
    # Write to file
    writeBin(httr2::resp_body_raw(response), destfile)
    
    if (!quiet) {
      cli::cli_progress_done()
    }
    
    TRUE
  }, error = function(e) {
    if (!quiet) {
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
get_dataset_path <- function(dataset_id, version, type = c("metadata", "data")) {
  type <- match.arg(type)
  
  base_dir <- if (type == "metadata") {
    get_metadata_dir()
  } else {
    get_data_dir()
  }
  
  file.path(base_dir, "datasets", dataset_id, version)
}