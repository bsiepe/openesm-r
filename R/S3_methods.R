#' S3 Methods for openesm objects

#' Print method for openesm_dataset
#'
#' @param x An object of class `openesm_dataset`.
#' @param ... Additional arguments passed to `print`.
#' @return Invisibly returns the original object.
#' @importFrom cli cli_h1 cli_text cli_alert_info cli_bullets
#' @export
print.openesm_dataset <- function(x, ...) {
  cli::cli_h1("OpenESM Dataset: {.val {x$dataset_id}}")

  meta <- x$metadata
  
  safe_val <- function(v) if (is.null(v)) NA_character_ else v
  
  bullets <- c(
    "*" = "Version: {.val {safe_val(x$version)}}",
    "*" = "Authors: {safe_val(meta$first_author)} et al. ({safe_val(meta$year)})",
    "*" = "Paper DOI: {safe_val(meta$paper_doi)}",
    "*" = "License: {safe_val(meta$license)}",
    "*" = "Data: A tibble with {safe_val(meta$n_participants)} participants and {safe_val(meta$n_time_points)} maximum time points per participant"
  )

  cli::cli_bullets(bullets)
  cli::cli_alert_info("Use {.code cite(dataset)} for citation information.")
  cli::cli_alert_info("Please ensure you follow the license terms for this dataset.")
  invisible(x)
}


#' Print method for a list of openesm_dataset objects
#'
#' @param x An object of class `openesm_dataset_list`.
#' @param ... Additional arguments passed to `print`.
#' @return Invisibly returns the original object.
#' @importFrom cli cli_h1 cli_text cli_bullets cli_alert_info
#' @export
print.openesm_dataset_list <- function(x, ...) {
  num_datasets <- length(x)
  cli::cli_h1("Collection of {num_datasets} OpenESM Dataset{?s}")

  # show the names of the first few datasets
  max_show <- 5
  dataset_names <- names(x)
  
  bullets <- paste0("* ", cli::style_bold(dataset_names[1:min(num_datasets, max_show)]))
  
  if (num_datasets > max_show) {
    bullets <- c(bullets, "  ... and {num_datasets - max_show} more.")
  }
  
  cli::cli_bullets(bullets)
  cli::cli_alert_info("Access individual datasets using {.code list_name$dataset_id}")
  
  invisible(x)
}


#' Cite method for openesm_dataset
#' 
#' @param x An object of class `openesm_dataset`.
#' @param format Character, format for citation (currently only enables "bibtex").
#' @param ... Additional arguments (not used).
#' @return A character string with the citation information.
#' @export
cite.openesm_dataset <- function(x, format = "bibtex", ...) {
  if (tolower(format) != "bibtex") {
    cli::cli_abort(c("Unsupported format: {.val {format}}. Only {.val bibtex} is currently supported."))
  }
  
  meta <- x$metadata
  
  # Helper to check if a reference string is valid (not NULL, NA, or empty)
  is_valid_ref <- function(ref) {
    !is.null(ref) && !is.na(ref) && nzchar(trimws(ref))
  }
  
  citations <- c()
  if (is_valid_ref(meta$reference_a)) {
    citations <- c(citations, meta$reference_a)
  }
  if (is_valid_ref(meta$reference_b)) {
    citations <- c(citations, meta$reference_b)
  }
  
  if (length(citations) == 0) {
    cli::cli_alert_info("No citation information available for this dataset.")
    return(invisible(NULL))
  }
  
  # Format the output similar to the base citation() function
  cli::cli_text("To cite this dataset in publications, please use:")
  
  full_citation_string <- paste(citations, collapse = "\n\n")
  
  # Print the bibtex string(s) directly to the console
  cat("\n")
  cat(full_citation_string)
  cat("\n")
  
  # Return the combined string invisibly
  return(invisible(full_citation_string))
}