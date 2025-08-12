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
  # replace NULL with NA_character_
  safe_val <- function(v) if (is.null(v)) NA_character_ else v
  
  bullets <- c(
    "*" = "Version: {.val {safe_val(x$version)}}",
    "*" = "Authors: {safe_val(meta$first_author)} et al. ({safe_val(meta$year)})",
    "*" = "Paper DOI: {safe_val(meta$paper_doi)}",
    "*" = "Data: A tibble with {safe_val(meta$n_participants)} participants and {safe_val(meta$n_time_points)} maximum time points per participant"
  )

  cli::cli_bullets(bullets)
  cli::cli_alert_info("Use {.code cite(dataset)} for citation information.")
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