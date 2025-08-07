#' Internal constructor for openESM dataset objects
#'
#' This function is used internally by \code(\link(get_dataset)) to create
#' openesm_dataset objects. Users should not call this directly.
#'
#' @param data The dataset as a data frame or tibble
#' @param metadata List containing dataset metadata
#' @param dataset_id Character string identifying the dataset
#' @param version Character string specifying the version
#' @return An object of class "openesm_dataset"
#' @keywords internal
#' @noRd
new_openesm_dataset <- function(data = NULL, 
                                metadata = list(), 
                                dataset_id = character(), 
                                version = character()) {
  
  # Validate inputs
  if (!is.null(data) && !is.data.frame(data)) {
    cli::cli_abort("Data must be a data frame or NULL")
  }
  
  if (!is.list(metadata)) {
    cli::cli_abort("Metadata must be a list")
  }
  
  if (length(dataset_id) != 1 || !is.character(dataset_id)) {
    cli::cli_abort("dataset_id must be a single character string")
  }
  
  if (length(version) != 1 || !is.character(version)) {
    cli::cli_abort("version must be a single character string")
  }
  
  # extract metadata
  citation <- metadata$citation %||% list()
  license <- metadata$license %||% list()
  notes <- metadata$notes %||% character()
  variables <- metadata$variables %||% data.frame()
  
  # object creation
  structure(
    list(
      data = data,
      metadata = metadata,
      dataset_id = dataset_id,
      version = version,
      citation = citation,
      license = license,
      notes = notes,
      variables = variables,
      downloaded = !is.null(data)
    ),
    class = c("openesm_dataset", "list")
  )
}

#' Check if object is an openesm_dataset
#'
#' @param x Object to test
#' @return Logical
#' @export
is_openesm_dataset <- function(x) {
  inherits(x, "openesm_dataset")
}

#' Validate openesm_dataset object
#' 
#' @param x Object to validate
#' @keywords internal
validate_openesm_dataset <- function(x) {
  if (!is_openesm_dataset(x)) {
    cli::cli_abort("Object is not an openesm_dataset")
  }
  
  required_fields <- c("data", "metadata", "dataset_id", "version", 
                       "citation", "license", "notes", "variables", "downloaded")
  
  missing_fields <- setdiff(required_fields, names(x))
  if (length(missing_fields) > 0) {
    cli::cli_abort(c(
      "Invalid openesm_dataset object",
      "x" = "Missing fields: {.field {missing_fields}}"
    ))
  }
  
  invisible(x)
}

# Helper for NULL-coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}