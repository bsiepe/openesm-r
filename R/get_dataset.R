#' Download ESM dataset(s) from OpenESM repository
#'
#' Downloads one or more Experience Sampling Method (ESM) datasets from the 
#' OpenESM repository hosted on Zenodo. Returns an S3 object containing the 
#' dataset and associated metadata.
#'
#' @param dataset_id Character string or vector of dataset IDs. Use 
#'   \code{\link{list_datasets}} to see available datasets.
#' @param version Character string specifying the dataset version. Default is 
#'   "latest" which downloads the most recent version.
#' @param path Character string specifying custom download path. If \code{NULL} 
#'   (default), files are cached in the user's cache directory.
#' @param cache Logical. If \code{TRUE} (default), uses cached version if 
#'   available and not expired.
#' @param force_download Logical. If \code{TRUE}, forces re-download even if 
#'   cached version exists. Default is \code{FALSE}.
#' @param sandbox Logical. If \code{TRUE}, uses Zenodo sandbox environment 
#'   for testing. Default is \code{FALSE}.
#' @param quiet Logical. If \code{TRUE}, suppresses informational messages. 
#'   Default is \code{FALSE}.
#' @param ... Additional arguments passed to \code{\link{list_datasets}}. This
#'   includes \code{metadata_version} to specify the metadata catalog version.
#'
#' @return For single dataset: An S3 object of class \code{openesm_dataset}
#'   containing:
#'   \itemize{
#'     \item \code{data}: A tibble with the ESM data
#'     \item \code{metadata}: List with dataset metadata
#'     \item \code{dataset_id}: Character string with dataset identifier
#'     \item \code{dataset_version}: Character string with dataset version number
#'     \item \code{metadata_version}: Character string with metadata catalog version
#'   }
#'   For multiple datasets: An S3 object of class \code{openesm_dataset_list}
#'   containing a named list of \code{openesm_dataset} objects.
#'
#' @details
#' This function downloads ESM datasets from Zenodo using DOIs stored in the 
#' OpenESM metadata repository. Datasets are cached locally to avoid repeated 
#' downloads. Use \code{force_download = TRUE} to refresh cached data.
#' 
#' The function handles both individual datasets and batch downloads. When 
#' downloading multiple datasets, progress is shown for each download.
#'
#' @seealso 
#' \code{\link{list_datasets}} for available datasets,
#' \code{\link{cite}} for citation information,
#' \code{\link{license}} for license details
#'
#' @importFrom cli cli_abort cli_alert_success
#' @importFrom readr read_tsv
#' @importFrom fs file_exists path
#'
#' @examples
#' \dontrun{
#' # List available datasets first
#' available <- list_datasets()
#' head(available)
#' 
#' # Download a single dataset
#' dataset <- get_dataset("0001")
#' 
#' # Access the data
#' head(dataset$data)
#' 
#' # View metadata and provenance information
#' dataset$metadata
#' dataset$dataset_version  # Dataset version  
#' dataset$metadata_version # Metadata catalog version
#' 
#' # Download multiple datasets
#' datasets <- get_dataset(c("0001", "0002"))
#' 
#' # Access individual datasets from the list
#' datasets[["0001"]]$data
#' 
#' # Use specific metadata catalog version
#' dataset_v1 <- get_dataset("0001", metadata_version = "1.0.0")
#' 
#' # Force re-download to get latest version
#' dataset_fresh <- get_dataset("0001", force_download = TRUE)
#' 
#' # Download to custom path
#' dataset_custom <- get_dataset("0001", path = "~/my_data")
#' }
#'
#' @export
get_dataset <- function(dataset_id,
                        version = "latest",
                        cache = TRUE,
                        path = NULL,
                        force_download = FALSE,
                        sandbox = FALSE,
                        quiet = FALSE,
                        ...) { 
  
  # handle multiple datasets
  if (length(dataset_id) > 1) {
    return(get_multiple_datasets(
      dataset_id, version, cache, force_download, sandbox, ...)
      )
  }
  
  # remove all non-numeric characters from dataset_id
  dataset_id <- gsub("[^0-9]", "", dataset_id)
  
  # resolve metadata version to track provenance
  metadata_doi <- "10.5281/zenodo.17182171"
  dots <- list(...)
  metadata_version_requested <- dots$metadata_version %||% "latest"
  resolved_metadata_version <- resolve_zenodo_version(
    metadata_doi, metadata_version_requested, sandbox = FALSE
    )
  # get dataset catalog
  all_datasets <- list_datasets(...)
  if (!dataset_id %in% all_datasets$dataset_id) {
    cli::cli_abort("Dataset with id {.val {dataset_id}} not found.")
  }
  # get dataset info
  # remove any NA ids
  all_datasets <- all_datasets[!is.na(all_datasets[["dataset_id"]]), ]
  dataset_info <- all_datasets[all_datasets[["dataset_id"]] == dataset_id, ]
  
  author_lower <- tolower(dataset_info$first_author)
  
  # get metadata from github
  metadata_gh_folder <- paste0(dataset_info$dataset_id, "_", author_lower, "/")
  metadata_gh_path <- paste0(dataset_info$dataset_id, "_", author_lower, "_metadata.json")
  metadata_url <- paste0(
    "https://raw.githubusercontent.com/bsiepe/openesm-metadata/main/datasets/",
    metadata_gh_folder,
    metadata_gh_path
  )
  # cache metadata
  local_metadata_path <- get_cache_path(dataset_id,
                                        filename = metadata_gh_path,
                                        type = "metadata",
                                        version = "latest") # metadata not version specific
  
  if (!fs::file_exists(local_metadata_path) || force_download) {
    download_with_progress(metadata_url, local_metadata_path)
  }
  
  specific_meta_raw <- read_json_safe(local_metadata_path)
  
  # get concept DOI from metadata
  zenodo_doi <- specific_meta_raw$zenodo_doi
  
  if (is.null(zenodo_doi)) {
    cli::cli_abort("No Zenodo DOI found in metadata for dataset {dataset_id}")
  }

  # resolve actual version if "latest" is requested
  actual_version <- resolve_zenodo_version(zenodo_doi, version, sandbox)
  
  # determine cache/destination path
  filename <- paste0(dataset_id, "_", author_lower, "_ts.tsv")
  if (is.null(path)) {
    local_data_path <- get_cache_path(dataset_id,
                                      filename = filename,
                                      type = "data",
                                      version = actual_version)
  } else {
    local_data_path <- fs::path(path, filename)
  }
  
  # download from Zenodo if needed
  if (!fs::file_exists(local_data_path) || force_download) {
    download_from_zenodo(
      zenodo_doi = zenodo_doi,
      dataset_id = dataset_id,
      author_name = author_lower,
      version = actual_version,
      sandbox = sandbox,
      dest_path = local_data_path
    )
  }
  
  # load dataset
  cli::cli_alert_success("Loading dataset {.val {dataset_id}} version {.val {actual_version}}")
  data <- readr::read_tsv(local_data_path, show_col_types = FALSE)
  
  # format metadata for cleaner output
  formatted_meta <- as.list(process_specific_metadata(specific_meta_raw))

  # add metadata and class
  dataset <- structure(
    list(
      data = data,
      metadata = formatted_meta,
      dataset_id = dataset_id,
      dataset_version = actual_version,
      metadata_version = resolved_metadata_version
    ),
    class = "openesm_dataset"
  )
  
  # explicitly print upon download, unless silenced
  if (!quiet) {
    print(dataset)
  }
  
  return(invisible(dataset))
}

#' Helper function for multiple datasets
#' 
#' This function handles downloading multiple datasets by calling
#' \code{\link{get_dataset}} for each dataset ID in the input vector.
#' This is used internally by \code{\link{get_dataset}} when multiple IDs
#' are provided.
#' @param dataset_ids Character vector of dataset IDs to download.
#' @param version Character string specifying the dataset version. Default is
#'   "latest" which downloads the most recent version.
#' @param cache Logical. If \code{TRUE} (default), uses cached version if
#'   available and not expired.
#' @param force_download Logical. If \code{TRUE}, forces re-download even if
#'  cached version exists. Default is \code{FALSE}.
#' @param sandbox Logical. If \code{TRUE}, uses Zenodo sandbox environment
#'    for testing. Default is \code{FALSE}.
#' @keywords internal
#' @noRd
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download,
                                  sandbox,
                                  ...) {
  result <- list()
  for (id in dataset_ids) {
    # call get_dataset in 'quiet' mode to suppress individual prints
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download,
      sandbox = sandbox,
      quiet = TRUE,
      ...
    )
  }
  # assign a special class to the list for custom printing
  result <- structure(result, class = c("openesm_dataset_list", "list"))
  
  # explicitly print the summary for the user
  print(result)
  
  return(invisible(result))
}

