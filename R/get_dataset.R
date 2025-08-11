#' Get a dataset from OpenESM
#'
#' Downloads and loads a dataset from the OpenESM database. Data is retrieved
#' from Zenodo and cached locally for future use.
#'
#' @param dataset_id Character string or vector of dataset IDs to download
#' @param version Character string specifying which version to download ("latest" for most recent)
#' @param path Character string specifying where to save the dataset (uses cache if NULL)
#' @param cache Logical, whether to use cached version if available
#' @param force_download Logical, whether to re-download even if file exists locally
#' @param sandbox Logical, whether to use Zenodo sandbox (for package testing only)
#' @return A data frame containing the dataset, or a list of data frames for multiple datasets
#' @export
get_dataset <- function(dataset_id,
                        version = "latest",
                        cache = TRUE,
                        path = NULL,
                        force_download = FALSE,
                        sandbox = FALSE) {
  
  # handle multiple datasets
  if (length(dataset_id) > 1) {
    return(get_multiple_datasets(dataset_id, version, cache, force_download, sandbox))
  }
  
  # get dataset catalog
  all_datasets <- list_datasets()
  if (!dataset_id %in% all_datasets$dataset_id) {
    cli::cli_abort("Dataset with id {.val {dataset_id}} not found.")
  }
  
  # get dataset info
  # remove any NA ids
  all_datasets <- all_datasets[!is.na(all_datasets[["dataset_id"]]), ]
  dataset_info <- all_datasets[all_datasets[["dataset_id"]] == dataset_id, ]
  
  author_lower <- tolower(dataset_info$first_author)
  
  # get metadata
  metadata_gh_folder <- paste0(dataset_info$dataset_id, "_", author_lower, "/")
  metadata_gh_path <- paste0(dataset_info$dataset_id, "_", author_lower,  "_metadata.json")
  metadata_url <- paste0(
    "https://raw.githubusercontent.com/bsiepe/openesm-metadata/main/datasets/",
    metadata_gh_folder, metadata_gh_path
  )
  
  # cache metadata
  local_metadata_path <- get_cache_path(dataset_id,
                                        filename = metadata_gh_path,
                                        type = "metadata",
                                        version = version)
  
  if (!fs::file_exists(local_metadata_path) || force_download) {
    download_with_progress(metadata_url, local_metadata_path)
  }
  
  specific_meta <- read_json_safe(local_metadata_path)
  
  # get concept DOI from metadata
  zenodo_doi <- specific_meta$zenodo_doi
  
  if (is.null(zenodo_doi)) {
    cli::cli_abort("No Zenodo DOI found in metadata for dataset {dataset_id}")
  }
  
  # determine cache/destination path
  filename <- paste0(dataset_id, "_", author_lower, "_ts.tsv")
  if (is.null(path)) {
    local_data_path <- get_cache_path(dataset_id, filename = filename, type = "data", version = version)
  } else {
    local_data_path <- fs::path(path, filename)
  }
  
  # download from Zenodo if needed
  if (!fs::file_exists(local_data_path) || force_download) {
    download_from_zenodo(
      zenodo_doi = zenodo_doi,
      dataset_id = dataset_id,
      author_name = author_lower,
      version = version,
      sandbox = sandbox,
      dest_path = local_data_path
    )
  }
  
  # load dataset
  cli::cli_alert_success("Loading dataset {.val {dataset_id}}")
  data <- readr::read_tsv(local_data_path, show_col_types = FALSE)
  
  # add metadata and class
  attr(data, "metadata") <- specific_meta
  class(data) <- c("openesm_dataset", class(data))
  
  return(data)
}

# helper function for multiple datasets
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download,
                                  sandbox) {
  result <- list()
  for (id in dataset_ids) {
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download,
      sandbox = sandbox
    )
  }
  return(result)
}




# Helper function to construct zenodo download URL
construct_zenodo_download_url <- function(zenodo_url, version = NULL) {
  if (is.null(zenodo_url) || zenodo_url == "") {
    cli::cli_abort("No Zenodo URL available for this dataset")
  }

  # for now, we assume the zenodo_url is the direct download link
  # versioning is also not implemented yet
  
  return(zenodo_url)
}
