#' Get a dataset from OpenESM
#'
#' @param dataset_id Character string or vector of dataset IDs
#' @param version Character string specifying the version (default is "latest")
#' @param path Character string specifying the path to save the dataset (default is NULL)
#' @param cache Logical, if TRUE uses cached version if available (default is TRUE)
#' @param force_download Logical, if TRUE forces re-download even if cached version exists (
#'   default is FALSE)
#' @return A data frame with the dataset, or a list of data frames for multiple
#'   datasets
#'   
#' @importFrom cli cli_abort 
#' @importFrom readr read_tsv
#' @examples
#' \dontrun{
#' # Get a single dataset
#' dataset <- get_dataset("example_dataset_id")
#'
#' # Get multiple datasets
#' datasets <- get_dataset(c("dataset1", "dataset2"))
#' }
#' @export
get_dataset <- function(dataset_id,
                        version = NULL,
                        cache = TRUE,
                        path = NULL,
                        force_download = FALSE) {
  # use default version if not specified
  if (is.null(version) || version == "latest") {
    version <- "latest"
  } else {
    cli::cli_abort("Versioning is not yet implemented. Please use 'latest' version for now.")
  }
  
  # handle multiple datasets
  if (length(dataset_id) > 1) {
    return(get_multiple_datasets(dataset_id, version, cache, force_download))
  }
  
  # first, get the main catalog of all available datasets
  all_datasets <- list_datasets()
  if (!dataset_id %in% all_datasets$dataset_id) {
    cli::cli_abort("Dataset with id {.val {dataset_id}} not found.")
  }
  
  # get the info for the specific dataset
  dataset_info <- all_datasets[all_datasets$dataset_id == dataset_id, ]
  
  # use the info from the index to find the specific metadata file
  metadata_gh_path <- paste0(dataset_info$dataset_id, "_", dataset_info$author)
  metadata_url <- paste0(
    "https://raw.githubusercontent.com/bsiepe/openesm-metadata/main/datasets/",
    metadata_gh_path
  )
  metadata_filename <- fs::path_file(metadata_gh_path)
  
  # get cache path
  local_metadata_path <- get_cache_path(dataset_id,
                                        filename = metadata_filename,
                                        type = "metadata",
                                        version = version)
  
  if (!fs::file_exists(local_metadata_path)) {
    download_with_progress(metadata_url, local_metadata_path)
  }
  
  specific_meta <- read_json_safe(local_metadata_path)
  
  # use the specific metadata to find and download the data file from zenodo
  # TODO this still needs to be checked with actual metadata fields
  zenodo_url <- specific_meta$link_to_zenodo
  data_filename <- specific_meta$data_filename

  # Mocking for testing when no real zenodo link is available
  if (is.null(zenodo_url) || zenodo_url == "") {
    cli::cli_alert_info("Using mock download link for testing.")
    zenodo_url <- "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv"
    data_filename <- "iris.csv"
  }
  
  if(is.null(path)){
    local_data_path <- get_cache_path(dataset_id, filename = data_filename, type = "data")
  } else {
    local_data_path <- fs::path(path, data_filename)
  }
  
  
  if (!fs::file_exists(local_data_path)) {
    download_with_progress(zenodo_url, local_data_path)
  }
  
  # dataset loading and metadata handling
  msg_success("Loading dataset {.val {dataset_id}}.")
  
  # Adjusting reader based on file type for mock data
  if (tools::file_ext(data_filename) == "csv") {
    data <- readr::read_csv(local_data_path, show_col_types = FALSE)
  } else {
    data <- readr::read_tsv(local_data_path, show_col_types = FALSE)
  }
  
  attr(data, "metadata") <- specific_meta
  class(data) <- c("openesm_dataset", class(data))
  
  return(data)
}

# Helper function for multiple datasets
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download) {
  result <- list()
  for (id in dataset_ids) {
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download
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
