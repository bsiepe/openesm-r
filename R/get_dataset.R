#' Get a dataset from OpenESM
#'
#' @param dataset_id Character string or vector of dataset IDs
#' @param version Character string specifying the version (default is "latest")
#' @param quiet Logical, if TRUE suppresses progress messages
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
#' \donttest{
#' # Get a single dataset
#' dataset <- get_dataset("example_dataset_id")
#'
#' # Get multiple datasets
#' datasets <- get_dataset(c("dataset1", "dataset2"))
#' }
#' @export
get_dataset <- function(dataset_id, version = NULL, cache = TRUE, force_download = FALSE) {
  
  # handle multiple datasets
  if (length(dataset_id) > 1) {
    return(get_multiple_datasets(dataset_id, version, cache, force_download, quiet))
  }
  
  # get metadata (cached if possible)
  all_datasets <- get_cached_metadata()
  
  # find the specific dataset
  dataset_meta <- all_datasets[all_datasets$dataset == dataset_id, ]
  if (nrow(dataset_meta) == 0) {
    cli::cli_abort("Dataset '{dataset_id}' not found. Use {.fn list_datasets} to see available datasets.")
  }
  
  # use default version if not specified
  if (is.null(version)) {
    version <- "latest"  
  }
  
  # construct cache file path
  data_dir <- get_data_dir()
  dataset_filename <- paste0(dataset_meta$first_author[1], "_", dataset_id, ".tsv")
  cache_file <- file.path(data_dir, dataset_filename)
  
  # check if we need to download
  need_download <- force_download || !cache || !file.exists(cache_file)
  
  if (need_download) {
    # get zenodo download url
    download_url <- construct_zenodo_download_url(dataset_meta$link_to_zenodo)
    
    msg_info("Downloading dataset {dataset_id} ({dataset_meta$first_author[1]} et al., {dataset_meta$year[1]})")
    
    # download with progress
    download_with_progress(download_url, cache_file, quiet = !is_interactive_cli())
    
    msg_success("Dataset {dataset_id} downloaded successfully")
  } else {
    msg_info("Loading cached dataset {dataset_id}")
  }
  
  # load the data
  data <- readr::read_tsv(cache_file, show_col_types = FALSE)
  
  # add metadata as attributes
  attr(data, "openesm_dataset_id") <- dataset_id
  attr(data, "openesm_first_author") <- dataset_meta$first_author[1]
  attr(data, "openesm_year") <- dataset_meta$year[1]
  attr(data, "openesm_citation") <- dataset_meta$reference_a[1]
  attr(data, "openesm_doi") <- dataset_meta$paper_doi[1]
  attr(data, "openesm_version") <- version
  
  # set class for S3 methods
  class(data) <- c("openesm_dataset", class(data))
  
  return(data)
}

# Helper function for multiple datasets
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download,
                                  quiet = FALSE) {
  result <- list()
  for (id in dataset_ids) {
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download,
      quiet = quiet
    )
  }
  return(result)
}

# Helper function to get cached metadata
get_cached_metadata <- function() {
  metadata_dir <- get_metadata_dir()
  metadata_file <- file.path(metadata_dir, "datasets.json")
  
  # use cached metadata if it exists and is recent (less than 24 hours old)
  if (file.exists(metadata_file)) {
    file_age <- difftime(Sys.time(), file.mtime(metadata_file), units = "hours")
    if (file_age < 24) {
      metadata <- read_json_safe(metadata_file)
      return(metadata$datasets)
    }
  }
  
  # otherwise fetch fresh metadata
  return(list_datasets(refresh = TRUE))
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
