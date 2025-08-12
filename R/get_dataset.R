#' Get a dataset from OpenESM
#'
#' @param dataset_id Character string or vector of dataset IDs
#' @param version Character string specifying the version (default is "latest")
#' @param path Character string specifying the path to save the dataset (default is NULL)
#' @param cache Logical, if TRUE uses cached version if available (default is TRUE)
#' @param force_download Logical, if TRUE forces re-download even if cached version exists (
#'   default is FALSE)
#' @param sandbox Logical, if TRUE uses Zenodo sandbox for testing (default is FALSE)
#' @param quiet Logical, if TRUE suppresses printing of dataset information (default is FALSE)
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
                        version = "latest",
                        cache = TRUE,
                        path = NULL,
                        force_download = FALSE,
                        sandbox = FALSE,
                        quiet = FALSE) { 
  
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
  
  # get metadata from github
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
                                        version = "latest") # metadata is not version specific
  
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
    local_data_path <- get_cache_path(dataset_id, filename = filename, type = "data", version = actual_version)
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
      version = actual_version
    ),
    class = "openesm_dataset"
  )
  
  # explicitly print upon download, unless silenced
  if (!quiet) {
    print(dataset)
  }
  
  return(invisible(dataset))
}

# helper function for multiple datasets
get_multiple_datasets <- function(dataset_ids,
                                  version,
                                  cache,
                                  force_download,
                                  sandbox) {
  result <- list()
  for (id in dataset_ids) {
    # call get_dataset in 'quiet' mode to suppress individual prints
    result[[id]] <- get_dataset(
      id,
      version = version,
      cache = cache,
      force_download = force_download,
      sandbox = sandbox,
      quiet = TRUE
    )
  }
  # assign a special class to the list for custom printing
  result <- structure(result, class = c("openesm_dataset_list", "list"))
  
  # explicitly print the summary for the user
  print(result)
  
  return(invisible(result))
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

