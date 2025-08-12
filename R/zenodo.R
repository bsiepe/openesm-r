# Zenodo Integration Functions

#' Resolve a Zenodo version
#'
#' Given a concept DOI, finds the specific version tag. If "latest" is requested,
#' it returns the most recent version tag.
#'
#' @param zenodo_doi Character string with the Zenodo concept DOI.
#' @param version Character string, either "latest" or a specific version tag (e.g., "v1.0.0").
#' @param sandbox Logical, whether to use Zenodo sandbox.
#' @return Character string with the resolved version tag.
#' @importFrom zen4R get_versions
#' @importFrom dplyr arrange desc slice pull
#' @noRd
resolve_zenodo_version <- function(zenodo_doi, version = "latest", sandbox = FALSE) {
  data_versions <- zen4R::get_versions(zenodo_doi, sandbox = sandbox)

  if (nrow(data_versions) == 0) {
    cli::cli_abort("No versions found for DOI {zenodo_doi}")
  }

  if (version == "latest") {
    latest_version_tag <- data_versions |>
      dplyr::arrange(dplyr::desc(version)) |>
      dplyr::slice(1) |>
      dplyr::pull(version)
    return(latest_version_tag)
  } else {
    if (!version %in% data_versions$version) {
      available_versions <- paste(data_versions$version, collapse = ", ")
      cli::cli_abort("Version {version} not found. Available versions: {available_versions}")
    }
    return(version)
  }
}

#' Download dataset from Zenodo.
#'
#' @param zenodo_doi Character string with the Zenodo concept DOI
#' @param dataset_id Character string with dataset identifier
#' @param author_name Character string with author name
#' @param version Character string specifying a specific version tag (e.g., "v1.0.0").
#' @param sandbox Logical, whether to use Zenodo sandbox. Defaults to FALSE. Mainly used for  internal testing
#' @param dest_path Character string with destination path
#' @return Character string with path to downloaded file.
#' @importFrom zen4R get_versions
#' @importFrom curl curl_download
download_from_zenodo <- function(zenodo_doi,
                                 dataset_id,
                                 author_name,
                                 version,
                                 sandbox = FALSE,
                                 dest_path = NULL) {

  # get available versions to find the record ID for the specific version
  data_versions <- zen4R::get_versions(zenodo_doi, sandbox = sandbox)

  version_match <- data_versions[data_versions$version == version, ]
  if (nrow(version_match) == 0) {
    # this check is redundant if resolve_zenodo_version is called first, but good for safety
    available_versions <- paste(data_versions$version, collapse = ", ")
    cli::cli_abort("Version {version} not found. Available versions: {available_versions}")
  }

  specific_version_doi <- version_match$doi

  # extract record ID from DOI and construct filename
  record_id <- sub(".*zenodo\\.", "", specific_version_doi)

  filename <- paste0(dataset_id, "_", author_name, "_ts.tsv")

  # construct download URL
  if (isTRUE(sandbox)) {
    download_url <- paste0("https://sandbox.zenodo.org/records/", record_id, "/files/", filename)
  } else {
    download_url <- paste0("https://zenodo.org/records/", record_id, "/files/", filename)
  }

  # Set destination path
  if (is.null(dest_path)) {
    dest_path <- filename
  }

  # download file
  cli::cli_alert_info("Downloading {filename} from Zenodo (version {version})...")
  curl::curl_download(download_url, destfile = dest_path)
  cli::cli_alert_success("Downloaded {filename}")

  return(dest_path)
}
