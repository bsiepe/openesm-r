# Zenodo Integration Functions

#' Download dataset from Zenodo.
#'
#' @param zenodo_doi Character string with the Zenodo concept DOI
#' @param dataset_id Character string with dataset identifier
#' @param author_name Character string with author name
#' @param version Character string specifying version ("latest" or specific version). Defaults to "latest".
#' @param sandbox Logical, whether to use Zenodo sandbox. Defaults to FALSE. Mainly used for  internal testing
#' @param dest_path Character string with destination path
#' @return Character string with path to downloaded file.
#' @importFrom zen4R get_versions
#' @importFrom dplyr arrange desc slice pull
#' @importFrom curl curl_download
download_from_zenodo <- function(zenodo_doi, 
                                 dataset_id, 
                                 author_name, 
                                 version = "latest",
                                 sandbox = FALSE,
                                 dest_path = NULL) {
  
  # get available versions
  data_versions <- zen4R::get_versions(zenodo_doi, sandbox = sandbox)
  
  if (nrow(data_versions) == 0) {
    cli::cli_abort("No versions found for DOI {zenodo_doi}")
  }
  
  # select version
  if (version == "latest") {
    newest_version <- data_versions |> 
      dplyr::arrange(dplyr::desc(version)) |> 
      dplyr::slice(1)
    newest_version <- newest_version$doi
  } else {
    # find specific version
    version_match <- data_versions[data_versions$version == version, ]
    if (nrow(version_match) == 0) {
      available_versions <- paste(data_versions$version, collapse = ", ")
      cli::cli_abort("Version {version} not found. Available versions: {available_versions}")
    }
    newest_version <- version_match$doi
  }
  
  # extract record ID from DOI and construct filename
  newest_version_string <- sub(".*zenodo\\.", "", newest_version)
  
  filename <- paste0(dataset_id, "_", author_name, "_ts.tsv")
  
  # construct download URL
  if (isTRUE(sandbox)) {
    download_url <- paste0("https://sandbox.zenodo.org/records/", newest_version_string, "/files/", filename)
  } else {
    download_url <- paste0("https://zenodo.org/records/", newest_version_string, "/files/", filename)
  }
  
  # Set destination path
  if (is.null(dest_path)) {
    dest_path <- filename
  }
  
  # download if file doesn't exist
  if (!file.exists(dest_path)) {
    cli::cli_alert_info("Downloading {filename} from Zenodo (version {version})...")
    curl::curl_download(download_url, destfile = dest_path)
    cli::cli_alert_success("Downloaded {filename}")
  } else {
    cli::cli_alert_info("File {filename} already exists, skipping download")
  }
  
  return(dest_path)
}
