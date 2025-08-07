# Function to list available datasets from the OpenESM metadata repository
list_datasets <- function(cache_hours = 24) {
  # define the path to the cached master index file
  index_dir <- get_cache_dir() # gets ~/.cache/R/openesm
  index_path <- file.path(index_dir, "datasets.json")
  
  # check for a recent cache file (less than 24 hours old)
  if (fs::file_exists(index_path)) {
    file_age_hours <- difftime(Sys.time(), file.info(index_path)$mtime, units = "hours")
    if (file_age_hours < cache_hours) {
      msg_info("Using cached dataset index (less than {.val {cache_hours}} hours old).")
      return(read_json_safe(index_path))
    }
  }
  
  # otherwise, download a fresh copy
  msg_info("Downloading fresh dataset index from GitHub.")
  # TODO this needs to be updated 
  index_url <- "https://raw.githubusercontent.com/bsiepe/openesm-metadata/refs/heads/main/datasets.json?token=GHSAT0AAAAAACSKLQBOU2N4YKT4WQN5GSJA2EUWKSA"
  download_with_progress(index_url, index_path)
  
  return(read_json_safe(index_path))
}
