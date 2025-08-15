.onAttach <- function(libname, pkgname) {
  msg <- paste0(
    "Welcome to openesm!\n\n",
    "Find documentation and usage examples at https://openesmdata.org\n\n",
    "Get started:\n",
    "  * list_datasets() to browse available datasets\n",
    "  * get_dataset('dataset_id') to download a specific dataset\n",
    "  * ?get_dataset or visit the website for detailed guides\n"
  )
  
  packageStartupMessage(msg)
}