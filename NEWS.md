# openesm 0.1.1

## Bug fixes

* Added cleanup code in examples to remove temporary cache files created during CRAN checks
* Fixed cache directory consistency
* Fixed handling of multiple-word author names in `get_dataset()`


# openesm 0.1.0

* Initial CRAN submission.

## Main features

* `list_datasets()` to browse available ESM datasets with metadata.
* `get_dataset()` to download single or multiple datasets from Zenodo.
* `cite()` and `notes()` to access citation and additional dataset information.
* Automatic caching system for efficient data management.
* `cache_info()` and `clear_cache()` for cache management.
