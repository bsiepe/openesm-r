# openesm: Access openESM datasets in R

[![R-CMD-check](https://github.com/bsiepe/openesm-r/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bsiepe/openesm-r/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/bsiepe/openesm-r/graph/badge.svg)](https://app.codecov.io/gh/bsiepe/openesm-r)
[![CRAN status](https://www.r-pkg.org/badges/version/openesm)](https://CRAN.R-project.org/package=openesm)

The `openesm` package provides programmatic access to the openESM database, a collection of harmonized Experience Sampling Method (ESM) datasets ([openesmdata.org](https://openesmdata.org)). It enables researchers to discover, download, and work with ESM data while ensuring proper citation and license compliance.

## Installation

Install the released version from CRAN:

```r
install.packages("openesm")
```

Or install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("bsiepe/openesm-r")
```

## Usage

### Discover datasets

```r
library(openesm)

# List all available datasets
datasets <- list_datasets()
print(datasets)

# View specific dataset information
datasets[1, ]
```

### Download data

```r
# Download a single dataset
data <- get_dataset("example_dataset_id")

# Access the data
head(data$data)

# View metadata
data$metadata

# Download multiple datasets
multiple_data <- get_dataset(c("dataset1", "dataset2"))
```

### Citation and licensing

```r
# Get citation information
cite(data)

# View license details
license(data)

# Additional notes
notes(data)
```

### Cache management

```r
# View cache information
cache_info()

# Clear cache
clear_cache()
```

## Dataset Structure

Each dataset is returned as an S3 object containing:

- `data`: A tibble with the ESM observations
- `metadata`: List with dataset information including sample size, study design, and variable descriptions
- `dataset_id`: Unique dataset identifier
- `version`: Dataset version number

## Data Citation

When using datasets from the openESM database, please cite the original publication(s) for a specific dataset. The `cite()` function provides properly formatted citations for both.

## License

This package is licensed under the MIT License. Individual datasets may have different licenses - check using the `license()` function.

## Contributing

Please report bugs and request features at <https://github.com/bsiepe/openesm-r/issues>.






