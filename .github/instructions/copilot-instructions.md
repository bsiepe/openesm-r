---
applyTo: '**'
---
## Project Goal
R package to download psychology ESM datasets from Zenodo via DOIs. Target: non-technical psychology researchers.
Core Functions

- list_datasets() - show available datasets, obtain metadata
- get_dataset(dataset_id) - download single dataset, return S3 object with metadata
- get_dataset(c("id1", "id2")) - download multiple, return named list
- cite(), license(), notes() - S3 generics for compliance 
- utils.R - utility functions for internal use

## Technical Stack

S3 classes 
Data format: TSV from Zenodo
Metadata: JSON from GitHub repo

## Coding Rules

- snake_case for everything
- comments only when logic unclear, always lowercase
- no fancy metaprogramming
- iterative changes, one function at a time
- minimal abstractions
- explicit over clever
- use cli package for user messages

## Data Flow

- Read metadata from GitHub JSON
- Download TSV from Zenodo using DOI
- Parse TSV into tibble
- Wrap in S3 class with metadata
- Cache locally in ~/.openesm/