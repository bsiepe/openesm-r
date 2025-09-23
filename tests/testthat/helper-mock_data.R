# This file contains helper functions for tests.
# It is sourced automatically by testthat.

create_mock_dataset_json <- function() {
  '{
    "datasets": [
      {
        "dataset_id": "0001_test",
      "title": "Test Dataset 1",
      "author": "John Doe",
      "version": "1.0.0",
      "url": "https://example.com/ds1",
      "description": "A test dataset.",
      "collection_date": "2023-01-01",
      "publication_date": "2023-02-01",
      "data_type": "ESM",
      "instrument": "Survey",
      "license": "CC-BY-4.0",
      "doi": "10.5281/zenodo.12345",
      "keywords": ["test", "esm"],
      "contributors": ["Jane Smith"],
      "funding": "Test Foundation",
      "references": ["Doe, J. (2023)."],
      "additional_comments": "None",
      "features": [
        {
          "name": "id", "description": "Participant ID", "variable_type": "categorical",
          "coding": "", "answer_categories": "", "wording": "", "labels": "",
          "transformation": "", "source": "", "assessment_type": "ESM", "construct": "", "comments": ""
        },
        {
          "name": "scheduled", "description": "Time of beep schedule", "variable_type": "PosixCt",
          "coding": "", "answer_categories": "", "wording": "", "labels": "",
          "transformation": "", "source": "", "assessment_type": "ESM", "construct": "", "comments": ""
        }
      ]
    },
    {
      "dataset_id": "0002_test",
      "title": "Test Dataset 2",
      "author": "Jane Smith",
      "version": "2.0.0",
      "url": "https://example.com/ds2",
      "description": "Another test dataset.",
      "collection_date": "2024-01-01",
      "publication_date": "2024-02-01",
      "data_type": "ESM",
      "instrument": "Experience Sampling",
      "license": "CC0",
      "doi": "10.5281/zenodo.67890",
      "keywords": ["test", "experience sampling"],
      "contributors": ["John Doe"],
      "funding": "Another Foundation",
      "references": ["Smith, J. (2024)."],
      "additional_comments": "Also none",
      "features": [
        {
          "name": "id", "description": "Participant ID", "variable_type": "categorical",
          "coding": "", "answer_categories": "", "wording": "", "labels": "",
          "transformation": "", "source": "", "assessment_type": "ESM", "construct": "", "comments": ""
        }
      ]
    }
  ]
}'
}

create_mock_openesm_dataset <- function() {
  # Use ::: to access internal function within the test environment
  metadata <- openesm:::process_specific_metadata(
    jsonlite::fromJSON(create_mock_dataset_json())[[1]]
  )
  data <- tibble::tibble(x = 1:5, y = letters[1:5])
  structure(
    list(
      data = data,
      metadata = metadata
    ),
    class = "openesm_dataset"
  )
}
