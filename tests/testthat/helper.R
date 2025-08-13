# This file contains helper functions for tests.

create_mock_dataset_json <- function() {
  # This JSON string is structured to match the real metadata format exactly.
  # Using jsonlite::toJSON to handle escaping and formatting correctly.
  mock_list <- list(
    datasets = list(
      list(
        first_author = "Fried",
        dataset_id = "0001",
        year = 2021,
        reference_a = "@article{fried2022, title = {Mental health...}}",
        reference_b = NULL,
        paper_doi = "https://doi.org/10.1177/21677026211017839",
        zenodo_doi = "10.5072/zenodo.308201",
        link_to_data = "https://osf.io/mvdpe/",
        link_to_codebook = "https://osf.io/mx87b",
        link_to_code = "https://osf.io/mvdpe/",
        license = "CC-BY-4.0",
        n_participants = 80,
        n_time_points = 56,
        n_beeps_per_day = "4",
        passive_data_available = "no",
        cross_sectional_available = "yes",
        topics = "mental health, social contact, COVID",
        implicit_missingness = "no",
        raw_time_stamp = "yes",
        sampling_scheme = "4x/day fixed schedule",
        participants = "student sample",
        coding_file = "0001_fried_variables",
        additional_comments = "some individuals have more than the maximum number of observations",
        features = list(
          list(name = "id", description = "Participant ID"),
          list(name = "relax", description = "Difficulties relaxing")
        )
      ),
      list(
        first_author = "Test",
        dataset_id = "0002",
        year = 2023,
        reference_a = "@article{test2023, title = {A mock study...}}",
        reference_b = "@article{test2023b, title = {Another mock...}}",
        paper_doi = "https://doi.org/10.1234/mock.2",
        zenodo_doi = "10.5072/zenodo.mock2",
        link_to_data = "https://osf.io/mock2/",
        link_to_codebook = NULL,
        link_to_code = NULL,
        license = "CC0",
        n_participants = 50,
        n_time_points = 20,
        n_beeps_per_day = "8",
        passive_data_available = "yes",
        cross_sectional_available = "yes",
        topics = "testing, mock data",
        implicit_missingness = "yes",
        raw_time_stamp = "yes",
        sampling_scheme = "random",
        participants = "general population",
        coding_file = "0002_test_variables",
        additional_comments = "This is the second mock dataset.",
        features = list(
          list(name = "id", description = "Participant ID"),
          list(name = "mood", description = "Current mood rating")
        )
      )
    )
  )
  jsonlite::toJSON(mock_list, auto_unbox = TRUE)
}

create_mock_openesm_dataset <- function() {
  # This function creates a single, valid mock openesm_dataset object
  # for testing S3 methods.

  # create list of all datasets
  json_string <- create_mock_dataset_json()
  raw_list <- jsonlite::fromJSON(json_string, simplifyVector = FALSE)
  all_datasets_tibble <- openesm:::process_raw_datasets_list(raw_list)

  # select metadata for the first dataset
  mock_metadata <- all_datasets_tibble[1, ]

  # create mock data
  mock_data_content <- tibble::tibble(
    id = rep(1:5, each = 10),
    relax = sample(1:5, 50, replace = TRUE)
  )

  structure(
    list(
      data = mock_data_content,
      metadata = mock_metadata,
      dataset_id = mock_metadata$dataset_id,
      version = "1.0.0" # A mock resolved version
    ),
    class = "openesm_dataset"
  )
}
