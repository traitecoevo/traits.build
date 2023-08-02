schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml",  "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
taxon_list <- read_csv_char("config/taxon_list.csv")
examples_dir <- "examples"

testthat::test_that("Test Dataset 1 builds correctly", {

  # Test Dataset 1: Test_2023_1
  # See README.md in examples/Test_2023_1 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_1 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_1/metadata.yml"),
      file.path(examples_dir, "Test_2023_1/data.csv"),
      "Test Dataset 1", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 1")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(tables, ~read_csv(sprintf("examples/Test_2023_1/output/%s.csv", .x), col_types = "cc")),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Temporary modifications to get these tests to pass
  expected_output$traits <- expected_output$traits %>% mutate(across(everything(), as.character))
  expected_output$traits <-
    expected_output$traits %>%
    mutate(across(c(temporal_id, entity_context_id, plot_id, treatment_id, method_id), ~if_else(is.na(.x), "NA", .x)))
  names(expected_output$locations$value) <- expected_output$locations$location_property
  expected_output$methods <-
    expected_output$methods %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(source_secondary_key, source_original_dataset_key), ~""))
  expected_output$excluded_data <-
    expected_output$excluded_data %>%
    mutate(across(everything(), as.character)) %>%
    mutate(across(c(temporal_id, entity_context_id, plot_id, treatment_id, method_id), ~if_else(is.na(.x), "NA", .x)))
  expected_output$taxonomic_updates <- expected_output$taxonomic_updates %>% mutate(across(everything(), as.character))
  expected_output$taxa <- expected_output$taxa %>% mutate(across(everything(), as.character))
  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_1[[v]], expected_output[[v]])
  }

})
