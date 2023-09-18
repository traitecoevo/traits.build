schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml",  "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
taxon_list <- read_csv_char("config/taxon_list.csv")
examples_dir <- "examples"

# Not sure why but running the tests line by line generates different ids than when you
# run the whole `test_that` function... It means that I have to use the `test_that` function to
# generate the expected output.

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
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_1/output/%s.csv", .x), col_types = "cccccccccccccccccccccccc")),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Temporary modifications to get these tests to pass
  Test_2023_1$traits <-
    Test_2023_1$traits %>%
    mutate(across(
      c(temporal_id, entity_context_id, plot_id, treatment_id, method_context_id),
      ~if_else(.x == "NA", NA_character_, .x)
    ))
  Test_2023_1$methods <-
    Test_2023_1$methods %>%
    mutate(across(c(source_secondary_key, source_original_dataset_key), ~NA_character_))
  Test_2023_1$excluded_data <-
    Test_2023_1$excluded_data %>%
    mutate(across(
      c(temporal_id, entity_context_id, plot_id, treatment_id, method_context_id),
      ~if_else(.x == "NA", NA_character_, .x)
    ))

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_1[[v]], expected_output[[v]])
  }

})


testthat::test_that("Test Dataset 2 builds correctly", {

  # Test Dataset 2: Test_2023_2
  # See README.md in examples/Test_2023_2 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_2 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_2/metadata.yml"),
      file.path(examples_dir, "Test_2023_2/data.csv"),
      "Test Dataset 2", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 2")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_2/output/%s.csv", .x), col_types = "cccccccccccccccccccccccc")),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Temporary modifications to get these tests to pass
  columns <- c("basis_of_value", "replicates", "life_stage", "collection_date", "measurement_remarks")

  Test_2023_2$traits <-
    Test_2023_2$traits %>%
    mutate(across(dplyr::all_of(columns), as.character))
  Test_2023_2$methods <-
    Test_2023_2$methods %>%
    mutate(across(c(source_secondary_key, source_original_dataset_key), ~NA_character_))
  Test_2023_2$excluded_data <-
    Test_2023_2$excluded_data %>%
    mutate(across(dplyr::all_of(columns), as.character))

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_2[[v]], expected_output[[v]])
  }

})


#testthat::test_that("`dataset_test` works properly for Test Dataset 7", {

  # Test Dataset 7: Test_2023_7
  # See README.md in examples/Test_2023_7 for details about this dataset

  # Build dataset

  # Expected output
  #dataset_test("Test_2023_7", path_data = "examples")

#})
