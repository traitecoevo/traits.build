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
        tables, ~read_csv(sprintf("examples/Test_2023_1/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

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
        tables, ~read_csv(sprintf("examples/Test_2023_2/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_2[[v]], expected_output[[v]])
  }

})


testthat::test_that("Test Dataset 3 builds correctly", {

  # Test Dataset 3: Test_2023_3
  # See README.md in examples/Test_2023_3 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_3 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_3/metadata.yml"),
      file.path(examples_dir, "Test_2023_3/data.csv"),
      "Test Dataset 3", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 3")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_3/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_3[[v]], expected_output[[v]])
  }

})


testthat::test_that("Test Dataset 4 builds correctly", {

  # Test Dataset 4: Test_2023_4
  # See README.md in examples/Test_2023_4 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_4 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_4/metadata.yml"),
      file.path(examples_dir, "Test_2023_4/data.csv"),
      "Test Dataset 4", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 4")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_4/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_4[[v]], expected_output[[v]])
  }

})

# TODO
# Need to check for long datasets
# Reading in from trait vs dataset level? ### Check for long datasets
# Maybe there should be a prompt in `metadata_create_template` and `metadata_add_traits`
# about `repeat_measurements_id`?
# Make sure that the order of measurements in the data is preserved with `repeat_measurements_id`

# Question: For the dataset level in wide format, would every row have the same `repeat_measurements_id`
# across all variables? Right now I think if there's an NA in a column then `repeat_measurements_id`
# wouldn't be the same after that NA
# I'm not sure what users of response curve data would prefer

# Trait level, wide format:
# If there are repeat measurements at the individual level, there needs to be an `individual_id`
# column otherwise each row will be assumed to be a different `observation_id`
# If there are repeat measurements at the population level, there needs to be a location name,
# plot context, or treatment context identifying the populations
# If there are repeat measurements at the species level, the code works fine because `observation_id`
# is the same across rows for a given species

# Trait level, long format:
# If there are repeat measurements at the individual level, there doesn't need to be an `individual_id`
# column specified because long format automatically groups together values with the same location
# and `taxon_name`
# If there are repeat measurements at the population level, there needs to be a location name,
# plot context, or treatment context identifying the populations
# If there are repeat measurements at the species level, the code works fine because `observation_id`
# is the same across rows for a given species


testthat::test_that("Test Dataset 5 builds correctly", {

  # Test Dataset 5: Test_2023_5
  # See README.md in examples/Test_2023_5 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_5 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_5/metadata.yml"),
      file.path(examples_dir, "Test_2023_5/data.csv"),
      "Test Dataset 5", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 5")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_5/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_4[[v]], expected_output[[v]])
  }

})


testthat::test_that("Test Dataset 7 builds correctly", {

  # Test Dataset 7: Test_2023_7
  # See README.md in examples/Test_2023_7 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_7 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_7/metadata.yml"),
      file.path(examples_dir, "Test_2023_7/data.csv"),
      "Test Dataset 7", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 7")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_7/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_7[[v]], expected_output[[v]])
  }

})


testthat::test_that("Test Dataset 8 builds correctly", {

  # Test Dataset 8: Test_2023_8
  # See README.md in examples/Test_2023_8 for details about this dataset

  # Build dataset
  expect_no_error(
    Test_2023_8 <- test_build_dataset(
      file.path(examples_dir, "Test_2023_8/metadata.yml"),
      file.path(examples_dir, "Test_2023_8/data.csv"),
      "Test Dataset 8", definitions, unit_conversions, schema, resource_metadata, taxon_list
    ),
    info = "Building Test Dataset 8")

  # Expected output
  tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
              "taxonomic_updates", "taxa", "contributors")
  expect_no_error(
    expected_output <-
      purrr::map(
        tables, ~read_csv(sprintf("examples/Test_2023_8/output/%s.csv", .x), col_types = cols(.default = "c"))),
    info = "Reading in expected output tables"
  )
  # Todo: also load and test non-csv outputs
  names(expected_output) <- tables

  # Check all tables are equal to expected output tables
  for (v in tables) {
    expect_equal(Test_2023_8[[v]], expected_output[[v]])
  }

})


#testthat::test_that("`dataset_test` works properly for Test Dataset 9", {

  # Test Dataset 9: Test_2023_9
  # See README.md in examples/Test_2023_9 for details about this dataset

  # Build dataset
  #expect_no_error(
  #  Test_2023_9 <- test_build_dataset(
  #    file.path(examples_dir, "Test_2023_9/metadata.yml"),
  #    file.path(examples_dir, "Test_2023_9/data.csv"),
  #    "Test Dataset 9", definitions, unit_conversions, schema, resource_metadata, taxon_list
  #  ),
  #  info = "Building Test Dataset 9")

  # Expected output
  #dataset_test("Test_2023_9", path_data = "examples")

  # Test `pivot_wider`

#})
