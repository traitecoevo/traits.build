
test_data <- "data/Test_2022/data.csv"

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
traits_definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
test_config <- dataset_configure("data/Test_2022/test-metadata.yml",
                                  traits_definitions,
                                  unit_conversions)

test_that("dataset_configure is working", {
  expect_no_error(
    test_config <- dataset_configure("data/Test_2022/test-metadata.yml",
                                      traits_definitions,
                                      unit_conversions))
  expect_type(test_config, "list")
  expect_length(test_config, 4)
  expect_named(test_config,
               c("dataset_id", "metadata", "definitions", "unit_conversion_functions"))
})

test_that("dataset_process is working", {
  expect_no_error(austraits_names <- schema$austraits$elements %>% names())
  expect_no_error(x <- dataset_process(test_data, test_config, schema, resource_metadata))
  expect_type(x, "list")
  expect_length(x, 13)
  expect_named(x, austraits_names)
  expect_equal(nrow(x$excluded_data), 0)
  # Test to see if `filter_missing_values` argument works
  expect_equal(
    nrow(
      dataset_process(test_data, test_config, schema, resource_metadata,
                      filter_missing_values = TRUE)$excluded_data
    ),
  0)
  expect_equal(
    nrow(
      dataset_process(test_data, test_config, schema, resource_metadata,
      filter_missing_values = FALSE)$excluded_data
    ),
  44)
})

test_that("process_custom_code is working", {
  expect_no_error(metadata <- test_config$metadata)
  expect_no_error(data <- readr::read_csv(test_data, col_types = cols(), guess_max = 100000, progress = FALSE))
  expect_equal(ncol(data), 13)
  expect_equal(ncol(process_custom_code(metadata[["dataset"]][["custom_R_code"]])(data)), 16)
  expect_silent(process_custom_code(NA))
})

# The below functions are not working
#test_that("process_flag_unsupported_traits is working", {
#  process_flag_unsupported_traits(data, definitions)
#})

#test_that("process_flag_excluded_observations is working", {
#  process_flag_excluded_observations
#})
