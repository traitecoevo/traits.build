test_that("`dataset_find_pivot_duplicates` returns nothing for a dataset that pivots", {
  # Test_2023_5 passes every `dataset_test()` check
  expect_equal(
    nrow(suppressMessages(dataset_find_pivot_duplicates("Test_2023_5", path_data = "examples"))),
    0
  )
})


test_that("`dataset_find_pivot_duplicates` finds and flags exact duplicates", {
  # Repeat the first row of Test_2023_5's data, so every trait it carries has
  # two identical rows sharing a pivot cell. A compiled database drops such
  # rows, which is why this has to build the dataset rather than read one.
  path_data <- withr::local_tempdir()
  file.copy("examples/Test_2023_5", path_data, recursive = TRUE)
  data_file <- file.path(path_data, "Test_2023_5", "data.csv")
  data <- read_csv_char(data_file)
  readr::write_csv(dplyr::bind_rows(data, data[1, ]), data_file, na = "")

  dupes <- suppressMessages(dataset_find_pivot_duplicates("Test_2023_5", path_data = path_data))

  expect_gt(nrow(dupes), 0)
  expect_true(all(dupes$number_of_duplicates == 2))
  expect_true(all(dupes$exact_duplicate))
  expect_true(all(dupes$original_name == data$name_original[1]))
})


test_that("`dataset_find_pivot_duplicates` rejects a database object", {
  expect_error(dataset_find_pivot_duplicates(list()), "character vector of dataset ids")
})


test_that("`check_pivot_duplicates` reports cells holding more than one value", {
  traits <- tibble::tibble(
    dataset_id = c("A", "A", "A", "B"),
    taxon_name = "Sp one",
    original_name = "Sp one",
    observation_id = c("A_01", "A_01", "A_02", "B_01"),
    trait_name = c("leaf_length", "leaf_length", "leaf_length", "leaf_length"),
    value = c("1.2", "1.3", "1.2", "1.2"),
    value_type = "mean",
    repeat_measurements_id = NA_character_,
    method_id = "01",
    method_context_id = NA_character_
  )
  database <- list(traits = traits)

  dupes <- check_pivot_duplicates(database)
  expect_equal(nrow(dupes), 1)
  expect_equal(dupes$observation_id, "A_01")
  expect_equal(dupes$number_of_duplicates, 2L)
  expect_equal(
    names(dupes)[1:7],
    c("dataset_id", "trait_name", "number_of_duplicates", "taxon_name", "original_name",
      "observation_id", "value_type")
  )
  expect_equal(nrow(check_pivot_duplicates(database, "B")), 0)
})
