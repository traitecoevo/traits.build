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
