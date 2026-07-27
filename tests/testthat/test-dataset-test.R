test_that("`dataset_test` handles datasets declaring more than one identifier", {
  # A dataset may declare several identifiers, so `metadata$identifiers` is a
  # list and `is.na()` on it returns one value per element. Guarding with
  # `if (!is.na(metadata$identifiers))` therefore aborted the whole run with
  # "the condition has length > 1" for any such dataset.
  expect_gt(length(read_metadata("examples/Test_2023_1/metadata.yml")$identifiers), 1)

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_ids = "Test_2023_1", path_data = "examples")
    )
  )

  expect_false(any(grepl("the condition has length > 1", out, fixed = TRUE)))
})
