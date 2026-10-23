## A `value_type` or `basis_of_value` in a dataset's `dataset` block is the
## default for every trait that does not declare its own (#249). The build has
## always honoured this; `dataset_test` used to reject it.


# Copy an example dataset into a fresh data folder, apply `edit` to its
# metadata, and return the folder
example_with_metadata <- function(dataset_id, edit, env = parent.frame()) {
  path_data <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(path_data, dataset_id))
  file.copy(
    list.files(file.path("examples", dataset_id), full.names = TRUE),
    file.path(path_data, dataset_id), recursive = TRUE
  )

  path_metadata <- file.path(path_data, dataset_id, "metadata.yml")
  write_metadata(edit(read_metadata(path_metadata)), path_metadata)

  path_data
}

# Test_2023_2 with `value_type` and `basis_of_value` moved to the dataset
# block. Most traits read both from the `value_type` and `basis_of_value`
# columns, and lose those entries. Three fix a value of their own --
# `leaf_mass_per_area` (`basis_of_value: measurement`) and two `huber_value`
# columns (`value_type: mean` and `maximum`) -- and keep it, as overrides.
move_to_dataset_block <- function(metadata) {
  metadata$dataset$value_type <- "value_type"
  metadata$dataset$basis_of_value <- "basis_of_value"

  metadata$traits <- lapply(metadata$traits, function(trait) {
    if (identical(trait$value_type, "value_type")) {
      trait$value_type <- NULL
    }
    if (identical(trait$basis_of_value, "basis_of_value")) {
      trait$basis_of_value <- NULL
    }
    trait
  })

  metadata
}

dataset_test_output <- function(dataset_id, path_data) {
  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )

  out <- capture.output(
    suppressMessages(
      dataset_test(dataset_id, path_data = path_data)
    )
  )
  gsub("\033\\[[0-9]+m", "", out)
}

build_traits <- function(dataset_id, path_data) {
  definitions <- get_schema("config/traits.yml", "traits")
  dataset_process(
    file.path(path_data, dataset_id, "data.csv"),
    dataset_configure(file.path(path_data, dataset_id, "metadata.yml"), definitions),
    get_schema(),
    get_schema("config/metadata.yml", "metadata"),
    get_unit_conversions("config/unit_conversions.csv")
  )$traits
}


test_that("`dataset_test` accepts `value_type`/`basis_of_value` in the dataset block (#249)", {
  path_data <- example_with_metadata("Test_2023_2", move_to_dataset_block)

  # The move must add no failures to those of the unedited Test_2023_2 (which
  # has one: blank `collection_date` values)
  failures <- function(out) {
    sub("^.*metadata\\.yml\t", "", grep("metadata\\.yml\t", out, value = TRUE))
  }
  baseline <- dataset_test_output("Test_2023_2", "examples")
  out <- dataset_test_output("Test_2023_2", path_data)
  expect_equal(failures(out), failures(baseline))
  expect_equal(
    tail(grep("^\\[ FAIL", out, value = TRUE), 1),
    tail(grep("^\\[ FAIL", baseline, value = TRUE), 1)
  )
})


test_that("the dataset block supplies what each trait leaves out (#249)", {
  # The edit must not change a single built value: the dataset block fills in
  # the fields the traits no longer declare, and the trait that still declares
  # its own `basis_of_value` keeps it
  path_data <- example_with_metadata("Test_2023_2", move_to_dataset_block)

  expected <- build_traits("Test_2023_2", "examples")
  traits <- build_traits("Test_2023_2", path_data)

  expect_equal(
    traits[c("trait_name", "value", "value_type", "basis_of_value")],
    expected[c("trait_name", "value", "value_type", "basis_of_value")]
  )
  expect_equal(
    unique(traits$basis_of_value[traits$trait_name == "leaf_mass_per_area"]),
    "measurement"
  )

  # The overrides are still in place, so this is a real test of precedence
  metadata <- read_metadata(file.path(path_data, "Test_2023_2", "metadata.yml"))
  expect_setequal(
    unlist(lapply(metadata$traits, `[[`, "value_type")), c("mean", "maximum")
  )
})


test_that("a trait still needs `value_type`/`basis_of_value` when the dataset block lacks them", {
  path_data <- example_with_metadata("Test_2023_2", function(metadata) {
    metadata$traits[[2]]$value_type <- NULL
    metadata
  })

  out <- dataset_test_output("Test_2023_2", path_data)
  expect_match(out, "does not contain: 'value_type'", fixed = TRUE, all = FALSE)
})


test_that("`dataset_test` still rejects unknown fields in the dataset block", {
  # Test_2023_9's dataset-level `value_type`/`basis_of_value` used to be the
  # only coverage of this check, and they are now allowed
  path_data <- example_with_metadata("Test_2023_2", function(metadata) {
    metadata$dataset$not_a_field <- "unknown"
    metadata
  })

  out <- dataset_test_output("Test_2023_2", path_data)
  expect_match(
    out, "`dataset` field names include(s) invalid terms: 'not_a_field'",
    fixed = TRUE, all = FALSE
  )
})


test_that("a dataset builds when every dataset-level field is read from a column", {
  # Test_2023_2 fixes `entity_type` and `basis_of_record` as literals and reads
  # the rest from columns. Reading those two from columns as well leaves nothing
  # for `process_parse_data()` to add as constants, and binding the resulting
  # 0-row tibble used to abort the build ("Can't recycle ... (size 0)").
  all_from_columns <- function(metadata) {
    metadata$dataset$custom_R_code <-
      'data %>% mutate(LASA1000_dupe = LASA1000, et = "species", bor = "field")'
    metadata$dataset$entity_type <- "et"
    metadata$dataset$basis_of_record <- "bor"
    metadata
  }
  path_data <- example_with_metadata("Test_2023_2", all_from_columns)

  expected <- build_traits("Test_2023_2", "examples")
  traits <- build_traits("Test_2023_2", path_data)

  expect_equal(nrow(traits), nrow(expected))
  expect_equal(traits$entity_type, expected$entity_type)
  expect_equal(traits$basis_of_record, expected$basis_of_record)
})
