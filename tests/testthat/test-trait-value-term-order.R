## `trait_value_term_order` in the `metadata` block of `config/metadata.yml`
## chooses whether the terms of a multi-term trait value are sorted
## (`alphabetical`, the default) or keep the source's order (`input_order`), and
## either way drops a repeated term (#297).
##
## `Test_2023_3` holds `fruit_colour` values with several colours in a
## non-alphabetical order, e.g. `red yellow brown black` for Juncus kraussii.

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")

with_term_order <- function(order) {
  m <- resource_metadata
  m[["trait_value_term_order"]] <- order
  m
}

build_example <- function(metadata = resource_metadata, config = NULL, id = "Test_2023_3") {
  if (is.null(config)) {
    config <- dataset_configure(file.path("examples", id, "metadata.yml"), definitions)
  }
  dataset_process(file.path("examples", id, "data.csv"), config, schema, metadata, unit_conversions)
}

sorted_terms <- function(x) util_separate_and_sort(x, order = "alphabetical")


test_that("`util_separate_and_sort` sorts or keeps the order, dropping repeats either way", {
  x <- c("tree shrub", "shrub tree", "tree shrub tree", "tree", NA)

  expect_equal(util_separate_and_sort(x), c("shrub tree", "shrub tree", "shrub tree", "tree", NA))
  expect_equal(util_separate_and_sort(x, order = "alphabetical"), util_separate_and_sort(x))
  expect_equal(
    util_separate_and_sort(x, order = "input_order"),
    c("tree shrub", "shrub tree", "tree shrub", "tree", NA)
  )
  # The first occurrence of a repeated term keeps its place
  expect_equal(util_separate_and_sort("c a b a c", order = "input_order"), "c a b")
  expect_error(util_separate_and_sort(x, order = "reverse"), "should be one of")
})


test_that("`util_trait_value_term_order` defaults to `alphabetical` and rejects anything else", {
  expect_equal(util_trait_value_term_order(list()), "alphabetical")
  expect_equal(util_trait_value_term_order(list(trait_value_term_order = NA)), "alphabetical")
  expect_equal(util_trait_value_term_order(with_term_order("alphabetical")), "alphabetical")
  expect_equal(util_trait_value_term_order(with_term_order("input_order")), "input_order")
  expect_error(
    util_trait_value_term_order(with_term_order("source")),
    "must be one of `alphabetical` or `input_order`, not `source`"
  )
  expect_error(util_trait_value_term_order(with_term_order(c("alphabetical", "input_order"))), "must be one of")
})


test_that("the schema documents exactly the settings the build accepts, default first", {
  documented <- schema[["austraits"]][["elements"]][["metadata"]][["elements"]][["trait_value_term_order"]]
  expect_equal(names(documented[["values"]]), util_trait_value_term_orders())
  expect_equal(util_trait_value_term_orders()[1], util_trait_value_term_order(list()))
})


test_that("leaving the setting out builds exactly what `alphabetical` builds", {
  # The examples' config sets nothing, so this is also what the golden fixtures pin
  expect_null(resource_metadata[["trait_value_term_order"]])

  default <- build_example()
  alphabetical <- build_example(with_term_order("alphabetical"))

  for (v in c("traits", "excluded_data", "locations", "contexts", "methods")) {
    expect_equal(default[[v]], alphabetical[[v]], info = v)
  }
  # Only the recorded setting differs, and only because it was set explicitly
  expect_null(default$metadata[["trait_value_term_order"]])
  expect_equal(alphabetical$metadata[["trait_value_term_order"]], "alphabetical")

  i <- default$traits$taxon_name == "Juncus kraussii" & default$traits$trait_name == "fruit_colour"
  expect_equal(default$traits$value[i], "black brown red yellow")
})


test_that("`input_order` keeps the source's order and changes nothing else", {
  alphabetical <- build_example()
  input_order <- build_example(with_term_order("input_order"))

  # The built dataset records the setting, so users can tell order is meaningful
  expect_equal(input_order$metadata[["trait_value_term_order"]], "input_order")

  i <- input_order$traits$taxon_name == "Juncus kraussii" & input_order$traits$trait_name == "fruit_colour"
  expect_equal(input_order$traits$value[i], "red yellow brown black")

  # Same rows in the same order; values differ only in the order of their terms
  expect_equal(
    dplyr::select(input_order$traits, -"value"),
    dplyr::select(alphabetical$traits, -"value")
  )
  changed <- input_order$traits$value != alphabetical$traits$value
  expect_gt(sum(changed), 1)
  expect_equal(sorted_terms(input_order$traits$value), alphabetical$traits$value)

  # Excluded values are never reordered, in either mode
  expect_equal(input_order$excluded_data, alphabetical$excluded_data)
})


test_that("a repeated term is dropped in both modes", {
  config <- dataset_configure("examples/Test_2023_3/metadata.yml", definitions)
  config[["metadata"]][["substitutions"]] <- c(
    config[["metadata"]][["substitutions"]],
    list(list(trait_name = "fruit_colour", find = "red yellow brown", replace = "red yellow red brown"))
  )

  i <- function(x) x$traits$taxon_name == "Schoenus vaginatus" & x$traits$trait_name == "fruit_colour"

  alphabetical <- build_example(config = config)
  expect_equal(alphabetical$traits$value[i(alphabetical)], "brown red yellow")

  input_order <- build_example(with_term_order("input_order"), config = config)
  expect_equal(input_order$traits$value[i(input_order)], "red yellow brown")
})


test_that("under `input_order` no record's terms are reordered, even to match another", {
  # Wenk_2025 in austraits.build has one species-level observation given as
  # `prostrate cushion-forming` and as `cushion-forming prostrate`. Sorting
  # makes the two rows identical; under `input_order` each keeps its order.
  dir <- file.path(withr::local_tempdir(), "Test_2023_3")
  dir.create(dir)
  file.copy(file.path("examples/Test_2023_3", c("data.csv", "metadata.yml")), dir)
  data <- readLines(file.path(dir, "data.csv"))
  juncus <- grep("Juncus kraussii,fruit_colour", data, value = TRUE)
  expect_length(juncus, 1)
  writeLines(c(data, sub("red yellow brown black", "black brown yellow red", juncus)), file.path(dir, "data.csv"))

  config <- dataset_configure(file.path(dir, "metadata.yml"), definitions)
  build <- function(metadata) {
    built <- dataset_process(file.path(dir, "data.csv"), config, schema, metadata, unit_conversions)
    built$traits %>% dplyr::filter(.data$taxon_name == "Juncus kraussii", .data$trait_name == "fruit_colour")
  }

  alphabetical <- build(resource_metadata)
  input_order <- build(with_term_order("input_order"))

  expect_equal(alphabetical$value, rep("black brown red yellow", 2))
  expect_setequal(input_order$value, c("red yellow brown black", "black brown yellow red"))
  expect_equal(nrow(dplyr::distinct(alphabetical)), 1)
  expect_equal(nrow(dplyr::distinct(input_order)), 2)
})


test_that("an unknown setting stops the build, naming the setting", {
  expect_error(
    build_example(with_term_order("as_entered")),
    "`trait_value_term_order` in the `metadata` block of `config/metadata.yml` must be one of"
  )
})


test_that("`dataset_test` builds with the database's setting", {
  # The build inside `dataset_test()` reads `path_config`; an invalid setting
  # there must surface as a failed build rather than being ignored
  path_config <- withr::local_tempdir()
  file.copy(list.files("config", full.names = TRUE), path_config, recursive = TRUE)
  config_metadata <- yaml::read_yaml(file.path(path_config, "metadata.yml"))

  failures_with <- function(order) {
    config_metadata$metadata$trait_value_term_order <- order
    yaml::write_yaml(config_metadata, file.path(path_config, "metadata.yml"))
    reporter <- testthat::ListReporter$new()
    testthat::with_reporter(
      reporter,
      dataset_test_worker("Test_2023_3", path_config = path_config, path_data = "examples")
    )
    sum(as.data.frame(reporter$get_results())$failed)
  }

  # Test_2023_3 has blank dates, which fail whatever the setting, so compare
  # with the default
  expect_gt(failures_with("as_entered"), failures_with("alphabetical"))
  expect_equal(failures_with("input_order"), failures_with("alphabetical"))
})


test_that("`dataset_check_duplicate_values` compares a `text` value's words as written (#87)", {
  # A `text` value's words are in order, so two records differing only in word
  # order are different values
  records <- tibble::tibble(
    .row = 1:2, dataset_id = c("A", "B"), taxon_name = "Sp one", trait_name = "host_name",
    value = c("Eucalyptus viminalis", "viminalis Eucalyptus"), lat = -33, lon = 151
  )
  expect_equal(nrow(util_categorical_value_matches(records, 10, sort_terms = "plant_growth_form")), 0)
  expect_equal(nrow(util_categorical_value_matches(records, 10)), 1)
})


test_that("`dataset_check_duplicate_values` matches categorical values whatever their term order", {
  records <- tibble::tibble(
    .row = 1:4,
    dataset_id = c("A", "B", "B", "B"),
    taxon_name = "Sp one",
    trait_name = "plant_growth_form",
    value = c("tree shrub", "shrub tree", "tree shrub", "tree"),
    lat = -33,
    lon = 151
  )
  expect_equal(
    util_categorical_value_matches(records, 10),
    tibble::tibble(.row_1 = c(1L, 1L), .row_2 = c(2L, 3L))
  )
})
