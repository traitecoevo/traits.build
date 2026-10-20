## The four trait types, `numeric`, `categorical`, `month_presence` and `text`
## (#87). The check on a `month_presence` value used to apply by name to two
## AusTraits traits, `flowering_time` and `fruiting_time`; it now follows the
## type. `text` takes open-ended values, which are kept as written.
##
## The golden examples cover `month_presence` too: `flowering_time` in
## `config/traits.yml` is declared `month_presence`, and Test_2023_1, _3 and _9
## exclude values with both of its messages.


# Test_2023_2 in a fresh folder, with `columns` appended to its data.csv,
# `edit` applied to its metadata, and a config folder whose traits.yml is
# changed by `edit_definitions`. Returns the folder, holding `config/` and
# `data/`.
example_with_traits <- function(columns = list(), edit = identity,
                                edit_definitions = identity,
                                env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  path_dataset <- file.path(root, "data", "Test_2023_2")
  dir.create(path_dataset, recursive = TRUE)
  dir.create(file.path(root, "config"))
  file.copy(list.files(file.path("examples", "Test_2023_2"), full.names = TRUE), path_dataset)
  file.copy(list.files("config", full.names = TRUE), file.path(root, "config"))

  data <- read_csv_char(file.path(path_dataset, "data.csv"))
  for (v in names(columns)) {
    data[[v]] <- rep_len(columns[[v]], nrow(data))
  }
  readr::write_csv(data, file.path(path_dataset, "data.csv"), na = "")

  path_metadata <- file.path(path_dataset, "metadata.yml")
  write_metadata(edit(read_metadata(path_metadata)), path_metadata)

  path_traits <- file.path(root, "config", "traits.yml")
  traits <- yaml::read_yaml(path_traits)
  traits$traits$elements <- edit_definitions(traits$traits$elements)
  yaml::write_yaml(traits, path_traits)

  root
}

# A `traits` entry for a non-numeric trait read from column `var_in`
non_numeric_trait <- function(var_in, trait_name, ...) {
  utils::modifyList(
    list(
      var_in = var_in, unit_in = NA, trait_name = trait_name,
      value_type = "mode", basis_of_value = "expert_score", replicates = NA,
      methods = "Recorded from a flora."
    ),
    list(...)
  )
}

build_example <- function(root) {
  path_dataset <- file.path(root, "data", "Test_2023_2")
  dataset_process(
    file.path(path_dataset, "data.csv"),
    dataset_configure(
      file.path(path_dataset, "metadata.yml"),
      get_schema(file.path(root, "config", "traits.yml"), "traits")
    ),
    get_schema(),
    get_schema(file.path(root, "config", "metadata.yml"), "metadata"),
    get_unit_conversions(file.path(root, "config", "unit_conversions.csv"))
  )
}

dataset_test_example <- function(root, dataset_id = "Test_2023_2") {
  withr::local_options(
    useFancyQuotes = FALSE, cli.unicode = FALSE, crayon.enabled = FALSE,
    width = 120
  )
  out <- capture.output(
    suppressMessages(
      dataset_test(
        dataset_id,
        path_config = file.path(root, "config"),
        path_data = file.path(root, "data")
      )
    )
  )
  gsub("\033\\[[0-9]+m", "", out)
}

fail_count <- function(out) {
  tail(grep("^\\[ FAIL", out, value = TRUE), 1)
}


test_that("the schema lists the four trait types", {
  expect_equal(util_trait_types(), c("numeric", "categorical", "month_presence", "text"))
})


test_that("the `month_presence` check follows the type, not the trait name (#87)", {
  data <- tibble::tibble(
    trait_name = c("flowering_time", "leaf_flush_time", "leaf_flush_time", "leaf_flush_time", "leaf_flush_time"),
    value = c("nyyy", "nyyy", "nnnnyyyynnnx", "nnnnyyyynnnn", "NNNNYYYYNNNN"),
    taxon_name = "Acacia celsa",
    value_type = "mode",
    error = NA_character_
  )
  definitions <- list(
    # The name the rule used to be hard-coded for, now declared a categorical
    # trait without allowed values: no longer checked
    flowering_time = list(type = "categorical"),
    leaf_flush_time = list(type = "month_presence")
  )

  out <- process_flag_unsupported_values(data, definitions)

  expect_equal(
    out$error,
    c(NA, "Times must be length 12", "Time can only contain Y & Ns", NA, NA)
  )
})


test_that("a `month_presence` trait is checked and lower-cased like `flowering_time` was (#87)", {
  root <- example_with_traits(
    columns = list(flush = c("nnyyynnnnnnn", "YYNNNNNNNNNN", "nnyy", "jan-mar")),
    edit = function(metadata) {
      # `unit_in` is ignored: only a numeric trait has units
      metadata$traits <- c(metadata$traits, list(non_numeric_trait("flush", "leaf_flush_time", unit_in = "month")))
      metadata
    },
    edit_definitions = function(elements) {
      elements$leaf_flush_time <- list(label = "Leaf flush time, by month", type = "month_presence")
      elements
    }
  )

  built <- build_example(root)

  kept <- built$traits %>% dplyr::filter(.data$trait_name == "leaf_flush_time")
  expect_setequal(kept$value, c("nnyyynnnnnnn", "yynnnnnnnnnn"))
  expect_true(all(is.na(kept$unit)))

  excluded <- built$excluded_data %>% dplyr::filter(.data$trait_name == "leaf_flush_time")
  expect_setequal(
    paste(excluded$value, excluded$error),
    c("nnyy Times must be length 12", "jan-mar Time can only contain Y & Ns")
  )
})


test_that("a `text` trait's values are kept as written (#87)", {
  hosts <- c("Acacia dealbata", "Eucalyptus viminalis subsp. viminalis", "Zieria", "zieria")
  root <- example_with_traits(
    columns = list(host = hosts),
    edit = function(metadata) {
      metadata$traits <- c(metadata$traits, list(non_numeric_trait("host", "host_name")))
      # A substitution on a `text` trait matches the value exactly, case included
      metadata$substitutions <- list(list(trait_name = "host_name", find = "Zieria", replace = "Zieria smithii"))
      metadata
    },
    edit_definitions = function(elements) {
      elements$host_name <- list(label = "Host plant", type = "text")
      elements
    }
  )

  built <- build_example(root)

  values <- built$traits %>% dplyr::filter(.data$trait_name == "host_name") %>% dplyr::pull(.data$value)
  # Neither lower-cased nor split and sorted, as a categorical value would be
  # ("eucalyptus subsp. viminalis")
  expect_setequal(values, c("Acacia dealbata", "Eucalyptus viminalis subsp. viminalis", "Zieria smithii", "zieria"))
  expect_equal(sum(built$excluded_data$trait_name == "host_name"), 0)

  # Other traits' values are lower-cased as before
  expect_true(all(built$traits$value == tolower(built$traits$value) | built$traits$trait_name == "host_name"))

  # And the dataset passes `dataset_test`, as Test_2023_2 does
  expect_match(fail_count(dataset_test_example(root)), "[ FAIL 0 |", fixed = TRUE)
})


test_that("a trait `type` that is missing or not supported is rejected (#87)", {
  root <- example_with_traits(
    edit_definitions = function(elements) {
      elements$leaf_mass_per_area$type <- "numerical"
      elements$wood_density$type <- NULL
      elements
    }
  )

  offending <- "leaf_mass_per_area (`numerical`), wood_density (no `type`)"

  # Both traits are used by Test_2023_2, so the build stops
  expect_error(
    build_example(root),
    paste0(
      "whose `type` in `traits.yml` is not one of `numeric`, `categorical`, `month_presence`, `text`: ",
      offending
    ),
    fixed = TRUE
  )

  out <- dataset_test_example(root)
  expect_match(
    out,
    paste0(
      "trait definitions - `type` must be one of 'numeric', 'categorical', 'month_presence', 'text': ",
      offending
    ),
    fixed = TRUE, all = FALSE
  )
})


test_that("`dataset_test` checks a `month_presence` trait's substitutions (#87)", {
  # Test_2023_3 maps `flowering_time`, with a valid replacement and one to
  # `.na`; add an invalid one
  root <- withr::local_tempdir()
  dir.create(file.path(root, "data", "Test_2023_3"), recursive = TRUE)
  dir.create(file.path(root, "config"))
  file.copy(list.files(file.path("examples", "Test_2023_3"), full.names = TRUE), file.path(root, "data", "Test_2023_3"))
  file.copy(list.files("config", full.names = TRUE), file.path(root, "config"))

  path_metadata <- file.path(root, "data", "Test_2023_3", "metadata.yml")
  metadata <- read_metadata(path_metadata)
  metadata$substitutions <- c(
    metadata$substitutions,
    list(list(trait_name = "flowering_time", find = "spring", replace = "sep-nov"))
  )
  write_metadata(metadata, path_metadata)

  out <- dataset_test_example(root, "Test_2023_3")
  expect_match(
    out,
    "`flowering_time` has invalid replacement values, which must be twelve `y`s and `n`s: sep-nov",
    fixed = TRUE, all = FALSE
  )
  # The only failure: the valid replacement and `.na` pass
  expect_match(fail_count(out), "[ FAIL 1 |", fixed = TRUE)
})
