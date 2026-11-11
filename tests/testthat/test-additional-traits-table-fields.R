## `additional_traits_table_fields` in the `metadata` block of `config/metadata.yml`
## adds a database's own columns to the traits table, which each dataset sets
## as it sets `measurement_remarks` (#314).
##
## `Test_2023_1` is a wide dataset with a `species_abb` column; `Test_2023_3` is
## long, and a copy of it is given extra columns to read the fields from.

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")

fields <- list(
  specimen_type = "The kind of specimen the value was measured on.",
  taxonomic_verification = "Whether the identification was verified by a taxonomist."
)

with_fields <- function(fields) {
  m <- resource_metadata
  m[["additional_traits_table_fields"]] <- fields
  m
}

build_example <- function(id, metadata = resource_metadata, edit = identity, path = "examples") {
  config <- dataset_configure(file.path(path, id, "metadata.yml"), definitions)
  config[["metadata"]] <- edit(config[["metadata"]])
  dataset_process(file.path(path, id, "data.csv"), config, schema, metadata, unit_conversions)
}

# Sets `value` for `field` on the `traits` entry whose `var_in` is `var_in`
set_on_trait <- function(metadata, var_in, field, value) {
  i <- which(vapply(metadata[["traits"]], function(x) identical(x[["var_in"]], var_in), logical(1)))
  expect_length(i, 1)
  metadata[["traits"]][[i]][[field]] <- value
  metadata
}


test_that("`util_additional_traits_table_fields` reads the setting and rejects invalid ones", {
  expect_equal(util_additional_traits_table_fields(list()), character())
  expect_equal(util_additional_traits_table_fields(list(additional_traits_table_fields = NA)), character())
  expect_equal(util_additional_traits_table_fields(with_fields(fields)), unlist(fields))

  not_a_map <- "must map each column name to a description"
  expect_error(util_additional_traits_table_fields(with_fields("specimen_type")), not_a_map)
  expect_error(util_additional_traits_table_fields(with_fields(list("a", "b"))), not_a_map)
  expect_error(util_additional_traits_table_fields(with_fields(list(specimen_type = ""))), not_a_map)
  expect_error(util_additional_traits_table_fields(with_fields(list(specimen_type = NULL))), not_a_map)
  expect_error(util_additional_traits_table_fields(with_fields(list(specimen_type = c("a", "b")))), not_a_map)

  expect_error(
    util_additional_traits_table_fields(with_fields(list(specimen_type = "a", specimen_type = "b"))),
    "names a column more than once: `specimen_type`"
  )
  expect_error(
    util_additional_traits_table_fields(with_fields(list(`specimen type` = "a", `1st_check` = "b"))),
    "only letters, digits and underscores: `specimen type`, `1st_check`"
  )
  for (v in c("life_stage", "original_name", "error", "var_in", "omit_values", "location_name", "custom_R_code", "parsing_id")) {
    expect_error(
      util_additional_traits_table_fields(with_fields(list(specimen_type = "a", x = "b") |> setNames(c("specimen_type", v)))),
      sprintf("has names the schema already uses: `%s`", v),
      info = v
    )
  }
})


test_that("the columns are added to the schema after its own, once however often they are added", {
  expect_identical(util_schema_add_traits_table_fields(schema, character()), schema)

  added <- util_schema_add_traits_table_fields(schema, unlist(fields))
  expect_identical(util_schema_add_traits_table_fields(added, unlist(fields)), added)

  traits_columns <- names(schema[["austraits"]][["elements"]][["traits"]][["elements"]])
  expect_equal(
    names(added[["austraits"]][["elements"]][["traits"]][["elements"]]),
    c(traits_columns, names(fields))
  )
  expect_equal(
    names(added[["austraits"]][["elements"]][["excluded_data"]][["elements"]]),
    c(names(schema[["austraits"]][["elements"]][["excluded_data"]][["elements"]]), names(fields))
  )
  expect_equal(added[["metadata"]][["elements"]][["dataset"]][["values"]][["specimen_type"]], fields$specimen_type)
  expect_equal(added[["metadata"]][["elements"]][["traits"]][["elements"]][["specimen_type"]], fields$specimen_type)

  # The schema documents the setting itself
  expect_true(
    "additional_traits_table_fields" %in% names(schema[["austraits"]][["elements"]][["metadata"]][["elements"]])
  )
})


test_that("without the setting the build is unchanged, and with it a dataset setting nothing gains empty columns", {
  # The examples' config sets nothing, so this is also what the golden fixtures pin
  expect_null(resource_metadata[["additional_traits_table_fields"]])

  for (id in c("Test_2023_1", "Test_2023_3")) {
    default <- build_example(id)
    expect_identical(default$schema, schema, info = id)
    expect_equal(
      setdiff(names(default$traits), "taxonomic_resolution"),
      names(schema[["austraits"]][["elements"]][["traits"]][["elements"]]),
      info = id
    )

    with_columns <- build_example(id, with_fields(fields))
    for (v in c("traits", "excluded_data")) {
      # `taxonomic_resolution` is kept until `dataset_update_taxonomy()` drops it
      expect_equal(
        setdiff(names(with_columns[[v]]), "taxonomic_resolution"),
        c(setdiff(names(default[[v]]), "taxonomic_resolution"), names(fields)),
        info = paste(id, v)
      )
      expect_true(all(is.na(with_columns[[v]][["specimen_type"]])), info = paste(id, v))
      expect_equal(dplyr::select(with_columns[[v]], -dplyr::all_of(names(fields))), default[[v]], info = paste(id, v))
    }
    for (v in c("locations", "contexts", "methods")) {
      expect_equal(with_columns[[v]], default[[v]], info = paste(id, v))
    }
    expect_equal(
      with_columns$schema[["austraits"]][["elements"]][["traits"]][["elements"]][["specimen_type"]],
      fields$specimen_type,
      info = id
    )
  }
})


test_that("a wide dataset sets the columns in its `dataset` block and per trait, as values or columns", {
  built <- build_example(
    "Test_2023_1", with_fields(fields),
    edit = function(m) {
      m[["dataset"]][["specimen_type"]] <- "herbarium_sheet"
      m[["dataset"]][["taxonomic_verification"]] <- "species_abb"
      set_on_trait(m, "LMA (mg mm-2)", "specimen_type", "fresh_leaf")
    }
  )
  traits <- built$traits

  # A value set on a trait takes precedence over the `dataset` block's
  lma <- traits$trait_name == "leaf_mass_per_area"
  expect_gt(sum(lma), 0)
  expect_true(all(traits$specimen_type[lma] == "fresh_leaf"))
  expect_true(all(traits$specimen_type[!lma] == "herbarium_sheet"))

  # A `dataset` value naming a column is read record by record; a blank cell
  # leaves the record's value empty
  data <- readr::read_csv("examples/Test_2023_1/data.csv", col_types = readr::cols(.default = "c"))
  expected <- data$species_abb[match(traits$original_name, data$Species)]
  expect_equal(traits$taxonomic_verification, expected)
  expect_gt(length(unique(stats::na.omit(traits$taxonomic_verification))), 1)

  expect_true(all(built$excluded_data$specimen_type %in% c("herbarium_sheet", "fresh_leaf")))

  # The fields describe records, not methods, so stay out of the methods table
  expect_equal(names(built$methods), names(build_example("Test_2023_1")$methods))
  expect_equal(built$methods, build_example("Test_2023_1")$methods)

  # Once taxonomy is updated, the columns are the table's last
  updated <- dataset_update_taxonomy(built, read_csv_char("config/taxon_list.csv"))
  expect_equal(
    names(updated$traits),
    c(names(schema[["austraits"]][["elements"]][["traits"]][["elements"]]), names(fields))
  )
})


test_that("a long dataset sets the columns in its `dataset` block and per trait, as values or columns", {
  dir <- file.path(withr::local_tempdir(), "Test_2023_3")
  dir.create(dir)
  file.copy("examples/Test_2023_3/metadata.yml", dir)
  data <- readr::read_csv("examples/Test_2023_3/data.csv", col_types = readr::cols(.default = "c"))
  genus <- stringr::word(data$taxon_name, 1)
  data$verified_by <- paste0("checker_", genus)
  data$specimen_col <- paste0("specimen_", genus)
  readr::write_csv(data, file.path(dir, "data.csv"), na = "")

  built <- build_example(
    "Test_2023_3", with_fields(fields), path = dirname(dir),
    edit = function(m) {
      m[["dataset"]][["specimen_type"]] <- "flora_description"
      m[["dataset"]][["taxonomic_verification"]] <- "verified_by"
      set_on_trait(m, "fruit_colour", "specimen_type", "specimen_col")
    }
  )
  traits <- built$traits
  genus <- stringr::word(traits$original_name, 1)

  # A trait's entry naming a column reads it on that trait's rows only
  colour <- traits$trait_name == "fruit_colour"
  expect_gt(sum(colour), 0)
  expect_equal(traits$specimen_type[colour], paste0("specimen_", genus[colour]))
  expect_true(all(traits$specimen_type[!colour] == "flora_description"))

  expect_equal(traits$taxonomic_verification, paste0("checker_", genus))
})


test_that("`dataset_test` accepts the fields only when the database declares them", {
  path_config <- withr::local_tempdir()
  file.copy(list.files("config", full.names = TRUE), path_config, recursive = TRUE)
  config_metadata <- yaml::read_yaml(file.path(path_config, "metadata.yml"))

  # A copy of `Test_2023_1` that sets `specimen_type` in its `dataset` block and on a trait
  path_data <- withr::local_tempdir()
  dir <- file.path(path_data, "Test_2023_1_fields")
  dir.create(dir)
  file.copy(file.path("examples/Test_2023_1", c("data.csv", "metadata.yml")), dir)
  lines <- readLines(file.path(dir, "metadata.yml"))
  i <- which(lines == "  notes: none")[1]
  j <- which(lines == "- var_in: LMA (mg mm-2)")
  expect_length(j, 1)
  lines <- append(lines, "  specimen_type: fresh_leaf", after = j)
  lines <- append(lines, "  specimen_type: herbarium_sheet", after = i)
  writeLines(lines, file.path(dir, "metadata.yml"))

  failures <- function(id, setting) {
    config_metadata$metadata$additional_traits_table_fields <- setting
    yaml::write_yaml(config_metadata, file.path(path_config, "metadata.yml"))
    reporter <- testthat::ListReporter$new()
    testthat::with_reporter(
      reporter,
      dataset_test_worker(id, path_config = path_config, path_data = path_data)
    )
    sum(as.data.frame(reporter$get_results())$failed)
  }

  file.copy("examples/Test_2023_1", path_data, recursive = TRUE)
  baseline <- failures("Test_2023_1", NULL)

  # Example datasets have known failures (blank dates), so compare with a copy setting nothing
  expect_equal(failures("Test_2023_1_fields", fields), baseline)
  expect_equal(failures("Test_2023_1", fields), baseline)
  expect_gt(failures("Test_2023_1_fields", NULL), baseline)
  expect_gt(failures("Test_2023_1", list(life_stage = "Clashes with the schema.")), baseline)
})
