## `omit_values` on a `traits` entry in `metadata.yml` lists values to treat as
## missing for that trait, as `custom_R_code: ~ na_if(.x, ...)` on the trait's
## column does (#12). Replacing one with the other must build an identical
## database, so most tests here build both and compare every table.


tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
            "taxonomic_updates", "taxa", "contributors", "identifiers")

# Copy an example dataset into a fresh data folder, apply `edit_data` to its
# `data.csv` (read as text, so every value keeps its spelling) and
# `edit_metadata` to its metadata, and return the folder
example_edited <- function(dataset_id, edit_metadata = identity, edit_data = identity,
                           env = parent.frame()) {
  path_data <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(path_data, dataset_id))
  file.copy(
    list.files(file.path("examples", dataset_id), full.names = TRUE),
    file.path(path_data, dataset_id), recursive = TRUE
  )

  path_csv <- file.path(path_data, dataset_id, "data.csv")
  data <- readr::read_csv(path_csv, col_types = readr::cols(.default = "c"), progress = FALSE)
  readr::write_csv(edit_data(data), path_csv, na = "")

  path_metadata <- file.path(path_data, dataset_id, "metadata.yml")
  write_metadata(edit_metadata(read_metadata(path_metadata)), path_metadata)

  path_data
}

build_example <- function(dataset_id, path_data) {
  dataset_build(
    file.path(path_data, dataset_id, "metadata.yml"),
    file.path(path_data, dataset_id, "data.csv"),
    get_schema("config/traits.yml", "traits"),
    get_unit_conversions("config/unit_conversions.csv"),
    get_schema(),
    get_schema("config/metadata.yml", "metadata"),
    read_csv_char("config/taxon_list.csv")
  )
}

# Add `code` to the end of the dataset's `custom_R_code` pipeline. As read by
# `read_metadata()`, existing code keeps the quotes it was written in.
append_custom_code <- function(code) {
  function(metadata) {
    existing <- metadata$dataset$custom_R_code
    pipeline <- if (is.na(existing)) "data" else gsub("^\\s*'|'\\s*$", "", existing)
    metadata$dataset$custom_R_code <- sprintf("'%s %%>%% %s'", stringr::str_trim(pipeline), code)
    metadata
  }
}

# Set `omit_values` on the `traits` entries reading `var_in`
set_omit_values <- function(var_in, omit_values) {
  function(metadata) {
    metadata$traits <- lapply(metadata$traits, function(trait) {
      if (trait$var_in %in% var_in) trait$omit_values <- omit_values
      trait
    })
    metadata
  }
}

trait_values <- function(built, trait_name) {
  built$traits$value[built$traits$trait_name == trait_name]
}


test_that("`process_omit_values()` compares as `na_if()` does", {
  # A numeric column is compared as numbers, so `0.0` in `data.csv`, which
  # `read_csv()` reads as 0, is omitted by `0`
  x <- c(0, 0.5, -9999, NA, 12)
  expect_identical(process_omit_values(x, 0L), dplyr::na_if(x, 0))
  expect_identical(process_omit_values(x, list(0L, -9999L)), c(NA, 0.5, NA, NA, 12))
  expect_identical(process_omit_values(x, list("0", "-")), dplyr::na_if(x, 0))
  expect_identical(process_omit_values(c(-Inf, 1), "-Inf"), dplyr::na_if(c(-Inf, 1), -Inf))

  # Any other column is compared as exact text
  y <- c("0", "0.0", "-", "NaN", "nan", NA)
  expect_identical(process_omit_values(y, "-"), dplyr::na_if(y, "-"))
  expect_identical(process_omit_values(y, list(0L, "-", "NaN")), c(NA, "0.0", NA, NA, "nan", NA))

  # Nothing declared leaves the values alone
  expect_identical(process_omit_values(x, NULL), x)
  expect_identical(process_omit_values(y, NULL), y)
})


test_that("`omit_values` must be a string or number, or a list of them", {
  for (valid in list(0L, "-", "", NaN, -Inf, c(0L, -9999L), c("-", "NaN"), list(0L, "-"))) {
    expect_true(util_omit_values_valid(valid), info = deparse(valid))
  }
  # `FALSE` is what YAML makes of an unquoted `no` or `n`, `NA` of `.na`
  for (invalid in list(FALSE, NA, NA_character_, list(), character(0), list(c(1L, 2L)),
                       list(x = 1L), list("a", TRUE), list(list("a")))) {
    expect_false(util_omit_values_valid(invalid), info = deparse(invalid))
  }
})


test_that("`omit_values` builds what `na_if()` builds: wide data, numbers", {
  # Test_2023_2 records two plants with no branches as a `branch mass fraction`
  # of 0. Spell a third `0.0`: `read_csv()` reads both as 0, so `na_if(.x, 0)`
  # omits it, and so must `omit_values: 0`.
  edit_data <- function(data) {
    data[["branch mass fraction"]][3] <- "0.0"
    data
  }

  baseline <- build_example("Test_2023_2", example_edited("Test_2023_2", edit_data = edit_data))
  with_na_if <- build_example("Test_2023_2", example_edited(
    "Test_2023_2", append_custom_code('mutate(across(c("branch mass fraction"), ~ na_if(.x, 0)))'), edit_data
  ))
  with_omit <- build_example("Test_2023_2", example_edited(
    "Test_2023_2", set_omit_values("branch mass fraction", 0L), edit_data
  ))

  expect_identical(with_omit[tables], with_na_if[tables])

  # And it did omit something: all three zeros, from that trait only
  expect_equal(sum(trait_values(baseline, "branch_mass_fraction") == "0"), 3)
  expect_false(any(trait_values(with_omit, "branch_mass_fraction") == "0"))
  expect_equal(nrow(baseline$traits) - nrow(with_omit$traits), 3)
  expect_identical(with_omit$excluded_data, baseline$excluded_data)
})


test_that("`omit_values` builds what `na_if()` builds: wide data, text", {
  # A `-` makes `Seed mass (mg)` a text column, so values are compared as
  # text: `0` omits `0` but not `0.0`
  edit_data <- function(data) {
    data[["Seed mass (mg)"]][c(1, 2)] <- "-"
    data[["Seed mass (mg)"]][4] <- "0"
    data[["Seed mass (mg)"]][5] <- "0.0"
    data
  }

  baseline <- build_example("Test_2023_2", example_edited("Test_2023_2", edit_data = edit_data))
  with_na_if <- build_example("Test_2023_2", example_edited(
    "Test_2023_2",
    append_custom_code('mutate(across(c("Seed mass (mg)"), ~ na_if(na_if(.x, "-"), "0")))'),
    edit_data
  ))
  # A list mixing a number and a string, as YAML reads `[0, "-"]`
  with_omit <- build_example("Test_2023_2", example_edited(
    "Test_2023_2", set_omit_values("Seed mass (mg)", list(0L, "-")), edit_data
  ))

  expect_identical(with_omit[tables], with_na_if[tables])

  seed_mass <- function(built) {
    c(trait_values(built, "seed_dry_mass"),
      built$excluded_data$value[built$excluded_data$trait_name == "seed_dry_mass"])
  }
  expect_true(all(c("-", "0") %in% seed_mass(baseline)))
  expect_false(any(c("-", "0") %in% seed_mass(with_omit)))
  expect_true("0.0" %in% seed_mass(with_omit))
})


test_that("`omit_values` builds what `na_if()` builds: long data, text, scoped to its trait", {
  # In long data every trait shares one value column, so `omit_values` applies
  # only to the rows of the trait declaring it
  edit_data <- function(data) {
    data$value[match(c("SLA (cm2/g)", "leaf longevity"), data$trait_name)] <- "-"
    data
  }

  # Declared on every trait, it is `na_if()` on the whole column
  with_na_if <- build_example("Test_2023_6", example_edited(
    "Test_2023_6", append_custom_code('mutate(value = na_if(value, "-"))'), edit_data
  ))
  with_omit <- build_example("Test_2023_6", example_edited(
    "Test_2023_6", set_omit_values(c("SLA (cm2/g)", "leaf longevity"), "-"), edit_data
  ))
  expect_identical(with_omit[tables], with_na_if[tables])

  # Declared on one trait, it is `na_if()` on that trait's rows
  with_na_if <- build_example("Test_2023_6", example_edited(
    "Test_2023_6",
    append_custom_code('mutate(value = if_else(trait_name == "SLA (cm2/g)", na_if(value, "-"), value))'),
    edit_data
  ))
  with_omit <- build_example("Test_2023_6", example_edited(
    "Test_2023_6", set_omit_values("SLA (cm2/g)", "-"), edit_data
  ))
  expect_identical(with_omit[tables], with_na_if[tables])

  # The other trait's `-` is still there
  built_values <- function(trait_name) {
    c(trait_values(with_omit, trait_name),
      with_omit$excluded_data$value[with_omit$excluded_data$trait_name == trait_name])
  }
  expect_false("-" %in% built_values("leaf_mass_per_area"))
  expect_true("-" %in% built_values("leaf_lifespan"))
})


test_that("`omit_values` builds what `na_if()` builds: long data, numbers", {
  # Keep only the `SLA (cm2/g)` rows of Test_2023_6, so the value column is
  # numeric, and zero two of them, one spelt `0.0`
  edit_data <- function(data) {
    data <- data[data$trait_name == "SLA (cm2/g)", ]
    data$value[c(2, 5)] <- c("0", "0.0")
    data
  }
  only_sla <- function(metadata) {
    metadata$traits <- Filter(function(trait) trait$var_in == "SLA (cm2/g)", metadata$traits)
    metadata
  }

  baseline <- build_example("Test_2023_6", example_edited("Test_2023_6", only_sla, edit_data))
  with_na_if <- build_example("Test_2023_6", example_edited(
    "Test_2023_6",
    function(metadata) append_custom_code("mutate(value = na_if(value, 0))")(only_sla(metadata)),
    edit_data
  ))
  with_omit <- build_example("Test_2023_6", example_edited(
    "Test_2023_6",
    function(metadata) set_omit_values("SLA (cm2/g)", 0L)(only_sla(metadata)),
    edit_data
  ))

  expect_identical(with_omit[tables], with_na_if[tables])
  # Both zeros were in the baseline build, and both are gone
  expect_equal(
    nrow(baseline$traits) + nrow(baseline$excluded_data) -
      nrow(with_omit$traits) - nrow(with_omit$excluded_data),
    2
  )
})


test_that("`dataset_process()` stops on an `omit_values` of the wrong shape", {
  # An unquoted `no` reads as `FALSE`
  path_data <- example_edited("Test_2023_2", set_omit_values("Seed mass (mg)", FALSE))
  expect_error(
    build_example("Test_2023_2", path_data),
    "`omit_values` must be a single string or number, or a list of them, on the `traits` entry for `Seed mass (mg)`",
    fixed = TRUE
  )
})


dataset_test_report <- function(dataset_id, path_data) {
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


test_that("`dataset_test` accepts `omit_values`, as a value or a list", {
  path_data <- example_edited("Test_2023_2", function(metadata) {
    metadata <- set_omit_values("branch mass fraction", 0L)(metadata)
    set_omit_values("Seed mass (mg)", list(0L, "-", "NaN"))(metadata)
  })

  # Test_2023_2 passes every check, so this must too
  out <- dataset_test_report("Test_2023_2", path_data)
  expect_match(tail(grep("^\\[ FAIL", out, value = TRUE), 1), "[ FAIL 0 |", fixed = TRUE)
})


test_that("`dataset_test` rejects an `omit_values` of the wrong shape", {
  path_data <- example_edited("Test_2023_2", function(metadata) {
    metadata <- set_omit_values("branch mass fraction", FALSE)(metadata)
    set_omit_values("Seed mass (mg)", list(list(0L, "-")))(metadata)
  })

  out <- dataset_test_report("Test_2023_2", path_data)
  expect_match(
    out,
    "`omit_values` must be a single string or number, or a list of them: 'branch mass fraction', 'Seed mass (mg)'",
    fixed = TRUE, all = FALSE
  )
})


test_that("`metadata_add_traits()` keeps an existing `omit_values`", {
  # It rewrites every existing trait entry through a table, which would spread
  # a list of values over several rows and keep only the first
  path_data <- example_edited("Test_2023_2", set_omit_values("Seed mass (mg)", list(0L, "-")))
  withr::local_dir(path_data)
  dir.create("data")
  file.rename("Test_2023_2", file.path("data", "Test_2023_2"))

  suppressMessages(metadata_add_traits("Test_2023_2", user_responses = list(var_in = "specimen")))

  traits <- read_metadata(file.path("data", "Test_2023_2", "metadata.yml"))$traits
  var_in <- vapply(traits, function(trait) trait$var_in, character(1))
  expect_equal(sum(var_in == "Seed mass (mg)"), 1)
  expect_identical(traits[[which(var_in == "Seed mass (mg)")]]$omit_values, list(0L, "-"))
  expect_true("specimen" %in% var_in)
  expect_true(all(vapply(traits[var_in != "Seed mass (mg)"], function(x) is.null(x$omit_values), logical(1))))
})
