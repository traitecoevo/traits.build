## Ids used to be generated with locale-collated sorting (`sort()` and
## `as.factor()` both follow `LC_COLLATE`), so a build on a contributor's
## machine produced different `observation_id`s, `method_id`s and context ids to
## a build on CI, which runs in the C locale. See issue #29.

build_example <- function(dataset_id) {
  schema <- get_schema()
  resource_metadata <- get_schema("config/metadata.yml", "metadata")
  definitions <- get_schema("config/traits.yml", "traits")
  unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
  # `config/taxon_list.csv` is generated as a side effect of `test-setup.R`, so
  # it does not exist when this file runs first. Read the committed fixture it
  # is copied from instead, so this test does not depend on file ordering.
  taxon_list <- read_csv_char("config/taxon_list-orig.csv")

  build_config <-
    dataset_configure(
      file.path("examples", dataset_id, "metadata.yml"), definitions
    )

  dataset_process(
    file.path("examples", dataset_id, "data.csv"),
    build_config, schema, resource_metadata, unit_conversions
  ) %>%
    dataset_update_taxonomy(taxon_list)
}


test_that("build output does not depend on `LC_COLLATE`", {
  # `test_that` pins collation to C, so a non-C locale has to be set explicitly
  # for this to test anything
  old_collate <- Sys.getlocale("LC_COLLATE")
  withr::defer(Sys.setlocale("LC_COLLATE", old_collate))

  other_locale <- "en_US.UTF-8"
  skip_if(
    suppressWarnings(Sys.setlocale("LC_COLLATE", other_locale)) == "",
    paste(other_locale, "locale is not available")
  )

  # Every example is built under both collations and compared in full, so that
  # any sort added to the build in future is caught wherever it sits.
  # `Test_2023_1` (multi-property entity contexts including NAs), `Test_2023_2`
  # and `Test_2023_4` are the ones that differed before this was fixed
  for (dataset_id in list.dirs("examples", recursive = FALSE, full.names = FALSE)) {

    Sys.setlocale("LC_COLLATE", other_locale)
    built_other <- suppressMessages(suppressWarnings(build_example(dataset_id)))

    Sys.setlocale("LC_COLLATE", "C")
    built_c <- suppressMessages(suppressWarnings(build_example(dataset_id)))

    # `build_info` records the session, including the locale, so is expected
    # to differ
    built_other[["build_info"]] <- NULL
    built_c[["build_info"]] <- NULL

    expect_equal(built_other, built_c, info = dataset_id)
  }
})


test_that("`util_sort_locale_independent` collates in the C locale", {
  old_collate <- Sys.getlocale("LC_COLLATE")
  withr::defer(Sys.setlocale("LC_COLLATE", old_collate))

  # Under a UTF-8 locale `sort()` puts "NA_2" last and ignores case; in the C
  # locale upper case sorts before lower case
  x <- c("male_1", NA, "NA_2", "female_3", "Apple")
  expected <- c("Apple", "NA_2", "female_3", "male_1")

  expect_equal(util_sort_locale_independent(x), expected)

  skip_if(
    suppressWarnings(Sys.setlocale("LC_COLLATE", "en_US.UTF-8")) == "",
    "en_US.UTF-8 locale is not available"
  )
  expect_equal(util_sort_locale_independent(x), expected)

  # Numbering of unique values matches `as.integer(as.factor())` in the C locale
  expect_equal(util_index_locale_independent(x), c(4L, NA, 2L, 3L, 1L))
})
