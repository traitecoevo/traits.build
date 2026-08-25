# `scaffold_metadata.R` ships in `inst/skills/`, not `R/`, so it isn't part
# of the package namespace -- `source()` it directly, the same way the skill
# itself does. `sys.nframe() == 0` inside that script (rather than plain
# `!interactive()`) is what keeps this `source()` from also trying to run it
# with no arguments; see the comment there if this test starts failing with
# "Usage: Rscript scaffold_metadata.R <answers.yml>".
skill_script <- function(name) {
  system.file("skills", "traits-build-add-dataset", "scripts", name, package = "traits.build")
}

test_that("`scaffold_metadata()` never fills a decision cell", {
  script <- skill_script("scaffold_metadata.R")
  skip_if(!nzchar(script), "scaffold_metadata.R not shipped with this build of the package")
  source(script, local = (env <- new.env()))

  path <- withr::local_tempdir()
  withr::local_dir(path)

  dir.create(file.path("data", "Test_scaffold"), recursive = TRUE)
  readr::write_csv(
    tibble::tibble(
      Species = c("Acacia dealbata", "Eucalyptus regnans"),
      site = c("A", "B"),
      `leaf area (mm2)` = c(120, 340),
      `wood density (mg/mm3)` = c(0.5, 0.6)
    ),
    file.path("data", "Test_scaffold", "data.csv")
  )

  answers <- list(
    dataset_id = "Test_scaffold",
    template = list(
      data_is_long_format = FALSE,
      taxon_name = "Species",
      location_name = "site",
      individual_id = NA,
      collection_date = "2020/2021",
      repeat_measurements_id = FALSE
    ),
    traits = list(var_in = list("leaf area (mm2)", "wood density (mg/mm3)"))
  )
  answers_file <- file.path(path, "answers.yml")
  yaml::write_yaml(answers, answers_file)

  suppressMessages(result <- env$scaffold_metadata(answers_file))

  expect_length(result$traits, 2)
  trait_names <- vapply(result$traits, function(x) x$trait_name, character(1))
  # The non-negotiable this test exists for: `trait_name` is the curator's
  # decision and must never be auto-filled with anything other than the
  # template's own "unknown" placeholder.
  expect_true(all(trait_names == "unknown"))

  # The `var_in`/`unit_in` "from"-side is exactly what the skill IS allowed
  # to fill in.
  var_in <- vapply(result$traits, function(x) x$var_in, character(1))
  expect_setequal(var_in, c("leaf area (mm2)", "wood density (mg/mm3)"))
})

test_that("`scaffold_metadata()` rejects a literal \"NA\" string in the answers file", {
  script <- skill_script("scaffold_metadata.R")
  skip_if(!nzchar(script), "scaffold_metadata.R not shipped with this build of the package")
  source(script, local = (env <- new.env()))

  path <- withr::local_tempdir()
  withr::local_dir(path)
  dir.create(file.path("data", "Test_scaffold"), recursive = TRUE)
  readr::write_csv(tibble::tibble(Species = "Acacia dealbata", site = "A"),
                    file.path("data", "Test_scaffold", "data.csv"))

  # Bare `NA` in YAML parses to the *string* "NA", not real `NA` -- this is
  # the gotcha documented in `references/metadata-fields.md`. Writing the raw
  # YAML text (rather than via `yaml::write_yaml()`, which would correctly
  # emit `.na` for an R `NA`) reproduces the mistake an agent or curator could
  # actually make by hand.
  answers_file <- file.path(path, "answers.yml")
  writeLines(
    c(
      "dataset_id: Test_scaffold",
      "template:",
      "  data_is_long_format: false",
      "  taxon_name: Species",
      "  location_name: site",
      "  individual_id: NA",
      "  collection_date: \"2020/2021\"",
      "  repeat_measurements_id: false"
    ),
    answers_file
  )

  expect_error(env$scaffold_metadata(answers_file), "literal string")
})
