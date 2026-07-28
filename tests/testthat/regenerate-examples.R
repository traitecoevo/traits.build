## Regenerate the golden output files under `examples/Test_2023_*/output/`.
##
## Run from `tests/testthat/`:
##
##     Rscript regenerate-examples.R              # all datasets
##     Rscript regenerate-examples.R Test_2023_1  # just one
##
## then read `git diff` before committing. Every diff is either a fix you meant
## to make or a regression -- there is no third case, and nothing here should be
## accepted without being understood. Until this script existed the only way to
## update a fixture was to hand-edit it towards the observed output, which is
## how `identifiers.csv` came to hold values no build had produced in years.
##
## The build is wrapped in the same settings `test_that()` applies, so what this
## writes is what the suite compares against. That wrapping is deliberately
## explicit: when it stops being necessary, the build has become properly
## independent of the session, which is the goal.

devtools::load_all("../..", quiet = TRUE)

args <- commandArgs(trailingOnly = TRUE)

dataset_ids <-
  if (length(args) > 0) {
    args
  } else {
    list.dirs("examples", recursive = FALSE, full.names = FALSE)
  }

# `test_that()` pins both of these; a bare `Rscript` session does not. Ids are
# derived from collated order (issue #29) and citations from `useFancyQuotes`,
# so without this the fixtures would not match what the suite builds.
old_collate <- Sys.getlocale("LC_COLLATE")
old_quotes <- getOption("useFancyQuotes")
on.exit({
  Sys.setlocale("LC_COLLATE", old_collate)
  options(useFancyQuotes = old_quotes)
}, add = TRUE)

Sys.setlocale("LC_COLLATE", "C")
options(useFancyQuotes = FALSE)

schema <- get_schema()
resource_metadata <- get_schema("config/metadata.yml", "metadata")
definitions <- get_schema("config/traits.yml", "traits")
unit_conversions <- get_unit_conversions("config/unit_conversions.csv")
taxon_list <- read_csv_char("config/taxon_list-orig.csv")

for (dataset_id in dataset_ids) {

  message("Rebuilding ", dataset_id)

  build_config <-
    dataset_configure(
      file.path("examples", dataset_id, "metadata.yml"), definitions
    )

  built <-
    dataset_process(
      file.path("examples", dataset_id, "data.csv"),
      build_config, schema, resource_metadata, unit_conversions
    ) %>%
    dataset_update_taxonomy(taxon_list)

  path <- file.path("examples", dataset_id, "output")
  write_plaintext(built, path)

  # `build_info` records the R version, platform and time of the build, and
  # `schema.yml` is a verbatim copy of the package's own schema, so neither
  # tells us anything by being compared against itself. Both are dropped rather
  # than committed as fixtures that would churn on every run.
  unlink(file.path(path, c("build_info.md", "schema.yml")))
}

message("\nDone. Now read `git diff` -- do not commit a change you cannot explain.")
