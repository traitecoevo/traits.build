#!/usr/bin/env Rscript
# `make ontology-check` -- validate the built data model and run its tests.
#
# Two severities, deliberately:
#
#   FAIL  a regression -- a missing output, an unresolved reference, RDF that
#         will not parse, or any validation problem that is not on the
#         known-gaps register. Exits non-zero.
#   gap   a defect that is already in the published data model and is listed
#         in ONTOLOGY_KNOWN_GAPS (ontology/R/validate.R). Reported in full
#         every run, but does not fail: each one changes published RDF when
#         fixed, so each needs its own reviewed change.
#
# The register is what keeps this honest. Anything NOT on it fails, so new
# breakage cannot hide behind the existing debt.

source("ontology/scripts/setup.R")
ontology_require(c("testthat", "withr"))

failures <- character()

report <- function(status, label, detail = NULL) {
  mark <- c(ok = "  ok  ", fail = " FAIL ", gap = " gap  ")[[status]]
  cat(sprintf("[%s] %s\n", mark, label))
  if (!is.null(detail)) cat(paste0("         ", detail, "\n", collapse = ""))
  if (status == "fail") failures <<- c(failures, label)
}

cat("Data model ", ontology_version(), " (previous: ",
    ontology_previous_version(), ")\n\n", sep = "")

# --- validation -------------------------------------------------------------

validation <- validate_ontology()
print(validation)
cat("\n")

n_fail <- length(ontology_validation_failures(validation))
n_gap <- length(validation) - n_fail
if (n_fail > 0) {
  report("fail", sprintf("validation: %d failure(s), %d known gap(s)", n_fail, n_gap))
} else {
  report("ok", sprintf("validation: no failures, %d known gap(s)", n_gap))
}

# --- tests -------------------------------------------------------------------
#
# test-golden.R rebuilds into a temporary directory and compares against the
# committed artefacts in ontology/output/ontology/, so an unintended change to
# the published RDF fails here.

cat("\n")
results <- testthat::test_dir(ontology_path("tests", "testthat"),
                              reporter = "summary", stop_on_failure = FALSE)
summary <- as.data.frame(results)
n_failed <- sum(summary$failed) + sum(summary$error)
if (n_failed > 0) {
  report("fail", sprintf("tests: %d failure(s)", n_failed))
} else {
  report("ok", sprintf("tests: %d expectation(s) passed, %d skipped",
                       sum(summary$passed), sum(summary$skipped)))
}

# --- verdict ------------------------------------------------------------------

cat("\n")
if (length(failures) > 0) {
  cat("FAILED:", length(failures), "check(s)\n")
  quit(status = 1)
}
cat("All checks passed.\n")
