# Sourced first by every script in ontology/scripts/, by ontology/index.qmd and
# by the tests. Finds the repository root, attaches the packages the build
# needs, and loads everything in ontology/R/.
#
# Paths: every function in ontology/R/ takes paths that default to absolute
# locations under the repository root, built with `ontology_path()` and
# `repo_path()`. That is what lets the same code run from `make` at the root,
# from Quarto (whose working directory is ontology/) and from testthat (whose
# working directory is ontology/tests/testthat/) without anyone calling
# setwd().

ONTOLOGY_REPO_ROOT <- local({
  dir <- normalizePath(getwd())
  while (!file.exists(file.path(dir, "DESCRIPTION"))) {
    parent <- dirname(dir)
    if (identical(parent, dir)) {
      stop("could not find the traits.build repository root (no DESCRIPTION ",
           "above ", getwd(), ")", call. = FALSE)
    }
    dir <- parent
  }
  dir
})

#' A path under the repository root
repo_path <- function(...) file.path(ONTOLOGY_REPO_ROOT, ...)

#' A path under ontology/
ontology_path <- function(...) repo_path("ontology", ...)

#' Stop with an installable message if any package is missing
#'
#' `make ontology` is often the first thing a contributor runs, so a missing
#' package should say what to type rather than fail inside a pipeline. None of
#' these are dependencies of the traits.build package itself -- the data model
#' is built by maintainers, not by users of the package -- so they are declared
#' here rather than in DESCRIPTION.
ontology_require <- function(packages, attach = FALSE) {

  installed <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)

  if (!all(installed)) {
    missing <- packages[!installed]
    stop("Missing package(s): ", paste(missing, collapse = ", "),
         "\nInstall with:\n  install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  }

  if (attach) {
    suppressPackageStartupMessages(
      invisible(lapply(packages, library, character.only = TRUE))
    )
  }
}

# The functions in ontology/R/ call dplyr, tidyr, readr and stringr verbs
# unqualified, so those are attached rather than merely installed.
ontology_require(c("dplyr", "tidyr", "readr", "stringr", "tibble", "purrr",
                   "rdflib", "yaml"), attach = TRUE)

# rdflib only *suggests* jsonld, so installing rdflib does not bring it, and
# without it traits.build.json cannot be written. Checked here so the message
# says what to install rather than failing inside rdf_serialize().
ontology_require("jsonld")

for (file in sort(list.files(ontology_path("R"), pattern = "\\.R$",
                             full.names = TRUE))) {
  source(file)
}

# Report warnings where they happen rather than in a batch at the end, so the
# message that precedes them says which step produced them.
options(warn = 1)
