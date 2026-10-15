# The data-model version used to live in the `params` block of index.qmd, where
# it was edited by hand at each release and drove the name of the
# release/<version>/ snapshot. Nothing recorded what had changed between
# versions, and the package's own NEWS.md could not: the data model and the R
# package are versioned separately (the package was at 2.1.0 when the data model
# was at 1.1.0).
#
# ontology/NEWS.md is now the single source. Its newest `## traits.build data
# model <X.Y.Z>` heading is the version being built, the heading below it is the
# previous release, and index.qmd and scripts/release.R read both from here
# rather than repeating them. Cutting a new version means adding a heading.

ONTOLOGY_NEWS_HEADING <- "^##[[:space:]]+traits.build data model[[:space:]]+([0-9]+\\.[0-9]+\\.[0-9]+)"

#' Every version with an entry in the data-model change log, newest first
#'
#' @param path Path to ontology/NEWS.md.
#' @return A character vector of version strings.
ontology_released_versions <- function(path = ontology_path("NEWS.md")) {
  headings <- stringr::str_match(readr::read_lines(path), ONTOLOGY_NEWS_HEADING)
  versions <- headings[!is.na(headings[, 2]), 2]
  if (length(versions) == 0) {
    stop("no `## traits.build data model <X.Y.Z>` heading in ", path,
         call. = FALSE)
  }
  versions
}

#' The version being built: the newest heading in the change log
#'
#' @param path Path to ontology/NEWS.md.
#' @return A version string, e.g. `"1.2.0"`.
ontology_version <- function(path = ontology_path("NEWS.md")) {
  ontology_released_versions(path)[[1]]
}

#' The version before the one being built
#'
#' @param version The version being built.
#' @param path Path to ontology/NEWS.md.
#' @return A version string, or `NA_character_` for the first release.
ontology_previous_version <- function(version = ontology_version(),
                                      path = ontology_path("NEWS.md")) {
  released <- ontology_released_versions(path)
  earlier <- released[released != version]
  if (length(earlier) == 0) NA_character_ else earlier[[1]]
}
