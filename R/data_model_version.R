#' Version of the traits.build data model this package builds
#'
#' Every database this version of traits.build builds follows this version of
#' the traits.build data model (<https://w3id.org/traits.build>) and records it
#' in `metadata$data_model_version`, which readers such as `traits.use` check to
#' decide whether they can read the database.
#'
#' The data model is versioned in `ontology/NEWS.md`, whose newest heading must
#' match this version; the ontology tests check that they do. A new major
#' version is a change to the tables an existing reader cannot read, and must be
#' supported by a released `traits.use` before traits.build writes it.
#'
#' @return A version string, `major.minor.patch`.
#' @keywords internal
data_model_version <- function() {
  "1.4.0"
}
