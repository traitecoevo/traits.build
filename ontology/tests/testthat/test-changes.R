test_that("mapping changes report added, removed and changed matches", {
  dir <- withr::local_tempdir()
  tb <- function(term) paste0("<", ONTOLOGY_BASE, term, ">")
  skos <- function(p) paste0("<http://www.w3.org/2004/02/skos/core#", p, ">")
  dwc <- function(x) paste0("<http://rs.tdwg.org/dwc/terms/", x, ">")
  write_nq <- function(file, ...) {
    readr::write_lines(paste(c(...), "."), file.path(dir, file))
    file.path(dir, file)
  }

  from <- write_nq("from.nq",
    paste(tb("unit"), skos("broader"), dwc("measurementUnit")),
    paste(tb("value"), skos("exactMatch"), dwc("measurementValue")),
    paste(tb("trait"), skos("closeMatch"), tb("trait_name")))
  to <- write_nq("to.nq",
    paste(tb("unit"), skos("exactMatch"), dwc("measurementUnit")),
    paste(tb("trait_name"), skos("exactMatch"), dwc("measurementType")),
    paste(tb("trait"), skos("closeMatch"), tb("trait_name")),
    paste(dwc("measurementType"), skos("prefLabel"), '"Measurement Type"@en'))

  inputs <- list(
    terms = tibble::tibble(Class = c("trait_name", "unit", "value")),
    published_classes = tibble::tibble(
      Entity = "http://rs.tdwg.org/dwc/terms/measurementType",
      prefix = "dwc", identifier = "measurementType"))

  changes <- ontology_mapping_changes(from, to, inputs)

  expect_equal(changes$term, c("trait_name", "unit", "value"))
  expect_equal(changes$change, c("added", "changed", "removed"))
  expect_equal(changes$relation, c("exactMatch", "exactMatch", "exactMatch"))
  expect_equal(changes$previous_relation, c(NA, "broader", NA))
  expect_equal(changes$class[1], "dwc:measurementType")
  expect_equal(changes$class_label[1], "Measurement Type")
})
