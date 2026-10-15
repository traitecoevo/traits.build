test_that("a text cell is one statement, a relation cell one per reference", {
  # Every cell used to be split on `; `, which cut comments holding a
  # semicolon into separate statements mid-sentence.
  inputs <- ontology_read_inputs()
  inputs$terms <- inputs$terms[inputs$terms$Class == "establishment_means", ]
  inputs$terms$`rdfs:comment` <- "First clause; second clause."
  inputs$terms$`skos:editorialNote` <- "Close; not exact."
  inputs$terms$relatedMatch <- "dwc:Taxon; dwc:Organism"

  triples <- ontology_convert_to_triples(inputs)
  objects <- function(predicate) {
    triples$Object[triples$Subject == paste0("<", ONTOLOGY_BASE, "establishment_means>") &
                     triples$Predicate == predicate]
  }

  expect_identical(objects("<http://www.w3.org/2000/01/rdf-schema#comment>"),
                   "\"First clause; second clause.\"@en")
  expect_identical(objects("<http://www.w3.org/2004/02/skos/core#editorialNote>"),
                   "\"Close; not exact.\"@en")
  expect_length(objects("<http://www.w3.org/2004/02/skos/core#relatedMatch>"), 2)
})
