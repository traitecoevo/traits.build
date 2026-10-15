test_that("validation of the committed data model finds no failures", {
  report <- validate_ontology()
  failures <- ontology_validation_failures(report)

  expect_length(failures, 0)
  if (length(failures) > 0) {
    print(report)
  }
})

test_that("every entry on the known-gaps register is still reported", {
  # A register entry that nothing reports any more is stale: the gap was fixed
  # without deleting it, or the check that found it was broken. Either way the
  # register no longer describes the published data model.
  report <- validate_ontology()
  reported <- vapply(report, function(p) p$id, character(1))

  stale <- setdiff(names(ONTOLOGY_KNOWN_GAPS), reported)
  expect_length(stale, 0)
  if (length(stale) > 0) {
    message("stale register entries: ", paste(stale, collapse = ", "))
  }
})

test_that("the validator catches a reference that does not resolve", {
  # The checks themselves are tested by planting defects in a copy of the
  # inputs, rather than by re-implementing them here: a second copy of a check
  # would not honour the known-gaps register, and would drift from the first.
  data_dir <- withr::local_tempdir()
  file.copy(list.files(ontology_path("data"), full.names = TRUE), data_dir)

  inputs <- ontology_read_inputs(data_dir)
  references <- ontology_references(inputs$terms)
  external <- references[stringr::str_detect(references$reference, ":"), ][1, ]
  internal <- references[!stringr::str_detect(references$reference, ":"), ][1, ]

  terms <- inputs$terms
  row <- match(external$Class, terms$Class)
  terms[[external$column]][row] <- "dwc:NoSuchClass"
  row <- match(internal$Class, terms$Class)
  terms[[internal$column]][row] <- "no_such_term"
  readr::write_csv(terms, file.path(data_dir, "traits.build_ontology.csv"), na = "")

  report <- validate_ontology(data_dir = data_dir)
  failures <- ontology_validation_failures(report)
  ids <- vapply(failures, function(p) p$id, character(1))

  expect_true("unresolved-reference" %in% ids)
  unresolved <- failures[[match("unresolved-reference", ids)]]$details
  expect_true(any(stringr::str_detect(unresolved, "dwc:NoSuchClass")))

  dangling <- unlist(lapply(report, function(p) if (p$id == "dangling-term-reference") p$details))
  expect_true(any(stringr::str_detect(dangling, "no_such_term")))
})

test_that("the validator catches a namespace with no Turtle prefix", {
  data_dir <- withr::local_tempdir()
  file.copy(list.files(ontology_path("data"), full.names = TRUE), data_dir)

  inputs <- ontology_read_inputs(data_dir)
  references <- ontology_references(inputs$terms)
  classes <- inputs$published_classes
  keys <- paste0(classes$prefix, ":", classes$identifier)
  row <- which(keys %in% references$reference & !is.na(classes$`skos:inScheme`))[1]
  classes$`skos:inScheme`[row] <- "https://example.org/no-such-namespace#"
  readr::write_csv(classes, file.path(data_dir, "traits.build_ontology_published_classes.csv"),
                   na = "")

  report <- validate_ontology(data_dir = data_dir)
  ids <- vapply(ontology_validation_failures(report), function(p) p$id, character(1))
  expect_true("namespace-undeclared" %in% ids)
})

test_that("the built RDF carries no unresolved <NA> term", {
  built <- ontology_test_build()
  for (file in c("traits.build.nq", "traits.build.nt", "traits.build.ttl")) {
    expect_false(any(stringr::str_detect(readr::read_lines(file.path(built, file)), "<NA>")),
                 info = file)
  }
})
