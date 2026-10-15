# The data-model build: ontology/data/ -> the triple table and the four RDF
# serialisations in ontology/output/ontology/.
#
# `ontology/scripts/build_data.R` is the entry point and `make ontology` runs
# it. Everything here is read-only with respect to ontology/data/. The logic is
# the pipeline that used to be the top-level script
# `ontology/build_traits.build_ontology.R`, split into functions that take
# paths so the tests can build into a temporary directory and compare against
# the committed artefacts (see tests/testthat/test-golden.R).

ONTOLOGY_SCHEME <- "https://w3id.org/traits.build"
ONTOLOGY_BASE <- paste0(ONTOLOGY_SCHEME, "#")

# Typed literals are written with the full datatype IRI: `^^<xsd:date>` would
# be a relative IRI, resolved against the document base rather than XML Schema.
XSD <- "http://www.w3.org/2001/XMLSchema#"

# What `ontology_build_data()` writes, relative to its `out_dir`.
ONTOLOGY_OUTPUTS <- c(
  "build_triples.csv", "mapping_changes.csv",
  "traits.build.nq", "traits.build.nt", "traits.build.ttl", "traits.build.json"
)

# The files a release snapshot carries: the four serialisations plus the
# rendered page. build_triples.csv is the intermediate the page is rendered
# from, and has never been part of a snapshot.
ONTOLOGY_RELEASE_FILES <- c(
  "traits.build.json", "traits.build.nq", "traits.build.nt", "traits.build.ttl",
  "index.html"
)

# Prefixes declared in traits.build.ttl. Every namespace a published class
# lives in (its `skos:inScheme` in the published-classes table) must have an
# entry here, or Turtle spells that class's IRI out in full; validate_ontology()
# checks that, so this list and the table cannot drift apart unnoticed. The
# prefix *labels* are not taken from the table because they are part of the
# published Turtle (`oboe:`, not `oboe-core:`), and changing them changes the
# output.
ONTOLOGY_NAMESPACES <- c(
  traits.build = ONTOLOGY_BASE,
  dc = "http://purl.org/dc/elements/1.1/",
  skos = "http://www.w3.org/2004/02/skos/core#",
  dwcattributes = "http://rs.tdwg.org/dwc/terms/attributes/",
  dwc = "http://rs.tdwg.org/dwc/terms/",
  dcterms = "http://purl.org/dc/terms/",
  ets = "http://terminologies.gfbio.org/terms/ETS/",
  obo = "http://purl.obolibrary.org/obo/",
  oboe = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#",
  `oboe-characteristics` = "http://ecoinformatics.org/oboe/oboe.1.2/oboe-characteristics.owl#",
  owl = "http://www.w3.org/2002/07/owl#",
  rdfs = "http://www.w3.org/2000/01/rdf-schema#",
  rdf = "http://www.w3.org/1999/02/22-rdf-syntax-ns#",
  xsd = "http://www.w3.org/2001/XMLSchema#",
  datacite = "http://purl.org/datacite/v4.4/",
  SIO = "http://semanticscience.org/resource/",
  gdmt = "http://vocab.fairdatacollective.org/gdmt/",
  sosa = "http://www.w3.org/ns/sosa/",
  tern = "https://w3id.org/tern/ontologies/tern/",
  terncv = "http://linked.data.gov.au/def/tern-cv/",
  qudt = "http://qudt.org/schema/qudt/"
)

#' Where the build writes, and where the committed artefacts live
ontology_output_dir <- function() ontology_path("output", "ontology")

#' Read the three input tables
#'
#' @param data_dir Directory holding `traits.build_ontology.csv`,
#'   `traits.build_ontology_published_classes.csv` and
#'   `traits.build_resource.csv`.
#' @return A named list of tibbles: `terms`, `published_classes`, `resource`.
ontology_read_inputs <- function(data_dir = ontology_path("data")) {

  # Every cell is text that ends up in a triple. Letting readr guess types
  # turns an ISO date column into a Date, which then cannot be pivoted
  # alongside the text columns.
  read_input <- function(file) {
    readr::read_csv(file.path(data_dir, file),
                    col_types = readr::cols(.default = readr::col_character()))
  }

  list(
    terms = read_input("traits.build_ontology.csv"),
    published_classes = read_input("traits.build_ontology_published_classes.csv"),
    resource = read_input("traits.build_resource.csv")
  )
}

# How each column of traits.build_ontology.csv becomes a predicate. Columns not
# listed here (`Class`, `contextof`) are not emitted.
ONTOLOGY_PREDICATES <- c(
  "<http://www.w3.org/2004/02/skos/core#prefLabel>" = "prefLabel",
  "<http://www.w3.org/2004/02/skos/core#altLabel>" = "altLabel",
  "<http://www.w3.org/2004/02/skos/core#exactMatch>" = "exactMatch",
  "<http://www.w3.org/2004/02/skos/core#broader>" = "broader",
  "<http://semanticscience.org/resource/SIO_000602>" = "computational_entity",
  "<http://www.w3.org/2004/02/skos/core#closeMatch>" = "closeMatch",
  "<http://www.w3.org/2004/02/skos/core#relatedMatch>" = "relatedMatch",
  "<http://rs.tdwg.org/dwc/terms/attributes/organizedInClass>" = "organizedInClass",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasContext>" = "hascontext",
  "<http://www.w3.org/2000/01/rdf-schema#label>" = "label",
  "<http://vocab.fairdatacollective.org/gdmt/Cites>" = "Cites",
  "<http://semanticscience.org/resource/SIO_000628>" = "refers to",
  "<http://semanticscience.org/resource/SIO_000673>" = "has unique identifier",
  "<http://semanticscience.org/resource/SIO_000674>" = "is unique identifier for",
  "<http://semanticscience.org/resource/SIO_000671>" = "has identifier",
  "<http://semanticscience.org/resource/SIO_000672>" = "is identifier for",
  "<http://semanticscience.org/resource/SIO_000255>" = "has annotation",
  "<http://semanticscience.org/resource/SIO_000254>" = "is annotation of",
  "<http://semanticscience.org/resource/SIO_000223>" = "has property",
  "<http://semanticscience.org/resource/SIO_000224>" = "is property of",
  "<http://semanticscience.org/resource/SIO_001096>" = "is specialisation of",
  "<http://semanticscience.org/resource/SIO_000642>" = "is base for",
  "<http://semanticscience.org/resource/SIO_000641>" = "has basis",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#characteristicFor>" = "oboe-core:characteristicFor",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#ofEntity>" = "oboe-core:ofEntity",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasValue>" = "oboe-core:hasValue",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#measurementFor>" = "oboe-core:measurementFor",
  "<http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasMeasurement>" = "oboe-core:hasMeasurement",
  "<http://purl.org/dc/terms/description>" = "dcterms:description",
  "<http://www.w3.org/2000/01/rdf-schema#comment>" = "rdfs:comment",
  "<http://www.w3.org/2004/02/skos/core#editorialNote>" = "skos:editorialNote",
  "<http://www.w3.org/1999/02/22-rdf-syntax-ns#datatype>" = "rdf:datatype",
  "<http://purl.org/dc/terms/created>" = "dcterms:created"
)

#' Every `prefix:identifier` reference the terms table makes
#'
#' A relation cell holds one or more references separated by `;`. A reference
#' containing `:` names a published class; one without names another
#' traits.build term.
#'
#' @param terms The `terms` table from `ontology_read_inputs()`.
#' @return A tibble of `Class`, `column`, `reference`.
ontology_references <- function(terms) {
  terms %>%
    dplyr::select(Class, dplyr::any_of(unname(ONTOLOGY_PREDICATES))) %>%
    dplyr::select(-dplyr::any_of(c("prefLabel", "altLabel", "label",
                                   "dcterms:description", "rdfs:comment",
                                   "skos:editorialNote", "rdf:datatype",
                                   "dcterms:created"))) %>%
    dplyr::mutate(dplyr::across(-Class, as.character)) %>%
    tidyr::pivot_longer(-Class, names_to = "column", values_to = "reference") %>%
    dplyr::filter(!is.na(reference)) %>%
    tidyr::separate_rows(reference, sep = ";") %>%
    dplyr::mutate(reference = stringr::str_trim(reference)) %>%
    dplyr::filter(reference != "")
}

#' Turn the input tables into RDF triples
#'
#' Three sources: the terms table (one row per traits.build term, one column
#' per predicate), the published-classes table (labels and definitions for
#' every external class a term is matched to, so the page can name them), and
#' the resource table (statements about the concept scheme itself, already in
#' triple form).
#'
#' A `prefix:identifier` object is resolved to an IRI through the
#' published-classes table. A reference that resolves to nothing becomes the
#' literal `<NA>`, which is why `validate_ontology()` checks every reference
#' first.
#'
#' @param inputs Output of `ontology_read_inputs()`.
#' @return A tibble of `Subject`, `Predicate`, `Object`, `graph`.
ontology_convert_to_triples <- function(inputs) {

  published_classes_csv <-
    inputs$published_classes %>%
    dplyr::mutate(identifier2 = paste0(prefix, ":", identifier))

  reformatted_classes <-
    published_classes_csv %>%
    dplyr::select(dplyr::all_of(c(
      "Entity", "skos:prefLabel", "dcterms:description", "rdfs:comment", "skos:inScheme"
    ))) %>%
    dplyr::rename(dplyr::all_of(c(
      "Subject" = "Entity",
      "<http://www.w3.org/2004/02/skos/core#prefLabel>" = "skos:prefLabel",
      "<http://purl.org/dc/terms/description>" = "dcterms:description",
      "<http://www.w3.org/2000/01/rdf-schema#comment>" = "rdfs:comment",
      "<http://www.w3.org/2004/02/skos/core#inScheme>" = "skos:inScheme"
    ))) %>%
    tidyr::pivot_longer(cols = -Subject) %>%
    dplyr::rename(Predicate = name, Object = value) %>%
    dplyr::filter(!is.na(Object)) %>%
    dplyr::mutate(
      Subject = paste0("<", Subject, ">"),
      Object = ifelse(stringr::str_detect(Predicate, "inScheme"),
                      paste0("<", Object, ">"),
                      paste0("\"", Object, "\"@en"))
    )

  reformatted_ontology <-
    inputs$terms %>%
    dplyr::select(-dplyr::any_of("contextof")) %>%
    dplyr::rename(dplyr::all_of(c("Subject" = "Class", ONTOLOGY_PREDICATES))) %>%
    dplyr::mutate(
      `<http://www.w3.org/2000/01/rdf-schema#label>` = `<http://www.w3.org/2004/02/skos/core#prefLabel>`,
      `<http://www.w3.org/2002/07/owl#sameAs>` = `<http://www.w3.org/2004/02/skos/core#exactMatch>`,
      `<http://www.w3.org/2004/02/skos/core#inScheme>` = paste0("<", ONTOLOGY_SCHEME, ">")
    ) %>%
    dplyr::mutate(Subject = paste0("<", ONTOLOGY_BASE, Subject, ">")) %>%
    tidyr::pivot_longer(cols = -Subject) %>%
    dplyr::rename(Predicate = name, Object_tmp = value) %>%
    dplyr::filter(!is.na(Object_tmp)) %>%
    dplyr::mutate(Object_tmp = stringr::str_split(Object_tmp, "\\;\\ ")) %>%
    tidyr::unnest_longer(Object_tmp) %>%
    dplyr::mutate(Object_tmp = stringr::str_trim(Object_tmp)) %>%
    dplyr::mutate(
      Object = ifelse(stringr::str_detect(Predicate, "description|[Ll]abel|[Cc]omment|editorialNote"),
                      paste0("\"", Object_tmp, "\"@en"),
                      NA),
      Object = ifelse(!stringr::str_detect(Object_tmp, "\\:") &
                        !stringr::str_detect(Predicate, "description|label|comment|editorialNote") &
                        is.na(Object),
                      paste0("<", ONTOLOGY_BASE, stringr::str_trim(Object_tmp), ">"),
                      Object),
      Object = ifelse(stringr::str_detect(Object_tmp, "\\:") & is.na(Object),
                      paste0("<", published_classes_csv$Entity[match(Object_tmp, published_classes_csv$identifier2)], ">"),
                      Object),
      Object = ifelse(stringr::str_detect(Predicate, "\\#datatype"),
                      paste0("<", Object_tmp, ">"),
                      Object),
      Object = ifelse(stringr::str_detect(Predicate, "inScheme"),
                      paste0("<", ONTOLOGY_SCHEME, ">"),
                      Object),
      Object = ifelse(stringr::str_detect(Predicate, "created"),
                      paste0("\"", Object_tmp, "\"", "^^<", XSD, "date>"),
                      Object),
      Subject = ifelse(Subject == paste0("<", ONTOLOGY_BASE, "traits.build>"),
                       paste0("<", ONTOLOGY_SCHEME, ">"),
                       Subject)
    ) %>%
    dplyr::select(-Object_tmp)

  reformatted_ontology %>%
    dplyr::bind_rows(reformatted_classes) %>%
    dplyr::bind_rows(inputs$resource) %>%
    dplyr::mutate(graph = ".")
}

#' Add human-readable labels to the triples, for the website
#'
#' @param triples Output of `ontology_convert_to_triples()`.
#' @param inputs Output of `ontology_read_inputs()`.
#' @return The triples with `predicate_tmp`, `object_tmp`, `subject_tmp`
#'   (the IRIs stripped of their brackets and datatypes) and `Predicate_labels`,
#'   `Object_labels`, `Subject_labels`.
ontology_label_triples <- function(triples, inputs) {

  triples <- triples %>%
    dplyr::mutate(
      predicate_tmp = stringr::str_replace(Predicate, "<", ""),
      predicate_tmp = stringr::str_replace(predicate_tmp, ">", ""),
      # A typed literal's datatype is a full XML Schema IRI in the RDF; shorten it
      # to the xsd: form the label stripping below expects.
      object_tmp = stringr::str_replace(Object, stringr::fixed(paste0("^^<", XSD)), "^^<xsd:"),
      object_tmp = stringr::str_replace_all(object_tmp, "<", ""),
      object_tmp = stringr::str_replace_all(object_tmp, ">", ""),
      object_tmp = stringr::str_replace(object_tmp, "\\^\\^", ""),
      object_tmp = stringr::str_replace(object_tmp, "xsd:date", ""),
      object_tmp = stringr::str_replace(object_tmp, "xsd:anyURI", ""),
      object_tmp = stringr::str_replace(object_tmp, "xsd:Name", ""),
      object_tmp = stringr::str_replace(object_tmp, "xsd:int", ""),
      object_tmp = stringr::str_replace(object_tmp, "xsd:decimal", ""),
      object_tmp = stringr::str_replace(object_tmp, "@en", ""),
      # Only the quotes of a literal: stripping any trailing punctuation also
      # cut the `/` or `#` that ends a namespace or licence IRI.
      object_tmp = stringr::str_replace(object_tmp, "\"$", ""),
      object_tmp = stringr::str_replace(object_tmp, "^\"", ""),
      subject_tmp = stringr::str_replace(Subject, paste0("<", ONTOLOGY_BASE), ""),
      subject_tmp = stringr::str_replace(subject_tmp, "<", ""),
      subject_tmp = stringr::str_replace(subject_tmp, ">", "")
    )

  labels <- triples %>%
    dplyr::filter(predicate_tmp %in% c("http://www.w3.org/2004/02/skos/core#prefLabel",
                                       "http://www.w3.org/2004/02/skos/core#altLabel")) %>%
    dplyr::mutate(
      subject_tmp = stringr::str_replace(Subject, "<", ""),
      subject_tmp = stringr::str_replace(subject_tmp, ">", "")
    )

  published_classes_csv <- inputs$published_classes

  triples %>%
    dplyr::mutate(
      Predicate_labels = published_classes_csv$`skos:prefLabel`[match(predicate_tmp, published_classes_csv$Entity)],
      Object_labels = ifelse(stringr::str_detect(object_tmp, "^http"),
                             labels$object_tmp[match(object_tmp, labels$subject_tmp)],
                             object_tmp),
      Object_labels = ifelse(is.na(Object_labels), object_tmp, Object_labels),
      Subject_labels = inputs$terms$prefLabel[match(subject_tmp, inputs$terms$Class)]
    )
}

#' Write the RDF serialisations
#'
#' N-Quads and N-Triples are written directly from the triple table; Turtle and
#' JSON-LD are serialised by `rdflib` from the N-Quads, which also proves the
#' output parses as RDF.
#'
#' @param triples Output of `ontology_convert_to_triples()`.
#' @param out_dir Directory to write into.
#' @return The parsed RDF graph, invisibly.
ontology_write_rdf <- function(triples, out_dir = ontology_output_dir()) {

  nq <- file.path(out_dir, "traits.build.nq")

  triples %>%
    readr::write_delim(nq, col_names = FALSE, escape = "none", quote = "none")

  # Every statement is in the default graph, so the `graph` column holds only
  # the terminating `.` and the N-Quads file is already valid N-Triples.
  # (Dropping that column, as the nt writer once did, dropped the terminator.)
  file.copy(nq, file.path(out_dir, "traits.build.nt"), overwrite = TRUE)

  graph <- rdflib::rdf_parse(nq, format = "nquads")

  rdflib::rdf_serialize(graph, file.path(out_dir, "traits.build.ttl"),
                        namespace = ONTOLOGY_NAMESPACES)
  rdflib::rdf_serialize(graph, file.path(out_dir, "traits.build.json"),
                        format = "jsonld")

  invisible(graph)
}

#' Build the data model from ontology/data/
#'
#' Writes the files in `ONTOLOGY_OUTPUTS`: the labelled triple table the
#' website renders from, the mapping changes since the previous release, and
#' the four RDF serialisations.
#'
#' @param data_dir Directory holding the input tables.
#' @param out_dir Directory to write outputs into.
#' @return The number of triples written, invisibly.
ontology_build_data <- function(data_dir = ontology_path("data"),
                                out_dir = ontology_output_dir()) {

  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  message("Reading inputs from ", data_dir, "/")
  inputs <- ontology_read_inputs(data_dir)

  message("Building triples")
  triples <- ontology_convert_to_triples(inputs)

  message("Serialising RDF (nq, nt, ttl, json)")
  ontology_write_rdf(triples, out_dir)

  message("Writing build_triples.csv")
  triples %>%
    ontology_label_triples(inputs) %>%
    readr::write_csv(file.path(out_dir, "build_triples.csv"))

  # Against the previous release's committed snapshot, wherever the build is
  # written, so a test build in a temp dir produces the same file.
  baseline <- ontology_baseline_nq()
  if (!is.na(baseline)) {
    message("Writing mapping_changes.csv (against ", ontology_previous_version(), ")")
    ontology_mapping_changes(baseline, file.path(out_dir, "traits.build.nq"), inputs) %>%
      readr::write_csv(file.path(out_dir, "mapping_changes.csv"), na = "")
  }

  invisible(nrow(triples))
}
