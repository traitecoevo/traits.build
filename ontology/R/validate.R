# Validation for the built data model.
#
# `validate_ontology()` returns a report of every problem it can find. Each
# problem carries an id, and an id listed in ONTOLOGY_KNOWN_GAPS is reported as
# a known gap rather than a failure. The register is the point: the checks are
# strict, the existing published defects are enumerated with a reason, and
# anything NOT on the list fails. Fixing a gap means deleting its entry, in the
# same change that fixes it -- tests/testthat/test-validate.R fails on a
# register entry that nothing reports any more.
#
# Every entry below is a defect in the RDF as published since data model 0.1.0.
# Each one changes what consumers parse when fixed, so each needs its own
# reviewed change and a data-model version to ship in, which is why they are
# registered rather than silently fixed by the refactor that introduced the
# checks.

ONTOLOGY_KNOWN_GAPS <- c(
  `input-duplicate-key` = paste(
    "`taxon` is the Class of two rows: the entity observed (broader",
    "oboe:Entity) and the column of the taxa table (exact dwc:Taxon). Both",
    "become the one IRI https://w3id.org/traits.build#taxon, which therefore",
    "carries two descriptions. Fixing it renames a published IRI; needs a",
    "maintainer decision on which row moves."),
  `nt-unterminated` = paste(
    "traits.build.nt is written without the statement terminator, so no line",
    "ends in `.` and the file is not valid N-Triples (traits.build.nq, which",
    "carries the terminator in its graph column, is what rdflib parses).",
    "Fixing it rewrites every line of a published file."),
  `rdf-datatype-relative-uri` = paste(
    "typed literals are written `^^<xsd:date>` rather than",
    "`^^<http://www.w3.org/2001/XMLSchema#date>`, so the datatype is a",
    "relative IRI that resolves against the base rather than XML Schema;",
    "the dates themselves (`1-Feb-24`) are not valid xsd:date lexical forms",
    "either. Fixing it changes every dcterms:created and resource literal."),
  `rdf-uri-inside-literal` = paste(
    "traits.build_resource.csv writes the licence, publisher and top-concept",
    "IRIs as string literals in angle brackets (`\"<https://...>\"^^<xsd:anyURI>`)",
    "rather than as IRIs, and the top-concept literal names",
    "`traits.build-database` without the `#`. Fixing it changes four",
    "statements about the concept scheme."),
  `rdf-rdfs-type` = paste(
    "the concept scheme's type is asserted with rdfs:type, which does not",
    "exist; rdf:type is meant. One statement in traits.build_resource.csv."),
  # Input defects in the published-classes and terms tables, as found by these
  # checks on arrival. Each is fixed in its own commit, which also deletes its
  # entry here; they are registered so that the commit that introduced the
  # checks is itself output-neutral.
  `published-class-missing-field` = paste(
    "12 published classes have no skos:inScheme, so no inScheme statement is",
    "emitted for them, and two rows (the licence and publisher URLs) carry an",
    "Entity and nothing else."),
  `published-class-duplicate-key` = paste(
    "dwc:Taxon is listed twice, identically, so its four statements are",
    "written twice; the two label-less rows share the key NA:NA."),
  `dangling-term-reference` = paste(
    "identifier_value's `has identifier` cell separates its two references with",
    "a comma rather than `;`, so the build emits one IRI",
    "<...#identifier_type, institution_code> that does not parse and the",
    "statement is dropped."),
  `rdf-statements-dropped` = paste(
    "the malformed IRI above makes one N-Quads line unparseable, and the",
    "duplicated dwc:Taxon rows repeat four more; the parsed graph is smaller",
    "than the file.")
)

new_problem <- function(id, message, details = character()) {
  list(id = id, message = message, details = as.character(details))
}

#' Validate the inputs and the built data model
#'
#' @param data_dir Directory holding the input tables.
#' @param out_dir Directory holding the build outputs.
#' @return An `ontology_validation` object: a list of problems, each with `id`,
#'   `message`, `details` and `severity` (`"error"` or `"known gap"`).
validate_ontology <- function(data_dir = ontology_path("data"),
                              out_dir = ontology_output_dir()) {

  problems <- list()
  add <- function(...) problems[[length(problems) + 1]] <<- new_problem(...)

  inputs <- ontology_read_inputs(data_dir)
  terms <- inputs$terms
  classes <- inputs$published_classes

  # --- the terms table -----------------------------------------------------

  keys <- terms$Class
  repeated <- unique(keys[duplicated(keys) & !is.na(keys)])
  if (length(repeated) > 0) {
    add("input-duplicate-key",
        sprintf("traits.build_ontology.csv: %d `Class` value(s) occur on more than one row",
                length(repeated)),
        repeated)
  }

  padded <- keys[!is.na(keys) & keys != stringr::str_trim(keys)]
  if (length(padded) > 0) {
    add("input-padded-key",
        sprintf("%d `Class` value(s) carry leading or trailing whitespace", length(padded)),
        padded)
  }

  missing_text <- is.na(terms$prefLabel) | is.na(terms$`dcterms:description`)
  if (any(missing_text)) {
    add("term-missing-required-field",
        sprintf("%d term(s) have no prefLabel or no description", sum(missing_text)),
        terms$Class[missing_text])
  }

  # --- the published-classes table ------------------------------------------

  required <- c("Entity", "skos:prefLabel", "prefix", "identifier", "skos:inScheme")
  for (column in required) {
    blank <- is.na(classes[[column]]) | classes[[column]] == ""
    if (any(blank)) {
      add("published-class-missing-field",
          sprintf("%d published class(es) have no `%s`", sum(blank), column),
          classes$Entity[blank])
    }
  }

  class_keys <- paste0(stringr::str_trim(classes$prefix), ":",
                       stringr::str_trim(classes$identifier))
  repeated <- unique(class_keys[duplicated(class_keys)])
  if (length(repeated) > 0) {
    add("published-class-duplicate-key",
        sprintf("%d `prefix:identifier` key(s) occur on more than one published class",
                length(repeated)),
        repeated)
  }

  # --- referential integrity ------------------------------------------------
  #
  # A relation cell names other things by `prefix:identifier` (a published
  # class) or by bare Class (another term). The build resolves the first with
  # match() and a miss becomes the IRI `<NA>`.

  references <- ontology_references(terms)
  external <- references[stringr::str_detect(references$reference, ":"), ]
  unresolved <- external[!external$reference %in% class_keys, ]
  if (nrow(unresolved) > 0) {
    add("unresolved-reference",
        sprintf("%d reference(s) name a published class that is not in the table",
                nrow(unresolved)),
        sprintf("%s.%s -> %s", unresolved$Class, unresolved$column,
                unresolved$reference))
  }

  internal <- references[!stringr::str_detect(references$reference, ":"), ]
  dangling <- internal[!internal$reference %in% keys, ]
  if (nrow(dangling) > 0) {
    add("dangling-term-reference",
        sprintf("%d reference(s) name a traits.build term that does not exist",
                nrow(dangling)),
        sprintf("%s.%s -> %s", dangling$Class, dangling$column, dangling$reference))
  }

  # --- namespaces -----------------------------------------------------------
  #
  # Every namespace a referenced class lives in needs a Turtle prefix, or
  # traits.build.ttl spells its IRIs out in full and the prefix list in
  # build.R has drifted from the table.

  used <- unique(stringr::str_trim(classes$`skos:inScheme`[class_keys %in% external$reference]))
  used <- used[!is.na(used)]
  undeclared <- setdiff(used, unname(ONTOLOGY_NAMESPACES))
  if (length(undeclared) > 0) {
    add("namespace-undeclared",
        sprintf("%d namespace(s) used by matched classes have no prefix in ONTOLOGY_NAMESPACES",
                length(undeclared)),
        sort(undeclared))
  }

  # --- the serialisations ---------------------------------------------------

  for (file in ONTOLOGY_OUTPUTS) {
    if (!file.exists(file.path(out_dir, file))) {
      add("output-missing", sprintf("%s does not exist; run `make ontology`", file))
    }
  }

  nq <- file.path(out_dir, "traits.build.nq")
  nt <- file.path(out_dir, "traits.build.nt")

  for (file in c(nq, nt)) {
    if (!file.exists(file)) next

    lines <- readr::read_lines(file)
    lines <- lines[nzchar(lines)]

    na_objects <- sum(stringr::str_detect(lines, "<NA>"))
    if (na_objects > 0) {
      add("rdf-na-term",
          sprintf("%s: %d statement(s) contain the unresolved term <NA>",
                  basename(file), na_objects))
    }

    unterminated <- sum(!stringr::str_detect(lines, "\\.[[:space:]]*$"))
    if (unterminated > 0) {
      add("nt-unterminated",
          sprintf("%s: %d of %d statements do not end in '.'",
                  basename(file), unterminated, length(lines)))
    }

    relative_datatype <- sum(stringr::str_detect(lines, "\\^\\^<[a-zA-Z]+:[^/>][^>]*>"))
    if (relative_datatype > 0) {
      add("rdf-datatype-relative-uri",
          sprintf("%s: %d typed literal(s) use a prefixed name as the datatype IRI",
                  basename(file), relative_datatype))
    }

    bracketed <- sum(stringr::str_detect(lines, '"<[a-zA-Z]+://[^"]*>"'))
    if (bracketed > 0) {
      add("rdf-uri-inside-literal",
          sprintf("%s: %d literal(s) contain an IRI in angle brackets",
                  basename(file), bracketed))
    }

    rdfs_type <- sum(stringr::str_detect(lines, "rdf-schema#type>"))
    if (rdfs_type > 0) {
      add("rdf-rdfs-type",
          sprintf("%s: %d statement(s) use rdfs:type (rdf:type is meant)",
                  basename(file), rdfs_type))
    }
  }

  if (file.exists(nq)) {
    parsed <- tryCatch(rdflib::rdf_parse(nq, format = "nquads"), error = function(e) e)
    if (inherits(parsed, "error")) {
      add("rdf-parse-failed", paste("traits.build.nq does not parse:", conditionMessage(parsed)))
    } else {
      # librdf reports a malformed line on stderr and carries on, so a dropped
      # statement never fails the parse. Count what came through against what
      # was written: a repeated statement is stored once, so compare with the
      # distinct lines.
      lines <- readr::read_lines(nq)
      lines <- lines[nzchar(lines)]
      n_parsed <- nrow(rdflib::rdf_query(parsed, "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"))
      n_distinct <- length(unique(lines))
      if (n_parsed != n_distinct) {
        add("rdf-statements-dropped",
            sprintf("traits.build.nq: %d distinct statement(s) written, %d parsed; %d repeated line(s)",
                    n_distinct, n_parsed, length(lines) - n_distinct))
      }
    }
  }

  for (i in seq_along(problems)) {
    problems[[i]]$severity <-
      if (problems[[i]]$id %in% names(ONTOLOGY_KNOWN_GAPS)) "known gap" else "error"
  }

  structure(problems, class = "ontology_validation")
}

print.ontology_validation <- function(x, ...) {

  if (length(x) == 0) {
    cat("validate_ontology(): no problems found.\n")
    return(invisible(x))
  }

  for (problem in x) {
    cat(sprintf("[%s] %s  (%s)\n",
                if (problem$severity == "error") " FAIL " else " gap  ",
                problem$message, problem$id))
    if (length(problem$details) > 0) {
      shown <- utils::head(problem$details, 10)
      cat(paste0("         ", shown, "\n", collapse = ""))
      if (length(problem$details) > 10) cat("         ...\n")
    }
  }

  invisible(x)
}

#' The problems in a report that count as failures
ontology_validation_failures <- function(report) {
  Filter(function(p) p$severity == "error", report)
}
