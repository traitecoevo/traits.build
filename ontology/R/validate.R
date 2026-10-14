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
# An entry is a defect in the published RDF whose fix needs a decision, not
# just a reviewed change: each one changes what consumers parse, so it ships in
# a data-model version. The mechanical defects found when these checks arrived
# (2026-10) were fixed in 1.2.0 and their entries deleted.

ONTOLOGY_KNOWN_GAPS <- c(
  `input-duplicate-key` = paste(
    "`taxon` is the Class of two rows of traits.build_ontology.csv: the entity",
    "observed (broader oboe:Entity, 'A circumscribed set of organisms') and",
    "the characteristic the taxa table describes (broader",
    "oboe:Characteristic, exact dwc:Taxon, has property taxon_name ...).",
    "Both become the one IRI https://w3id.org/traits.build#taxon, which is",
    "therefore both an Entity and a Characteristic with two descriptions.",
    "Fixing it renames a published IRI; needs a maintainer decision on which",
    "row moves.")
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
  # A published class's IRI must sit in the namespace it claims (the table had
  # oboe-characteristics#Identifier filed under oboe-core, and XML Schema and
  # OWL without their `#`), and every namespace needs a Turtle prefix, or
  # traits.build.ttl spells its IRIs out in full and the prefix list in build.R
  # has drifted from the table.

  scheme <- stringr::str_trim(classes$`skos:inScheme`)
  outside <- !is.na(scheme) & !is.na(classes$Entity) &
    !stringr::str_starts(classes$Entity, stringr::fixed(scheme))
  if (any(outside)) {
    add("published-class-outside-scheme",
        sprintf("%d published class(es) have an IRI outside their skos:inScheme", sum(outside)),
        sprintf("%s (inScheme %s)", classes$Entity[outside], scheme[outside]))
  }

  used <- unique(scheme[!is.na(scheme)])
  undeclared <- setdiff(used, unname(ONTOLOGY_NAMESPACES))
  if (length(undeclared) > 0) {
    add("namespace-undeclared",
        sprintf("%d namespace(s) used by published classes have no prefix in ONTOLOGY_NAMESPACES",
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

  # Each line-based file is parsed in its own format: an N-Triples file that
  # only parses as N-Quads is not N-Triples.
  for (file in c(nquads = nq, ntriples = nt)) {
    if (!file.exists(file)) next
    format <- names(which(c(nquads = nq, ntriples = nt) == file))

    parsed <- tryCatch(rdflib::rdf_parse(file, format = format), error = function(e) e)
    if (inherits(parsed, "error")) {
      add("rdf-parse-failed", sprintf("%s does not parse as %s: %s",
                                      basename(file), format, conditionMessage(parsed)))
      next
    }

    # librdf reports a malformed line on stderr and carries on, so a dropped
    # statement never fails the parse. Count what came through against what
    # was written: a repeated statement is stored once, so compare with the
    # distinct lines.
    lines <- readr::read_lines(file)
    lines <- lines[nzchar(lines)]
    n_parsed <- nrow(rdflib::rdf_query(parsed, "SELECT ?s ?p ?o WHERE { ?s ?p ?o }"))
    n_distinct <- length(unique(lines))
    if (n_parsed != n_distinct) {
      add("rdf-statements-dropped",
          sprintf("%s: %d distinct statement(s) written, %d parsed as %s; %d repeated line(s)",
                  basename(file), n_distinct, n_parsed, format, length(lines) - n_distinct))
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
