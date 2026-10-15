# What changed in the mapping since the previous release.
#
# The RDF diff of a release is thousands of lines, most of them the published
# classes' own labels and definitions. A reviewer wants the mapping: which
# traits.build term gained, lost or changed a match to which external class.
# `ontology_mapping_changes()` reads that from the N-Quads of the previous
# release's snapshot and of the build, and the build writes it as
# mapping_changes.csv, so every pull request that edits the mapping shows the
# change as a short diff of that file.

# The SKOS mapping relations, in the order the table lists them.
ONTOLOGY_MAPPING_RELATIONS <- c(
  exactMatch = "http://www.w3.org/2004/02/skos/core#exactMatch",
  closeMatch = "http://www.w3.org/2004/02/skos/core#closeMatch",
  broader = "http://www.w3.org/2004/02/skos/core#broader",
  narrower = "http://www.w3.org/2004/02/skos/core#narrower",
  relatedMatch = "http://www.w3.org/2004/02/skos/core#relatedMatch"
)

#' Read the statements of an N-Quads or N-Triples file
#'
#' Line-based, without an RDF parser: the files are written one statement per
#' line, and what is wanted is the statements as written.
#'
#' @return A tibble of `s`, `p`, `o`, with IRIs stripped of their brackets.
ontology_read_statements <- function(path) {
  lines <- readr::read_lines(path)
  parts <- stringr::str_match(lines, "^<([^>]*)> <([^>]*)> (.*?)\\s*\\.?\\s*$")
  tibble::tibble(s = parts[, 2], p = parts[, 3], o = parts[, 4]) %>%
    dplyr::filter(!is.na(s)) %>%
    dplyr::mutate(o = stringr::str_replace(o, "^<(.*)>$", "\\1"))
}

#' The mapping changes between two builds
#'
#' @param from Path to the earlier N-Quads (the previous release's snapshot).
#' @param to Path to the later N-Quads (the build).
#' @param inputs Output of `ontology_read_inputs()`, for term order and the
#'   `prefix:identifier` of each class.
#' @return A tibble with one row per change: `term`, `relation`, `change`
#'   (`added`, `removed`, or `changed` where the same class moved from one
#'   relation to another, with the old one in `previous_relation`), `class`
#'   (its `prefix:identifier`), `class_label` and `class_iri`.
ontology_mapping_changes <- function(from, to, inputs) {

  mappings <- function(statements) {
    statements %>%
      dplyr::filter(stringr::str_starts(s, stringr::fixed(ONTOLOGY_BASE)),
                    p %in% ONTOLOGY_MAPPING_RELATIONS,
                    !stringr::str_starts(o, stringr::fixed(ONTOLOGY_BASE))) %>%
      dplyr::transmute(term = stringr::str_remove(s, stringr::fixed(ONTOLOGY_BASE)),
                       relation = names(ONTOLOGY_MAPPING_RELATIONS)[match(p, ONTOLOGY_MAPPING_RELATIONS)],
                       class_iri = o) %>%
      dplyr::distinct()
  }

  before <- ontology_read_statements(from)
  after <- ontology_read_statements(to)
  old <- mappings(before)
  new <- mappings(after)

  added <- dplyr::anti_join(new, old, by = c("term", "relation", "class_iri"))
  removed <- dplyr::anti_join(old, new, by = c("term", "relation", "class_iri"))

  changed <- dplyr::inner_join(added, removed, by = c("term", "class_iri"),
                               suffix = c("", "_previous"),
                               relationship = "many-to-many") %>%
    dplyr::transmute(term, relation, change = "changed",
                     previous_relation = relation_previous, class_iri)

  changes <- dplyr::bind_rows(
    changed,
    dplyr::anti_join(added, changed, by = c("term", "relation", "class_iri")) %>%
      dplyr::mutate(change = "added"),
    dplyr::anti_join(removed, changed,
                     by = c("term", "relation" = "previous_relation", "class_iri")) %>%
      dplyr::mutate(change = "removed")
  )

  # A class's label from whichever build has it (a removed class may be gone
  # from the current table), and its key from the current table.
  labels <- dplyr::bind_rows(after, before) %>%
    dplyr::filter(p == "http://www.w3.org/2004/02/skos/core#prefLabel") %>%
    dplyr::transmute(class_iri = s, class_label = stringr::str_match(o, '^"(.*)"')[, 2]) %>%
    dplyr::distinct(class_iri, .keep_all = TRUE)
  keys <- inputs$published_classes %>%
    dplyr::transmute(class_iri = Entity, class = paste0(prefix, ":", identifier)) %>%
    dplyr::distinct(class_iri, .keep_all = TRUE)

  term_order <- unique(inputs$terms$Class)

  changes %>%
    dplyr::left_join(keys, by = "class_iri") %>%
    dplyr::left_join(labels, by = "class_iri") %>%
    dplyr::mutate(
      order = match(term, term_order),
      relation_order = match(relation, names(ONTOLOGY_MAPPING_RELATIONS))
    ) %>%
    dplyr::arrange(order, relation_order, change, class) %>%
    dplyr::select(term, relation, change, previous_relation, class, class_label, class_iri)
}

#' The previous release's N-Quads, if it has a snapshot
ontology_baseline_nq <- function(version = ontology_previous_version()) {
  if (is.na(version)) return(NA_character_)
  path <- file.path(ontology_output_dir(), "release", version, "traits.build.nq")
  if (file.exists(path)) path else NA_character_
}
