# Rendering one term as a property/value table on the website.
#
# index.qmd calls `create_term_table()` once per term with that term's labelled
# triples (from build_triples.csv), then `print_table_html()` to emit it. The
# helpers below are what those two need; the pdf and docx variants and the
# unused list printers that used to sit beside them are gone, since the page
# has only ever been rendered to HTML.

#' An HTML link, as a gt-safe html object
make_link <- function(text, url) {
  x <- htmltools::a(href = url, text)
  gt::html(as.character(x))
}

#' Several values on separate lines within one table cell
print_list2 <- function(vals) {
  vals <- vals[!is.na(vals)]
  if (length(vals) > 0) paste0(vals, collapse = "<br>\n") else character()
}

add_row <- function(data, name, description) {
  if (!("html" %in% class(name))) {
    name <- gt::html(name)
  }
  if (!("html" %in% class(description))) {
    description <- gt::html(description)
  }

  dplyr::bind_rows(
    data,
    tibble::tibble(name = list(name), description = list(description))
  )
}

#' Emit a property/value table as HTML
#'
#' @param x A tibble of `name` and `description` columns holding html objects.
#' @param id The table's HTML id. gt generates a random id when none is given,
#'   which made every render of the page differ from the last in 142 places and
#'   hid real changes in the diff; the term being rendered is the natural id.
#'   gt compiles the id into a Sass selector, so anything outside
#'   `[A-Za-z0-9_-]` (the `.` in `traits.build-traits`) becomes `-`.
print_table_html <- function(x, id) {
  id <- gsub("[^A-Za-z0-9_-]", "-", id)
  gt::gt(x, id = id) %>%
    gt::cols_align(align = "left") %>%
    gt::cols_width(
      name ~ gt::px(150),
      description ~ gt::px(600)
    ) %>%
    gt::cols_label(
      name = "**Property**",
      description = "**Value**"
    ) %>%
    # gt's own css is dropped from every table, or the page carries it 142
    # times over; see https://github.com/rstudio/gt/issues/816
    gt::as_raw_html(inline_css = FALSE) %>%
    remove_css() %>%
    print()
}

remove_css <- function(x) {
  x <- gsub("<style>.*</style>", "", x)
  htmltools::HTML(x)
}

#' A term's property/value table
#'
#' @param thisterm The term's identifier (its `Class` in the terms table).
#' @param triples The labelled triples for that term, from build_triples.csv.
#' @return A tibble of `name` and `description` html objects, one row per
#'   property, in the order the page shows them.
create_term_table <- function(thisterm, triples) {

  trait_i <-
    triples %>%
    dplyr::filter(subject_tmp == thisterm) %>%
    dplyr::mutate(property_link = NA, value_link = NA)

  for (i in seq_len(nrow(trait_i))) {
    trait_i$property_link[i] <- make_link(trait_i$Predicate_labels[i], trait_i$predicate_tmp[i])
    trait_i$value_link[i] <- ifelse(stringr::str_detect(trait_i$Object[i], "http"),
                                    make_link(trait_i$Object_labels[i], trait_i$object_tmp[i]),
                                    trait_i$Object_labels[i])
  }

  output <- tibble::tibble(name = list(), description = list())

  URI <- make_link(paste0("w3id.org/traits.build#", thisterm), paste0(ONTOLOGY_BASE, thisterm))

  output <- add_row(output, "URI", URI)

  # A property shown as one row: a single value.
  single <- function(output, predicate_label) {
    rows <- trait_i %>% dplyr::filter(Predicate_labels == predicate_label)
    if (purrr::is_empty(rows$value_link)) {
      output
    } else {
      add_row(output, rows$property_link, rows$value_link)
    }
  }

  # A property shown as one row: several values on separate lines.
  multiple <- function(output, predicate_label, predicate_iri,
                       shown_as = predicate_label) {
    rows <- trait_i %>% dplyr::filter(Predicate_labels == predicate_label)
    if (purrr::is_empty(rows$value_link)) {
      output
    } else {
      add_row(output, make_link(shown_as, predicate_iri),
              print_list2(rows$value_link))
    }
  }

  # The order the page has always shown them in. "is property of" used to be
  # looked up as "is property for", which matched nothing, so those rows never
  # rendered; they do now.
  output <- output %>%
    single("preferred label") %>%
    single("alternative label") %>%
    single("description") %>%
    single("comment") %>%
    single("computational entity") %>%
    multiple("organizedInClass", "http://rs.tdwg.org/dwc/terms/attributes/organizedInClass") %>%
    multiple("has exact match", "http://www.w3.org/2004/02/skos/core#exactMatch") %>%
    multiple("has close match", "http://www.w3.org/2004/02/skos/core#closeMatch") %>%
    multiple("has related match", "http://www.w3.org/2004/02/skos/core#relatedMatch") %>%
    multiple("has broader", "http://www.w3.org/2004/02/skos/core#broader") %>%
    multiple("has unique identifier", "http://semanticscience.org/resource/SIO_000673") %>%
    multiple("is unique identifier for", "http://semanticscience.org/resource/SIO_000674") %>%
    multiple("has identifier", "http://semanticscience.org/resource/SIO_000671") %>%
    multiple("is identifier for", "http://semanticscience.org/resource/SIO_000672") %>%
    multiple("of entity", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#ofEntity") %>%
    multiple("has value", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasValue") %>%
    multiple("has measurement", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasMeasurement") %>%
    multiple("measurement for", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#measurementFor") %>%
    multiple("has context", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#hasContext") %>%
    multiple("characteristic for", "http://ecoinformatics.org/oboe/oboe.1.2/oboe-core.owl#characteristicFor") %>%
    multiple("is specialisation of", "http://semanticscience.org/resource/SIO_001096") %>%
    multiple("has annotation", "http://semanticscience.org/resource/SIO_000255") %>%
    multiple("is annotation of", "http://semanticscience.org/resource/SIO_000254") %>%
    multiple("refers to", "http://semanticscience.org/resource/SIO_000628") %>%
    multiple("Cites", "http://vocab.fairdatacollective.org/gdmt/Cites", shown_as = "cites") %>%
    multiple("has basis", "http://semanticscience.org/resource/SIO_000641") %>%
    multiple("is base for", "http://semanticscience.org/resource/SIO_000642") %>%
    multiple("has property", "http://semanticscience.org/resource/SIO_000223") %>%
    multiple("is property of", "http://semanticscience.org/resource/SIO_000224") %>%
    single("created")

  # Every term is in the scheme; the row links to it rather than repeating the
  # IRI as text.
  scheme_tmp <- trait_i %>% dplyr::filter(Predicate_labels == "is in scheme")
  output <- add_row(output, scheme_tmp$property_link,
                    make_link("w3id.org/traits.build", ONTOLOGY_SCHEME))

  output %>%
    single("datatype")
}
