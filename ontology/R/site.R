# The website: ontology/index.qmd rendered to ontology/output/ontology/index.html
# and published from docs/ontology/.
#
# https://w3id.org/traits.build redirects to
# https://traitecoevo.github.io/traits.build/ontology/index.html, which GitHub
# Pages serves from docs/ on master. pkgdown copies ontology/output/ into docs/
# as a site asset (`_pkgdown.yml`), but only when the whole package site is
# rebuilt, which is how the published data model sat at 1.0.0 for sixteen
# months after 1.1.0 was built. `make ontology-site` now publishes into
# docs/ontology/ itself, so the two can no longer drift.

#' Render index.qmd into the output directory
#'
#' Quarto writes index.html beside index.qmd; it is moved into `out_dir` so the
#' page sits with the serialisations it links to.
#'
#' @param out_dir Where the build outputs are, and where index.html goes.
#' @return The path to the rendered page, invisibly.
ontology_render_site <- function(out_dir = ontology_output_dir()) {

  missing <- ONTOLOGY_OUTPUTS[!file.exists(file.path(out_dir, ONTOLOGY_OUTPUTS))]
  if (length(missing) > 0) {
    stop("Missing build outputs: ", paste(missing, collapse = ", "),
         "\nRun `make ontology` first.", call. = FALSE)
  }

  quarto::quarto_render(ontology_path("index.qmd"), output_format = "html")

  rendered <- ontology_path("index.html")
  target <- file.path(out_dir, "index.html")
  if (!file.copy(rendered, target, overwrite = TRUE)) {
    stop("could not copy ", rendered, " to ", target, call. = FALSE)
  }
  unlink(rendered)

  invisible(target)
}

#' Terms the rendered page carries no anchor for
#'
#' Every term's IRI is `https://w3id.org/traits.build#<term>`, and w3id.org
#' redirects the whole namespace to index.html, so the browser looks for an
#' element with `id="<term>"` on that page. An element whose id is the full IRI
#' does not count: a fragment is matched against ids verbatim. A term with no
#' bare-id anchor is still published but is no longer findable -- its IRI
#' scrolls to the top of the page.
#'
#' @param html Path to the rendered page.
#' @param terms The term identifiers to look for; defaults to every `Class`.
#' @return The terms with no anchor, in input order.
ontology_missing_anchors <- function(html = file.path(ontology_output_dir(), "index.html"),
                                     terms = unique(ontology_read_inputs()$terms$Class)) {

  if (!file.exists(html)) {
    stop(html, " does not exist; run `make ontology-site` first.", call. = FALSE)
  }

  rendered <- readr::read_file(html)

  found <- vapply(paste0('id="', terms, '"'),
                  function(anchor) grepl(anchor, rendered, fixed = TRUE),
                  logical(1), USE.NAMES = FALSE)

  terms[!found]
}

#' Publish the output directory into docs/ontology/
#'
#' Copies the build outputs, the rendered page and every release snapshot, so
#' docs/ontology/ is exactly ontology/output/ontology/. Nothing else in docs/
#' is touched: the rest of that directory is the pkgdown site.
#'
#' @param out_dir The build output directory.
#' @param docs_dir The published directory.
#' @return The number of files copied, invisibly.
ontology_publish_docs <- function(out_dir = ontology_output_dir(),
                                  docs_dir = repo_path("docs", "ontology")) {

  files <- list.files(out_dir, recursive = TRUE)
  if (length(files) == 0) {
    stop("nothing to publish from ", out_dir, call. = FALSE)
  }

  for (file in files) {
    target <- file.path(docs_dir, file)
    dir.create(dirname(target), showWarnings = FALSE, recursive = TRUE)
    if (!file.copy(file.path(out_dir, file), target, overwrite = TRUE)) {
      stop("could not copy ", file, " into ", docs_dir, call. = FALSE)
    }
  }

  # A file in docs/ontology/ that the build no longer produces would otherwise
  # linger there forever, published.
  stale <- setdiff(list.files(docs_dir, recursive = TRUE), files)
  if (length(stale) > 0) {
    unlink(file.path(docs_dir, stale))
  }

  invisible(length(files))
}
