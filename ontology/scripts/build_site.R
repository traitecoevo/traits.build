#!/usr/bin/env Rscript
# `make ontology-site` -- render ontology/index.qmd and publish it, with the
# serialisations and every release snapshot, into docs/ontology/.
#
# Needs the outputs of `make ontology` (index.qmd renders from
# build_triples.csv) and the Quarto CLI. Rendering takes a minute or two.
#
# This replaces the `pkgdown::build_site()` call that used to end the build
# script: pkgdown copies ontology/output/ into docs/ as an asset, but only when
# the whole package site is rebuilt, and that step had not been run since the
# 2.1.0 release, so docs/ontology/ served data model 1.0.0 while 1.1.0 sat built
# in ontology/output/. Rebuilding the package site remains a separate,
# release-time task.

source("ontology/scripts/setup.R")
ontology_require(c("quarto", "gt", "htmltools", "knitr"))

message("Rendering ontology/index.qmd")
page <- ontology_render_site()

# Every term IRI is https://w3id.org/traits.build#<term>, and w3id.org
# redirects the namespace to this page, so each term needs an element with
# id="<term>" or its IRI resolves to the top of the page. See R/site.R.
missing <- ontology_missing_anchors(page)
if (length(missing) > 0) {
  stop(length(missing), " term(s) have no anchor in ", page, ":\n  ",
       paste(utils::head(missing, 10), collapse = "\n  "),
       if (length(missing) > 10) "\n  ..." else "",
       call. = FALSE)
}
message("All ", length(unique(ontology_read_inputs()$terms$Class)),
        " terms have an anchor in the rendered page")

n <- ontology_publish_docs()
message("Published ", n, " files into docs/ontology/")
