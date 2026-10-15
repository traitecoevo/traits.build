#!/usr/bin/env Rscript
# `make ontology` -- build the data model from ontology/data/.
#
# Writes build_triples.csv and the four RDF serialisations into
# ontology/output/ontology/. Reads ontology/data/ and writes nothing to it.

source("ontology/scripts/setup.R")

n <- ontology_build_data()

message("\nBuilt data model ", ontology_version(), ": ",
        format(n, big.mark = ","), " triples.")
message("Run `make ontology-check` to validate the output.")
