# Build and publish the traits.build data model (the ontology at
# https://w3id.org/traits.build).
#
# The R package itself is built the usual way (devtools::test(),
# devtools::check()); these targets cover only ontology/, which R CMD check
# ignores. Each target runs a script in ontology/scripts/, which calls
# functions in ontology/R/.
#
# Targets always run: the data build takes a few seconds, so tracking which
# outputs are stale would cost more in surprise than it saves in time. The one
# slow step is `ontology-site`, which renders the page with Quarto.

R := Rscript

.PHONY: help ontology ontology-check ontology-site ontology-release

help:  ## Show this help
	@echo "traits.build -- make targets:"
	@echo
	@grep -E '^[a-z-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "  \033[1m%-18s\033[0m %s\n", $$1, $$2}'
	@echo
	@echo "Editing the data model:  edit ontology/data/*.csv -> make ontology-check"
	@echo "Releasing a version:     add a heading to ontology/NEWS.md -> make ontology-release"

ontology:  ## Build the data model from ontology/data/: triples + RDF serialisations
	$(R) ontology/scripts/build_data.R

ontology-check: ontology  ## Validate the built data model and run its tests
	$(R) ontology/scripts/check.R

ontology-site: ontology  ## Render ontology/index.qmd and publish into docs/ontology/ (needs Quarto)
	$(R) ontology/scripts/build_site.R

ontology-release: ontology-check ontology-site  ## Snapshot the version in ontology/NEWS.md into release/<version>/
	$(R) ontology/scripts/release.R
