# The traits.build data model

This directory builds the ontology published at <https://w3id.org/traits.build>: the formal description of every table, column, row type and entity in a traits.build database, with its matches to OBOE, SOSA/SSN, the TERN Ontology, Darwin Core and the Ecological Trait-data Standard. It is versioned separately from the R package (see [`NEWS.md`](NEWS.md)) and is maintained by the traits.build maintainers, not by users of the package.

Run `make` at the repository root for the targets. There are four.

| Target | What it does | Needs |
|---|---|---|
| `make ontology` | Builds `output/ontology/` from `data/`: the labelled triple table and the four RDF serialisations (nq, nt, ttl, json). A few seconds. | R with the packages listed in `scripts/setup.R` |
| `make ontology-check` | Validates the build (`R/validate.R`) and runs the tests in `tests/testthat/`, including the golden test that rebuilds into a temporary directory and compares against the committed artefacts byte for byte. | `testthat`, `withr` |
| `make ontology-site` | Renders `index.qmd`, checks every term has an anchor on the page, and publishes `output/ontology/` into `docs/ontology/`, which is what GitHub Pages serves. A minute or two. | the Quarto CLI, `quarto`, `gt`, `htmltools`, `knitr` |
| `make ontology-release` | Runs the two above, then snapshots the version named in `NEWS.md` into `output/ontology/release/<version>/` and publishes it. Refuses to overwrite a non-empty snapshot unless `TRAITS_BUILD_FORCE_RELEASE=1`. | |

CI runs the first three on every pull request that touches `ontology/` (`.github/workflows/ontology.yml`).

## What is where

- `data/traits.build_ontology.csv`: one row per term. `Class` is the term's identifier (its IRI is `https://w3id.org/traits.build#<Class>`); the other columns are predicates. A relation cell holds one or more references separated by `;`: a `prefix:identifier` names a row of the published-classes table, a bare name names another term.
- `data/traits.build_ontology_published_classes.csv`: every external class a term is matched to, with its IRI, label, definition, `prefix`, `identifier` and the namespace it lives in (`skos:inScheme`). A reference that is not in this table becomes the IRI `<NA>` in the RDF, which is why `make ontology-check` refuses it.
- `data/traits.build_resource.csv`: statements about the concept scheme itself, already as triples.
- `R/build.R`: reading the inputs, turning them into triples, serialising. `ONTOLOGY_NAMESPACES` there is the prefix list the Turtle declares; the validator checks that every namespace a matched class lives in has an entry.
- `R/validate.R`: the checks, and `ONTOLOGY_KNOWN_GAPS`, the register of defects that are in the published RDF and are reported as `gap` rather than `FAIL`. Anything not on the register fails. Fixing a gap means deleting its entry in the same change.
- `R/version.R`: the version is the newest `## traits.build data model <X.Y.Z>` heading in `NEWS.md`; the heading below it is the previous release. Nothing else records it.
- `R/table.R`, `index.qmd`: the page.
- `R/site.R`: rendering, the anchor check, and publishing into `docs/ontology/`.
- `output/ontology/`: the build artefacts, committed. They are the golden fixtures: change `R/` and if `make ontology-check` passes the change was output-neutral. To change the output deliberately, rebuild, review `git diff`, and commit the artefacts in their own commit, separate from the code change.
- `output/ontology/release/<version>/`: one snapshot per version, what `https://w3id.org/traits.build/release/<version>/` resolves to. Never edit one that has shipped.

## Editing the data model

1. Edit the CSVs in `data/`. Add any new external class to the published-classes table first, with its definition.
2. `make ontology-check`. Read the report: `FAIL` lines are yours to fix; `gap` lines are the register.
3. Review `git diff output/`. Every changed statement is either the change you meant or a regression.
4. If the change is published output, it needs a version: add a `## traits.build data model <X.Y.Z>` heading at the top of `NEWS.md` with what changed, then `make ontology-release`.
5. Commit `data/`, `output/` and `docs/ontology/` together, open a pull request to `develop`.

The published site updates when `develop` is released to `master` (GitHub Pages serves `docs/` from `master`), not on merge to `develop`.

## Known gaps in the published RDF

The register in `R/validate.R` lists them with reasons. As of data model 1.2.0 there is one: the term `taxon` is defined by two rows (the entity observed, and the characteristic the taxa table describes), so its IRI carries both descriptions. Fixing it renames a published IRI, which needs a maintainer decision on which row moves. The other defects the checks found on arrival (an unterminated `.nt`, relative datatype IRIs and non-ISO dates, IRIs written as literals, `rdfs:type`, published classes with a missing or wrong `skos:inScheme`) were fixed in 1.2.0.
