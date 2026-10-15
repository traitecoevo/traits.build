# The traits.build data model

This directory builds the ontology published at <https://w3id.org/traits.build>: the formal description of every table, column, row type and entity in a traits.build database, with its matches to OBOE, SOSA/SSN, the TERN Ontology, Darwin Core and the Ecological Trait-data Standard. It is versioned separately from the R package (see [`NEWS.md`](NEWS.md)) and is maintained by the traits.build maintainers, not by users of the package.

Run `make` at the repository root for the targets. There are four.

| Target | What it does | Needs |
|---|---|---|
| `make ontology` | Builds `output/ontology/` from `data/`: the labelled triple table, the mapping changes since the previous release, and the four RDF serialisations (nq, nt, ttl, json). A few seconds. | R with the packages listed in `scripts/setup.R` |
| `make ontology-check` | Validates the build (`R/validate.R`) and runs the tests in `tests/testthat/`, including the golden test that rebuilds into a temporary directory and compares against the committed artefacts byte for byte. | `testthat`, `withr` |
| `make ontology-site` | Renders `index.qmd`, checks every term has an anchor on the page, and publishes `output/ontology/` into `docs/ontology/`, which is what GitHub Pages serves. A minute or two. | the Quarto CLI, `quarto`, `gt`, `htmltools`, `knitr` |
| `make ontology-release` | Runs the two above, then snapshots the version named in `NEWS.md` into `output/ontology/release/<version>/` and publishes it. Refuses to overwrite a non-empty snapshot unless `TRAITS_BUILD_FORCE_RELEASE=1`. | |

CI runs the first three on every pull request that touches `ontology/` (`.github/workflows/ontology.yml`).

## What is where

- `data/traits.build_ontology.csv`: one row per term. `Class` is the term's identifier (its IRI is `https://w3id.org/traits.build#<Class>`); the other columns are predicates. A relation cell holds one or more references separated by `;`: a `prefix:identifier` names a row of the published-classes table, a bare name names another term. A text cell (`prefLabel`, `altLabel`, `label`, `dcterms:description`, `rdfs:comment`, `skos:editorialNote`) is one statement, whatever punctuation it holds.
- `data/traits.build_ontology_published_classes.csv`: every external class a term is matched to, with its IRI, label, definition, `prefix`, `identifier` and the namespace it lives in (`skos:inScheme`). A reference that is not in this table becomes the IRI `<NA>` in the RDF, which is why `make ontology-check` refuses it.
- `data/traits.build_resource.csv`: statements about the concept scheme itself, already as triples.
- `R/build.R`: reading the inputs, turning them into triples, serialising. `ONTOLOGY_NAMESPACES` there is the prefix list the Turtle declares; the validator checks that every namespace a matched class lives in has an entry.
- `R/validate.R`: the checks, and `ONTOLOGY_KNOWN_GAPS`, the register of defects that are in the published RDF and are reported as `gap` rather than `FAIL`. Anything not on the register fails. Fixing a gap means deleting its entry in the same change.
- `R/version.R`: the version is the newest `## traits.build data model <X.Y.Z>` heading in `NEWS.md`; the heading below it is the previous release. Nothing else records it.
- `R/changes.R`: `mapping_changes.csv`, one row per SKOS match a term gained, lost or moved between relations since the previous release's snapshot. The build writes it, so a pull request that edits the mapping shows its changes as a short diff of that file rather than only as thousands of lines of RDF.
- `R/table.R`, `index.qmd`: the page.
- `R/site.R`: rendering, the anchor check, and publishing into `docs/ontology/`.
- `output/ontology/`: the build artefacts, committed. They are the golden fixtures: change `R/` and if `make ontology-check` passes the change was output-neutral. To change the output deliberately, rebuild, review `git diff`, and commit the artefacts in their own commit, separate from the code change.
- `output/ontology/release/<version>/`: one snapshot per version, what `https://w3id.org/traits.build/release/<version>/` resolves to. Never edit one that has shipped.

## Mapping conventions

How a term is matched to another vocabulary. These are the rules data model 1.2.0 settled on, mostly in review on #276. Follow them, or change them here first, in the same pull request as the mapping that departs from them.

- **The four SKOS columns, for every vocabulary.** Matches go in `exactMatch`, `closeMatch`, `relatedMatch` and `broader`, never in a column per vocabulary: these are what the build turns into SKOS statements, and the view of one vocabulary is a filter on the prefix. `broader` names an external class more general than the term.
- **Exact** only when the two definitions agree and neither class requires more than the other. A class from a profile that adds required properties is a close match even where its definition is word for word the base class's. TERN's SHACL shapes require every `tern:Observation` to have a dataset, procedure, phenomenon time, result date-time, observed property, result and feature of interest, and every `tern:FeatureOfInterest` a `tern:featureType`. A traits.build `measurement` or `entity` requires none of these, so it is exact to the SOSA class and close to the TERN one. That says traits.build is as unconstrained as SOSA, not that it matches SOSA better than TERN does.
- **Close** when the meaning is the same but the scope differs in a way someone combining the data would notice (`collection_date` can hold a publication date; a Darwin Core organism can be a group), or when the two classes play different roles in their models (`tern:Taxon` is a `tern:Value`, the result of an identification, where `taxon` is the thing observed).
- **Related** when the two concern the same thing but one cannot stand in for the other (`taxon_name` carries no authorship and `dwc:scientificName` does; `method_context` is one varying component of a protocol, and the protocol itself is in `methods`).
- **Vocabularies in scope**, with the versions the 1.2.0 matches were made against: OBOE 1.2; SOSA/SSN, following the W3C [SOSA-to-OBOE alignment](https://www.w3.org/ns/sosa/oboe); the TERN Ontology 0.9.0 and the tern-cv feature types; Darwin Core of 2026-05-26 (`MeasurementOrFact` and `Assertion` terms); ETS 0.10; QUDT. Definitions in the published-classes table are copied verbatim from those versions. Matches are kept to well-used vocabularies, so there are none to the `dwciri:` namespace. A new vocabulary, or a new version of one, is added to this list first.
- **No counterpart.** Where no vocabulary in scope has a counterpart, the term's `rdfs:comment` says so (`schema`, `build_info`, the database itself).
- **The reason goes in `skos:editorialNote`.** `rdfs:comment` says what a term is; `skos:editorialNote` says why its matches have the strength they have. Write one wherever the choice is not obvious from the two definitions: above all for a match a reviewer questioned, and for a term whose matches to two closely related vocabularies (SOSA and TERN, say) differ in strength. A note names the review it came from. Notes are published with the term, in the RDF and on the page, so the reason stays beside the mapping and shows in the same diff as any change to it. Change a match and its note together.

## Editing the data model

1. Edit the CSVs in `data/`. Add any new external class to the published-classes table first, with its definition. Follow the [mapping conventions](#mapping-conventions), and give a match that needs explaining a `skos:editorialNote`.
2. `make ontology-check`. Read the report: `FAIL` lines are yours to fix; `gap` lines are the register.
3. Review `output/ontology/mapping_changes.csv` for what the mapping now says against the last release, then `git diff output/`. Every changed statement is either the change you meant or a regression.
4. If the change is published output, it needs a version: add a `## traits.build data model <X.Y.Z>` heading at the top of `NEWS.md` with what changed, then `make ontology-release`.
5. Commit `data/`, `output/` and `docs/ontology/` together, open a pull request to `develop`.

The published site updates when `develop` is released to `master` (GitHub Pages serves `docs/` from `master`), not on merge to `develop`.

## Known gaps in the published RDF

The register in `R/validate.R` is empty as of data model 1.2.0. The defects the checks found on arrival (an unterminated `.nt`, relative datatype IRIs and non-ISO dates, IRIs written as literals, `rdfs:type`, published classes with a missing or wrong `skos:inScheme`, and the term `taxon` defined by two rows) were fixed in 1.2.0. A new entry needs a reason a fix has to wait for a decision; anything else gets fixed.
