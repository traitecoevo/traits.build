# traits.build data model: change log

The data model (the ontology published at <https://w3id.org/traits.build>) is versioned separately from the traits.build R package. The newest heading below is the version `make ontology` builds and `make ontology-release` snapshots into `ontology/output/ontology/release/<version>/`; the heading under it is what the site shows as the previous version. To cut a new version, add a heading. The package's own `NEWS.md` records the same changes from the package's side.

## traits.build data model 1.2.0

Prepared 2026-10-12 (traits.build #275, #276).

- Every OBOE relation now has a SOSA/SSN counterpart, following the published SOSA-to-OBOE alignment (`oboe:Measurement` is `sosa:Observation`, `oboe:Observation` is `sosa:ObservationCollection`, `oboe:Characteristic` is `sosa:ObservableProperty`, `oboe:Protocol` is `sosa:Procedure`, `oboe:Entity` subsumes `sosa:FeatureOfInterest`), and a TERN Ontology 0.9.0 counterpart where one exists (`tern:Observation`, `tern:ObservationCollection`, `tern:FeatureOfInterest`, `tern:Site`, `tern:Attribute`, `tern:Procedure`, `tern:Method`, `tern:Taxon`, `tern:RDFDataset`, and the tern-cv feature types plant individual and plant population).
- Trait-measurement terms are matched to the Darwin Core terms of 2026-05-26: `MeasurementOrFact` terms (used by Darwin Core Archives) and the new `Assertion` terms (used by the Darwin Core Data Package).
- `value_type` and `basis_of_value` gain the only published counterparts they have, the ETS terms `statisticalMethod`, `aggregateMeasure`, `measurementValue_min`, `measurementValue_max` and `basisOfRecordDescription`.
- Exact Darwin Core matches that had been recorded only as labels or as a broader OBOE identifier are now declared exact: `original_name` to `verbatimIdentification`, `trait_name` to `measurementType`, `value` to `measurementValue`, `unit` to `measurementUnit`, `measurement_remarks` to `measurementRemarks`, `location_id` to `locationID`, `dataset_id` to `datasetID`.
- Relations revised after review on #276. Each change follows the definitions of the two terms:
  - `individual_id` is a close, not exact, match to `dwc:organismID`, because a Darwin Core organism may be a group.
  - `collection_date` is a close match to `dwc:eventDate` and `ets:eventDate`, because it holds the publication date when that is all that is known.
  - `taxon_name` is a related match to `dwc:scientificName` and `ets:scientificName`, which carry authorship. `scientific_name` remains the exact match.
  - `original_name` is a close match to `ets:verbatimScientificName`.
  - `method_context` is a related match to `sosa:Procedure` and `tern:Procedure`, because the procedure itself is recorded in `methods`.
  - No matches to the `dwciri:` namespace. It is little used, and matches are kept to well-used vocabularies.
  - `location_property` is narrower than `dwc:measurementType`.
  - `elevation` is an exact match to `ets:elevation`.
  - `unit` is a close match to `tern:unit`, which takes a QUDT IRI where `unit` holds text.
- Where the strength of a match is not obvious from the two definitions, a new `skos:editorialNote` says why, and the page shows it on the term's entry: every match revised or questioned on #276, and each term matched more closely to SOSA than to TERN. The conventions they follow are in `ontology/README.md`. `establishment_means`'s reason moves there from its comment.
- `establishment_means` is demoted from an exact to a close match: Darwin Core applies it to an organism at a place and time, and the `taxa` table records it once per taxon.
- `entity_type` and `taxon_distribution`, which had no mapping, now have one; the three terms with no counterpart in any standard (`schema`, `build_info`, the database itself) say so in their comment.
- 74 external classes added to the published-classes table with their definitions; the Turtle serialisation declares the `sosa`, `tern`, `terncv` and `qudt` prefixes. 2,054 to 2,467 statements.
- `identifier_value` now links to both `identifier_type` and `institution_code`. The two were separated by a comma rather than `;`, which made one IRI that did not parse, so the statement had been dropped since 1.1.0. `dwc:Taxon` was listed twice in the published-classes table; its statements are now written once.
- Fixes to the RDF as published since 0.1.0, found by the build checks added in this version:
  - `traits.build.nt` now ends each statement with `.`; before, it parsed as N-Triples to zero statements.
  - `dcterms:created` dates are ISO (`2024-02-01`) and typed with the full `http://www.w3.org/2001/XMLSchema#date` IRI. Before, they were written `"1-Feb-24"^^<xsd:date>`: a relative IRI and an invalid lexical form.
  - The concept scheme's licence, publisher, top concept and `topConceptOf` are IRIs rather than strings holding an IRI. The top concept is `#traits.build-database` (it had lost the `#`). The scheme is typed with `rdf:type`, where it had used `rdfs:type`, which does not exist.
  - Every published class has a `skos:inScheme`, and the namespace is the one its IRI lives in. 12 classes had none, XML Schema and OWL were missing their `#`, and `Identifier` (defined in `oboe-characteristics`) was filed under `oboe-core`. The Turtle declares the `xsd` and `oboe-characteristics` prefixes.
- `taxon` was defined by two rows, so its IRI was published as both an `oboe:Entity` and an `oboe:Characteristic` with two definitions. The rows are merged into one, following `individual` and `location`. It is broader than `oboe:Entity` and `sosa:FeatureOfInterest`, keeps its matches to `dwc:Taxon` and `tern:Taxon` and its identifier and property links, and takes its definition from `dwc:Taxon`, its exact match. The IRI is unchanged.
- The page gives every term an anchor by its bare name, so `https://w3id.org/traits.build#<term>` lands on the term, and shows the "is property of" relations, which it had never displayed.

## traits.build data model 1.1.0

Released 2025-06-19 with traits.build 2.1.0 (#210).

- Adds the terms of the `identifiers` table (`identifier_type`, `institution_code`, `identifier_value`), which links a trait observation to a specimen, collection or individual identifier held elsewhere.
- Never copied into `docs/ontology/`, so <https://w3id.org/traits.build> continued to serve 1.0.0 until 1.2.0 was prepared.

## traits.build data model 1.0.0

Released 2024-12-20 with traits.build 2.0.0 (#190).

- Term definitions and the web page revised for the traits.build 2.0.0 release.

## traits.build data model 0.1.0

Released 2024-02 with the traits.build preprint (Wenk et al. 2024, bioRxiv 2024.02.11.579848; *Ecological Informatics* 83:102773).

- First release: 13 database tables, their columns, the entity, observation and measurement terms, with OBOE, Darwin Core and ETS matches.
