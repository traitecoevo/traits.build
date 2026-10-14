# traits.build data model: change log

The data model (the ontology published at <https://w3id.org/traits.build>) is versioned separately from the traits.build R package. The newest heading below is the version `make ontology` builds and `make ontology-release` snapshots into `ontology/output/ontology/release/<version>/`; the heading under it is what the site shows as the previous version. To cut a new version, add a heading. The package's own `NEWS.md` records the same changes from the package's side.

## traits.build data model 1.2.0

Prepared 2026-10-12 (traits.build #275, #276).

- Every OBOE relation now has a SOSA/SSN counterpart, following the published SOSA-to-OBOE alignment (`oboe:Measurement` is `sosa:Observation`, `oboe:Observation` is `sosa:ObservationCollection`, `oboe:Characteristic` is `sosa:ObservableProperty`, `oboe:Protocol` is `sosa:Procedure`, `oboe:Entity` subsumes `sosa:FeatureOfInterest`), and a TERN Ontology 0.9.0 counterpart where one exists (`tern:Observation`, `tern:ObservationCollection`, `tern:FeatureOfInterest`, `tern:Site`, `tern:Attribute`, `tern:Procedure`, `tern:Method`, `tern:Taxon`, `tern:RDFDataset`, and the tern-cv feature types plant individual and plant population).
- Trait-measurement terms are matched to the Darwin Core terms of 2026-05-26: `MeasurementOrFact` terms (used by Darwin Core Archives) and the new `Assertion` terms (used by the Darwin Core Data Package), with `dwciri:` IRI twins where the value is an identifier.
- `value_type` and `basis_of_value` gain the only published counterparts they have, the ETS terms `statisticalMethod`, `aggregateMeasure`, `measurementValue_min`, `measurementValue_max` and `basisOfRecordDescription`.
- Exact Darwin Core matches that had been recorded only as labels or as a broader OBOE identifier are now declared exact: `original_name` to `verbatimIdentification`, `trait_name` to `measurementType`, `value` to `measurementValue`, `unit` to `measurementUnit`, `measurement_remarks` to `measurementRemarks`, `individual_id` to `organismID`, `location_id` to `locationID`, `dataset_id` to `datasetID`, `collection_date` to `eventDate`.
- `establishment_means` is demoted from an exact to a close match: Darwin Core applies it to an organism at a place and time, and the `taxa` table records it once per taxon.
- `entity_type` and `taxon_distribution`, which had no mapping, now have one; the three terms with no counterpart in any standard (`schema`, `build_info`, the database itself) say so in their comment.
- 77 external classes added to the published-classes table with their definitions; the Turtle serialisation declares the `sosa`, `tern`, `terncv`, `dwciri` and `qudt` prefixes. 2,054 to 2,468 statements.
- `identifier_value` now links to both `identifier_type` and `institution_code`. The two were separated by a comma rather than `;`, which made one IRI that did not parse, so the statement had been dropped since 1.1.0. `dwc:Taxon` was listed twice in the published-classes table; its statements are now written once.
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
