# traits.build — original-source references, linked to records by `source_id`

*Plan dated 2026-10-20, for [traits.build #296](https://github.com/traitecoevo/traits.build/issues/296). Related: #199 (how to cite data sources), austraits #173 (bibliography function). Status is kept current in the checklist at the end.*

## Context

A dataset's `metadata.yml` `source:` block held a `primary` reference plus any number of `secondary`, `secondary_01`, … entries, and two different things were recorded under `secondary`: a later publication that re-used the same dataset (the intended meaning, attached to the whole dataset), and, for compilation datasets, the study each row was taken from (which should attach to individual records). A third type, `original`, had existed since 2023 (`original_01`, …, written into `methods` as `source_original_dataset_key`), but the schema never documented it, and nothing anywhere checked the `traits` table's `source_id` column against the reference keys it was meant to name.

Measured on austraits.build `develop` (462 datasets) on 2026-10-20: 42 datasets map `source_id`; about 15 hold key-like values that mostly match references; about 10 use `secondary_*` to mean original source (SPRAT_2025_2 with 40 entries, Togashi_2015 with 28, Elliott_2026, Prendergast_2026_2, …); 9 herbarium datasets hold free-text author names; 9 hold key-like values with no reference entries; 6 are broken outright (five NHNSW datasets name a column that does not exist, so every row carries the constant `"authors"`). Downstream, `austraits::extract_data()` trims `sources` to primary and secondary keys only, so every extracted subset silently drops the original-source references.

## Decisions

1. **Shape: one key per type, holding a YAML list.** `secondary:` and `original_source:` are each a list of references (`- key:` items); `primary:` stays a single mapping. No numbering; a reference's identity is its `key`. The old shape is not read: `dataset_process()` stops with an error naming the entry to convert, so nothing can silently drop out of `methods`; `dataset_test()` reports the same failure and carries on with a lenient read so its other checks still run on an unconverted file. The data migration ships in lockstep with the release that carries this change (austraits.build; one dataset in ausfizz).
2. **Label: `original_source`.** The type `original` is renamed. The built `methods` columns keep their names (`source_original_dataset_key`, `source_original_dataset_citation`): they are hard-coded downstream and `test_expect_structure()` pins them to the schema.
3. **Rule, enforced by `dataset_test()`:** every non-NA `source_id` value equals the `key` of the dataset's `primary` reference or of one of its `original_source` references. A value matching a `secondary` key fails with "move these entries to `original_source`"; a value matching nothing fails listing the values; an `original_source` reference no record names fails too. Matching is exact and atomic, because `sources[[key]]` is exact. NA or the primary key both mean "collected in the primary study".
4. **No new table.** The record-level link is `traits$source_id` → `sources[[key]]`; the `methods` columns stay the dataset-level summary; the build output is otherwise unchanged. A bibliography for a subset is the primary keys of the datasets present, their secondary keys, and `unique(na.omit(traits$source_id))`, intersected with `names(sources)`.
5. **Scope.** This repo: engine, `dataset_test()`, schema, fixtures, dataset report, data model 1.2.1, `metadata_add_source_*()`. austraits.build: a matching migration PR (shape conversion of 70 datasets, relabelling, broken mappings, missing references where findable) pinned to the release that carries this change; the free-text and no-entry datasets are deferred to an issue there. Follow-ons: austraits `extract_data()` and print text; ausfizz Tarin_2020; the book; responses.build.

## Invariants a later edit could break

- Every consumer of `metadata$source` goes through `util_flatten_sources()`; do not read the block's names directly.
- `source_id` is part of the `observation_id` hash, so a data fix that changes `source_id` values renumbers that dataset's observations. Fixture `data.csv` values were deliberately left alone; fixtures gained references instead.
- `dataset_test()` must keep flattening with `strict = FALSE`, or an unconverted file reports only the shape and nothing else.
- Golden fixtures Test_2023_2/4/5/6 changed only in `metadata.yml`, `methods.csv` and `sources.bib`; `traits.csv` is byte-identical.

## Checklist

- [x] `util_flatten_sources()`, build and `dataset_test()` changes, `metadata_add_source_*()` appending
- [x] Fixtures converted; golden `methods.csv` / `sources.bib` regenerated
- [x] Schema, NEWS, dataset report
- [x] Data model 1.2.1 (`source_id`, `source_secondary_*`, `source_original_dataset_*` definitions)
- [ ] austraits.build migration PR (pinned to the release carrying this change) and the deferred-datasets issue
- [ ] austraits `extract_data()`; ausfizz Tarin_2020; book wording; responses.build port
