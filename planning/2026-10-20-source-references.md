# traits.build — references typed `dataset` / `source` / `secondary`, linked to records by `source_id`

*Plan dated 2026-10-20, reworked 2026-11-10 after review, for [traits.build #296](https://github.com/traitecoevo/traits.build/issues/296). Related: #199 (how to cite data sources), austraits #173 (bibliography function). Status is kept current in the checklist at the end.*

## Context

A dataset's `metadata.yml` `source:` block held a `primary` reference plus any number of `secondary`, `secondary_01`, … entries, and two different things were recorded under `secondary`: a later publication that re-used the same dataset (the intended meaning, attached to the whole dataset), and, for compilation datasets, the study each row was taken from (which should attach to individual records). A third type, `original`, had existed since 2023 (`original_01`, …, written into `methods` as `source_original_dataset_key`), but the schema never documented it, and nothing anywhere checked the `traits` table's `source_id` column against the reference keys it was meant to name.

Measured on austraits.build `develop` (462 datasets) on 2026-10-20: 42 datasets map `source_id`; about 15 hold key-like values that mostly match references; about 10 use `secondary_*` to mean original source (SPRAT_2025_2 with 40 entries, Togashi_2015 with 28, Elliott_2026, Prendergast_2026_2, …); 9 herbarium datasets hold free-text author names; 9 hold key-like values with no reference entries; 6 are broken outright (five NHNSW datasets name a column that does not exist, so every row carries the constant `"authors"`). Downstream, `austraits::extract_data()` trims `sources` to primary and secondary keys only, so every extracted subset silently drops the original-source references.

The first version of #298 (lists for `secondary` and `original_source`, `source_id` checked against them) was reviewed on 2026-11-10 against a full AusTraits build. The decisions below are the reworked design.

## Decisions

1. **Three types, `dataset`, `source` and `secondary`, in the `source:` block.** `dataset` (was `primary`) is a single mapping; `source` (was `original_source` / `original_NN`) and `secondary` are lists of `- key:` items, unnumbered. The block keeps its name, so it reads `source: source:`; renaming the top-level block was judged not worth touching every file's first line. Old shapes are not read: the build stops naming the entry, and `metadata_convert_sources()` converts a database, so databases not built by this team can migrate with the package alone.
2. **The `dataset` reference is keyed by the `dataset_id`, and every record's `source_id` defaults to the `dataset_id`.** So `sources[[source_id]]` always resolves and needs no NA handling. Mapping `source_id` stays optional. On AusTraits 9 of 470 dataset keys differed from the `dataset_id`; most came from `metadata_add_source_bibtex()` title-casing and truncating the key (`Gallagher_2011_1` → `Gallagher_2011`, `McQuillan_2020` → `Mcquillan_2020`), which is fixed, and three were key collisions (`Bryant_2021_2`/`_3` and `Stephens_2024_2` reused another dataset's key for a different paper, so `sources` silently held only one). A paper shared by two datasets (`Gallagher_2011_1`/`_3`) is a sign the datasets should be combined, not a reason to relax the rule.
3. **`source_id` is filled after `observation_id`.** It is part of that hash; filling it first would renumber the observations of a compilation that leaves some records blank.
4. **`methods` keeps one row per dataset × trait × method; no per-source rows.** Splitting rows by `source_id` was measured and rejected: 4,507 → 72,840 rows, driven by herbarium datasets with thousands of free-text `source_id` values, each repeating the methods text. Columns are named to match `traits`: `dataset_id_type`, `dataset_id_citation`, `secondary_id`, `secondary_citation`. `source_primary_key` (always the `dataset_id`) and `source_original_dataset_*` are dropped. A record's source citation is `sources[[source_id]]`; `austraits::join_sources()` joins it as `source_id_citation`. `dataset_id_type` is kept at the request of a database supporter.
5. **No `secondary_id` column in `traits`.** Secondary references are dataset-level; one record cannot name several.
6. **Long reference lists may live in a file:** `source: sources.yml` (or `.bib`) beside `metadata.yml`, read by `read_metadata()` and written back by `write_metadata()`, as `locations: locations.csv`.
7. **`dataset_test()` rules:** one `dataset` reference, keyed by the `dataset_id`; every `source_id` value is the `dataset_id` or a `source` key (a value naming a `secondary` key gets "move to `source`"); every `source` reference is named by some record.
8. **A key names one reference across the database.** `austraits::bind_databases()` stops when one key holds different references in different datasets, since it keeps only one per key.

## Invariants a later edit could break

- Every consumer of `metadata$source` goes through `util_flatten_sources()`; do not read the block's names directly.
- `source_id` is filled after `observation_id` is generated; moving it earlier renumbers observations.
- `dataset_test()` must keep flattening with `strict = FALSE`, or an unconverted file reports only the shape and nothing else.
- Golden fixtures: `traits.csv` / `excluded_data.csv` differ from before only by `source_id` moving to the second column and being filled; `observation_id` is unchanged.
- austraits must ship with `join_sources()` and the `extract_data()` change before a database built this way is released: earlier `extract_data()` fails on the renamed `methods` columns.

## Checklist

- [x] `util_flatten_sources()`, `metadata_convert_sources()`, build and `dataset_test()` changes, `metadata_add_source_*()`, external reference files
- [x] Fixtures converted with `metadata_convert_sources()`; golden outputs regenerated
- [x] Schema, NEWS, dataset report
- [x] Data model 1.3.0 (replaces the unshipped 1.2.1)
- [x] austraits: `join_sources()`, `extract_data()`, `flatten_database()`, `as_wide_table()`, `bind_databases()` key check (branch `source-references-296`, not yet a PR)
- [ ] austraits.build #878 reworked (conversion, rekeys, curator list #879); issue to combine Gallagher_2011_1/_3
- [ ] ausinvertraits.build, AusFizz conversions; book wording; responses.build port
