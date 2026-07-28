
# Test Dataset 9: `Test_2023_9`

This dataset is for testing `dataset_test`. Erroneous metadata entries have been added to check the expected output of `dataset_test` (more to be added).

It carries deliberate faults, so unlike the other examples it is **expected to fail `dataset_test`**. The report it produces is pinned as a snapshot in `tests/testthat/_snaps/dataset-test.md`; that snapshot is the only coverage the package has of the validation layer actually rejecting bad input. When it changes, work out which check changed and why before running `snapshot_accept()`.

Every message in that snapshot maps to a modification listed below. That was not true until #235: `trait N - does not contain: 'value_type', 'basis_of_value'` (x10) and `taxonomic_update N - does not contain: 'taxonomic_resolution'` (x3) were this metadata predating those schema fields, invisible until the dataset was un-commented in Stage 0 of #225. Declaring the fields explicitly removed those 13 messages and changed no trait value, because the build had been supplying `value_type`/`basis_of_value` from the dataset block all along.

One message does need understanding before anyone tidies it away: `` dataset - `dataset` field names include(s) invalid terms: 'basis_of_value', 'value_type' ``. The schema lists those two fields under `traits`, not `dataset`, so `dataset_test` rejects them there — but the build honours them, inheriting to any trait that does not declare its own. No dataset in `austraits.build`, `ausinvertraits.build` or `AusFizz` uses that path, and no sibling fixture does either; this metadata is the only place it appears anywhere. The dataset-level entries are kept deliberately, for two reasons: they are the snapshot's only coverage of the dataset-block allowed-names check, and they keep the validator/build disagreement visible until someone decides whether that inheritance is supported (see the discussion on #235). Removing them would make the snapshot shorter and the question invisible.

Its build output in `output/` is also compared, like the other examples. Those files were three years stale until Stage 0 regenerated them.

Test_2023_9 is a copy of Falster_2005_1 with the following modifications:
- `entity_type` and `replicates` were moved to dataset level fixed value in metadata.yml, except for `flowering_time`, `huber_value` (LASA1000) and `plant_growth_form` where they're specified at the trait level
- `basis_of_value` and `value_type` are declared at dataset level *and* on every trait. The dataset-level pair is what the `invalid terms` message above reports; the per-trait declarations are what `dataset_test` requires, and they restate what the dataset block was already supplying
- Added `measurement_remarks` to dataset level as fixed value in metadata.yml
- Added all different types of contexts (`entity_context`, `plot`, `treatment`, `temporal`, `method`) with some NA `find` and `value` values, duplicate `value` values, numeric and character type values, as separate columns (sex, slope position, nutrient treatment, sampling time of day, instrument used) plus entered at the trait level as a fixed value (for the `huber_value` traits)
- `slope position` has a `find` with no accompanying `value`. `nutrient treatment` and `instrument used` each have a `find: .na.character`, which is also what makes the NA `find` a duplicate across two context properties, and in `instrument used` that `find` maps to a `value: instrument 4` that appears nowhere in the data. Their NA `value`s carry an NA `description` while `branch length`'s NA `value` carries a real one, which is what trips the "one unique description each" check
- The `branch length` context is declared against `var_in: method_context`, which is not a column in `data.csv` — its values come from the trait-level `method_context` fields. Of its three values, one carries a `description` with no `find` or `value`, and one carries a `description` with an NA `value`, so only `250 mm branch` matches a trait
- `sampling time of day` is recorded in metadata as `9:00:00` where the build reformats the data column to `09:00:00`. Whether that missing leading zero was deliberate is not recoverable from the history — `Test_2023_1` carried the same value and had it corrected to `09:00:00` in #51, a month before this dataset was written. Leave it: the `hms` branch of the context-values check in `R/testdata.R` — added in #88 for exactly this failure mode — fires nowhere else in the suite, so the three messages it produces here are its only coverage
- The `LMA (mg mm-2)` methods text ends in `Ψ◦℃`, to exercise the disallowed-characters check. Its line number appears in the snapshot, so inserting lines above it in metadata.yml will change that message
- Removed latitude and longitude for Cape Tribulation
- Changed `trait_name` field for `branch_mass_fraction` to NA
- Added a categorical trait, `plant_growth_form`, and time trait, `flowering_time`
- Changed a numeric column `wood_density` to character type with `custom_R_code`
- Duplicated `flowering_time` value for Acacia celsa
- Excluded observation 0.17 for `leaf_mass_per_area`
