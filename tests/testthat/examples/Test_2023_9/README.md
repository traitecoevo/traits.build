
# Test Dataset 9: `Test_2023_9`

This dataset is for testing `dataset_test`. Erroneous metadata entries have been added to check the expected output of `dataset_test` (more to be added).

It carries deliberate faults, so unlike the other examples it is **expected to fail `dataset_test`**. The report it produces is pinned as a snapshot in `tests/testthat/_snaps/dataset-test.md`; that snapshot is the only coverage the package has of the validation layer actually rejecting bad input. When it changes, work out which check changed and why before running `snapshot_accept()`.

Note that not every message in that snapshot comes from a fault added on purpose. The `trait N - does not contain: 'value_type', 'basis_of_value'` and `taxonomic_update N - does not contain: 'taxonomic_resolution'` lines are this metadata having been written before those fields entered the schema, and were only visible once the dataset was un-commented in Stage 0 of #225. Tidying them is worth doing; doing so will change the snapshot.

Its build output in `output/` is also compared, like the other examples. Those files were three years stale until Stage 0 regenerated them.

Test_2023_9 is a copy of Falster_2005_1 with the following modifications:
- `entity_type`, `basis_of_value`, `value_type` and `replicates` were moved to dataset level fixed value in metadata.yml, except for `flowering_time`, `huber_value` (LASA1000) and `plant_growth_form` where they're specified at the trait level
- Added `measurement_remarks` to dataset level as fixed value in metadata.yml
- Added all different types of contexts (`entity_context`, `plot`, `treatment`, `temporal`, `method`) with some NA `find` and `value` values, duplicate `value` values, numeric and character type values, as separate columns (sex, slope position, nutrient treatment, sampling time of day, instrument used) plus entered at the trait level as a fixed value (for the `huber_value` traits)
- Removed latitude and longitude for Cape Tribulation
- Changed `trait_name` field for `branch_mass_fraction` to NA
- Added a categorical trait, `plant_growth_form`, and time trait, `flowering_time`
- Changed a numeric column `wood_density` to character type with `custom_R_code`
- Duplicated `flowering_time` value for Acacia celsa
- Excluded observation 0.17 for `leaf_mass_per_area`
