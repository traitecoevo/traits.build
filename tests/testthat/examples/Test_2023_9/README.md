
# Test Dataset 9: `Test_2023_9`

This dataset is for testing `dataset_test` (not yet implemented). Erroneous metadata entry has been added to check the expected output of `dataset_test` (more to be added).

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
