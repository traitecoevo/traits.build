
# Test Dataset 1: `Test_2023_1`

This dataset is for testing the following, for wide datasets:
- Dataset-level and trait-level input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates`
- `replicates` in numeric format
- Units are correctly converted to the accepted units, either from a fixed trait-level value or from a column at the trait level
- Contexts (correct `link_ids` and `link_vals`; duplicates get collapsed), with and without `find` and `description` values (or NA values), or entered at the trait level as a fixed value or a column (don't think this is possible right now)
- Methods (methods are unique), including method contexts
- Locations (latitude and longitude missing for one location, entered as .na)
- `trait_name` is NA (don't think this needs a test)
- Types of trait data -- numeric (entered as either true numeric type or character type), categorical, time (e.g. `flowering_time`)
- Excluded values (automatic and manual), such as out of allowable range, invalid categorical values, invalid time values, and that excluded values table are filled in with correct error types
- Substitutions work for categorical and time traits, with NA replace values
- Combinations of multiple categorical values per row, including if there are duplicate trait values within
- Taxonomic updates for different taxonomic resolutions
- `observation_id` refers to unique observations
- Duplicate trait values
- Test `method_id`

Test_2023_1 is a copy of Falster_2005_1 with the following modifications:
- `entity_type`, `basis_of_value`, `value_type` and `replicates` were moved to dataset level fixed value in metadata.yml, except for `flowering_time`, `huber_value` (LASA1000) and `plant_growth_form` where they're specified at the trait level
- Added `measurement_remarks` to dataset level as fixed value in metadata.yml
- Added all different types of contexts (`entity_context`, `plot`, `treatment`, `temporal`, `method`) with some NA `find` and `value` values, duplicate `value` values, numeric and character type values, as separate columns (sex, slope position, nutrient treatment, sampling time of day, instrument used) plus entered at the trait level as a fixed value (for the `huber_value` traits)
- Added a context 'plant diameter' to check whether omitting lists of values from context metadata works
- Removed latitude and longitude for Cape Tribulation
- Changed `trait_name` field for `branch_mass_fraction` to NA
- Added a categorical trait, `plant_growth_form`, and time trait, `flowering_time`
- Changed a numeric column `wood_density` to character type with `custom_R_code`
- Duplicated `flowering_time` value for Acacia celsa
- Excluded observation 0.17 for `leaf_mass_per_area`
- Added `seed_dry_mass` to check bin conversions, with `seed_dry_mass_units` as the column to read in units from
- Added a column `leaf_length` with accompanying `leaf_length_units` to test unit conversions read in from a column
- Added duplicate of LASA50 column to check `method_id`
- Added a substitution for `plant_growth_form` and `flowering_time`

See output/ for expected output files.
