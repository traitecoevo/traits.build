
# Test Dataset 1: `Test_2023_1`

This dataset is for testing the following, for wide datasets:
- Dataset-level and trait-level input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates`
- `replicates` in numeric format
- Units are correctly converted to the accepted units, either from a fixed trait-level value or from a column at the trait level
- Bins are correctly converted to the accepted units, from a fixed trait value or from a column at the trait level
- Contexts (correct `link_ids` and `link_vals`; duplicates get collapsed), with and without `find` and `description` values (or NA values), or entered at the trait level as a fixed value or a column (don't think this is possible right now)
- Methods (methods are unique with their own `method_id`), including method contexts
- Locations (latitude and longitude fields missing for one location, entered as .na for another location)
- `trait_name` is NA
- Types of trait data -- numeric (entered as either true numeric type or character type), categorical, time (e.g. `flowering_time`)
- Excluded values (automatic and manual), such as out of allowable range, invalid categorical values, invalid time values, and that excluded values table are filled in with correct error types
- Substitutions work for categorical and time traits, with NA replace values
- Combinations of multiple categorical values per row, including if there are duplicate trait values within
- *Taxonomic updates for different taxonomic resolutions - have yet to add*
- `observation_id` refers to unique observations
- `repeat_measurements_id` at the trait level, for individuals (using `individual_id`), populations and species (including when it is specified as TRUE and FALSE when a trait is entered twice)

Test_2023_1 is a copy of Falster_2005_1 with the following modifications:
- `entity_type` and `replicates` were moved to dataset level fixed value in metadata.yml, except for `flowering_time`, `huber_value` (LASA1000), `plant_growth_form`, `leaf_length` and `leaf_photosynthesis` (and some `excluded_data` dummy traits) where they're specified at the trait level
- Added `measurement_remarks` to dataset level as fixed value, as well as at trait level
- Added `seed_dry_mass` to check bin conversions, with `seed_dry_mass_units` as the column to read in units from
- Added a column `leaf_length` with accompanying `leaf_length_units` to test unit conversions read in from a column
- Added `bin_conversions` column to test bin unit converions for a fixed trait level `unit_in` value
- Added all different types of contexts (`entity_context`, `plot`, `treatment`, `temporal`, `method`) with some NA `find` and `value` values, duplicate `value` values, numeric and character type values, as separate columns (sex, slope position, nutrient treatment, sampling time of day, instrument used) plus entered at the trait level as a fixed value (for the `huber_value` traits)
- Added a context 'plant diameter' to check whether omitting lists of values from context metadata works
- Added duplicate of LASA50 column to check same `method_id`, added duplicate of LASA1000 with different methods to check unique `method_id`
- Removed latitude and longitude for Atherton and set to .na for Cape Tribulation
- Changed `trait_name` field for `branch_mass_fraction` to NA
- Added a categorical trait, `plant_growth_form`, and time trait, `flowering_time`
- Changed a numeric column `wood_density` to character type with `custom_R_code`
- Excluded observation Syzygium gustavioides using `exclude_observations` in metadata
- Changed leaf nitrogen for Acronychia acidula to 100 for checking out of allowable range error, "unknown" should be an excluded value for `plant_growth_form` and there should be an invalid `flowering_time` value
- Added substitutions for `plant_growth_form` and `flowering_time`
- Added `leaf_photosynthesis`, `leaf_stomatal_conductance` and `leaf_stomatal_conductance_2` to test `repeat_measurements_id`, with another `leaf_stomatal_conductance_3` variable that doesn't specify `repeat_measurements_id`

See output/ for expected output files.
