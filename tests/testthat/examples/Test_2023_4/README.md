
# Test Dataset 4: `Test_2023_4`

This dataset is for testing the following, for long datasets:
- Input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates` from a column at the dataset level versus at the trait and location level (or from a column at the trait level)
- Units read in from a column
- Replicates in numeric format
- `source_id` column
- Locations (latitude and longitude not missing)
- `repeat_measurements_id` at the trait level, for individuals (using and not using `individual_id`), populations and species (including when it is specified as TRUE and FALSE when a trait is entered twice)
- Check that `location_id` is NA for species `entity_type` measurements, and hence location-level metadata does not overwrite metadata

Test_2023_4 is a copy of NHNSW_2023 with the following modifications:
- Subsetted the dataset to only `fruit_colour`, `fruit_dehiscence`, `fruit_length`, `fruit_width` and randomly sampled 200 rows
- Added `entity_type` as a column, and read in as a dataset-level column with also trait-level and location-level metadata (included some NAs)
- Read `value_type` as a column and as fixed values at the trait level
- Added `basis_of_value` and `basis_of_record` as columns at the trait-level, with a fixed value at the dataset level for the latter (included NAs in column) (and missing `basis_of_record` for `fruit_width`)
- Added `life_stage`, `replicates`, as dataset-level, trait-level and location-level metadata
- Added `measurement_remarks` as dataset-level and location-level metadata
- Added `collection_date` as location-level metadata (set as NA at dataset level because otherwise `custom_R_code` won't be detected correctly -- see `read_metadata` function)
- Removed trait-level `unit_in` for `fruit_colour` and `fruit_length` and instead read their units in from a column at the dataset level
- Converted some `fruit_length` units to cm (and changed the "units" column)
- For `fruit_width`, put `units` column to NA using `custom_R_code` to test that units are read in at the trait level while they are NA in the column at the dataset level
- Added `fruit_width_2` to test units read in from a column for bin conversions
- Added `source_id` column (with NAs)
- Added two made-up locations
- Added `leaf_photosynthesis` and `leaf_stomatal_conductance` to test `repeat_measurements_id`, with another `leaf_stomatal_conductance_2` variable that doesn't specify `repeat_measurements_id`
- See Phyla nodiflora in `data.csv` to test that location-level metadata isn't filled in for the observations since `entity_type` is initially "species"

See output/ for expected output files.
