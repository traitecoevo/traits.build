
# Test Dataset 4: `Test_2023_4`

This dataset is for testing the following, for long datasets:
- Input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates` from a column versus at the trait and location level (or from a column at the trait level)
- `source_id` column
- Locations (latitude and longitude not missing)

Test_2023_4 is a copy of NHNSW_2023 with the following modifications:
- Subsetted the dataset to only `fruit_colour`, `fruit_dehiscence`, `fruit_length`, `fruit_width` and randomly sampled 200 rows
- Added `entity_type` as a column, and read in `entity_type` and `value_type` as dataset-level columns with also trait-level and location-level metadata (included some NAs)
- Added `basis_of_value` and `basis_of_record` as columns at the trait-level, with a fixed value at the dataset level for the former (included NAs in column) (and missing `basis_of_record` for `fruit_width`)
- Added `life_stage`, `replicates`, as dataset-level, trait-level and location-level metadata
- Added `measurement_remarks` as dataset-level and location-level metadata
- Added `collection_date` as location-level metadata (set as NA at dataset level because otherwise `custom_R_code` won't be detected correctly -- see `read_metadata` function)
- Added `source_id` column (with NAs)
- Added two made-up locations

See output/