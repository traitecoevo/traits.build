
# Test Dataset 2: `Test_2023_2`

This dataset is for testing the following, for wide datasets:
- Input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates` from a column versus at the trait and location level (or from a column at the trait level)
- `entity_type` read in as a fixed value if there is an existing column named as that fixed value
- `source_id` column
- Locations (latitude and longitude missing for both locations)

Test_2023_1 is a copy of Falster_2005_1 with the following modifications:
- Added `replicates`, `basis_of_value` as a column, with also trait-level and location-level metadata (included some NAs)
- Added `value_type` as a column but read in at the trait-level ("LASA1000"), not dataset-level (fixed value at dataset level) (included NAs in column)
- Added `basis_of_record`, `life_stage` as dataset-level, trait-level and location-level metadata
- Added `collection_date` as dataset-level and location-level metadata
- Added `measurement_remarks` as location-level metadata
- Read in `entity_type` as a fixed value and renamed "Species" column to "species"
- Added `source_id` column
- Removed latitude and longitude fields for Atherton location and put as .na for Cape Tribulation

See output/
