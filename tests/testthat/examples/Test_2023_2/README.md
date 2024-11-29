
# Test Dataset 2: `Test_2023_2`

This dataset is for testing the following, for wide datasets:
- Input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates` from a column at the dataset level versus at the trait and location level (or from a column at the trait level)
- `entity_type` read in as a fixed value if there is an existing column named as that fixed value
- `source_id` column
- Locations (latitude and longitude missing for both locations)
- Test `method_id` when `value_type` is read from a column at the trait level
- Read in `collection_date` as a column at the dataset level (check `observation_id`)
- Check that `location_id` is NA for species `entity_type` measurements

Test_2023_2 is a copy of Falster_2005_1 with the following modifications:
- Added `replicates` as a column at the dataset level, with also trait-level metadata (included some NAs)
- Added `basis_of_value` as a column at the trait level, with also some traits with fixed values (included some NAs)
- Added `value_type` as a column at the trait level ("LASA1000"), not dataset-level (fixed value at dataset level) (included NAs in column)
- Added `basis_of_record` as dataset-level, trait-level and location-level metadata
- Added `measurement_remarks` as dataset-level column metadata, trait-level metadata and location-level metadata
- Added `life_stage` as dataset-level column metadata, location-level metadata and trait-level column metadata
- Read in `entity_type` as a fixed value and renamed "Species" column to "species"
- Added `source_id` column
- Removed latitude and longitude fields for both locations
- Add duplicate of LASA1000 to test `method_id` when `value_type` is read from a column at the trait level
- Added `collection_date` as location-level metadata and added a column for `collection_date` at dataset level (duplicated row for Acacia celsa to test `observation_id`)
- Added `specimen` with fake hypothetical specimen numbers. In this case the `identifier_type` is not a value specified in the schema, so confirming that these are suggestions rather than requirements.

See output/ for expected output files.
