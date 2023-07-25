
# Test Dataset 1: `Test_2023_1`

This dataset is for testing the following, for wide datasets:
- Dataset-level input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates`
- Units are correctly converted to the accepted units
- Contexts (correct `link_ids` and `link_vals`; duplicates get collapsed), with and without `find` and `description` values (or NA values), or entered at the trait level
- Methods (methods are unique), including method contexts
- Locations (latitude and longitude missing)
- `trait_name` is NA
- `individual_id`
- Types of trait data -- numeric (entered as either true numeric type or character type), categorical, time (e.g. `flowering_time`)
- Excluded values, such as out of allowable range, invalid categorical values, invalid time values, and that excluded values table are filled in with correct error types
- Substitutions work for categorical, numeric (e.g. ranges) and time traits, with NA find or replace values
- Combinations of multiple categorical values per row, including if there are duplicate trait values within
- Taxonomic updates for different taxonomic resolutions
- `observation_id` refers to unique observations
- Duplicate trait values

Test_2023_1 is a copy of Falster_2005_1 with the following modifications:
-

See output/

