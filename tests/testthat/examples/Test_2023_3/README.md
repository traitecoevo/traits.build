
# Test Dataset 3: `Test_2023_3`

This dataset is for testing the following, for long datasets:
- Dataset-level and trait-level input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates`
- `replicates` in character format
- Units are correctly converted to the accepted units
- Contexts (correct `link_ids` and `link_vals`; duplicates get collapsed), with and without `find` and `description` values (the latter especially relevant for numeric contexts), or entered at the trait level
- Methods (methods are unique), including method contexts
- Locations (latitude and longitude missing)
- `trait_name` is NA
- Types of trait data -- numeric (entered as either true numeric type or character type), categorical, time (e.g. `flowering_time`)
- Excluded values, such as out of allowable range, invalid categorical values, invalid time values
- Substitutions work for categorical, numeric (e.g. ranges) and time traits, replacing with NA or finding NA


Test_2023_3 is a copy of Stephens_2021 with the following modifications:
-

See output/

