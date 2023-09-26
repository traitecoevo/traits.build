
# Test Dataset 3: `Test_2023_3`

This dataset is for testing the following, for long datasets:
- Dataset-level and trait-level input of `basis_of_record`, `life_stage`, `entity_type`, `value_type`, `basis_of_value`, `measurement_remarks`, `collection_date` and `replicates`
- `entity_type` read in as a fixed value if there is an existing column named as that fixed value
- `replicates` in character format
- Units are correctly converted to the accepted units
- Contexts (correct `link_ids` and `link_vals`; duplicates get collapsed), with and without `find` and `description` values (the latter especially relevant for numeric contexts), or entered at the trait level (not possible right now)
- Methods (methods are unique with their own `method_id`)
- Locations (latitude and longitude fields missing for one location and some NAs in column)
- `trait_name` is NA
- Types of trait data -- numeric (entered as either true numeric type or character type) and categorical, time (e.g. `flowering_time`)
- Excluded values, such as out of allowable range, invalid categorical values, invalid time values
- Substitutions work for categorical and time traits, replacing with NA or finding NA


Test_2023_3 is a copy of NHNSW_2023 with the following modifications:
- Subsetted the dataset to only `fruit_colour`, `fruit_dehiscence`, `fruit_length`, `fruit_width` and randomly sampled 200 rows
- Read in `value_type` from trait level
- Read in `entity_type` as a fixed value `species` at the trait level and added a dummy column called `species`
- `fruit_length` units is same as units in the definition, so no conversion should happen
- `fruit_width` units are read in from a column at the trait level
- Changed trait scoring method context to substitute "scored from text" with "scored_from_text", replaced "unknown" with "fruit" to test duplicates being collapsed
- Split `fruit_width` into two traits using `custom_R_code` and input with different methods to check `method_id`
- Added two made-up locations with one location missing latitude and longitude fields
- Duplicated `fruit_length` and set `trait_name` to .na
- Added a `flowering_time` trait
- Invalid `flowering_time` value, invalid `fruit_colour` value ("brunneous" instead of "brown"), out of range `fruit_length` value (3000 mm) should go to `excluded_data`
- Changed a "indehiscent" value to "not dehiscent" for substitutions, added some `flowering_time` values to be substituted

See output/ for expected output files.

