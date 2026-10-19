
# Test Dataset 5: `Test_2023_5`

This dataset is for testing:
- `individual_id` for wide datasets
- Different `bibtype`s across `original_source` and `secondary` references (also check methods table), with a `source_id` column whose values are the keys of the primary and `original_source` references
- No locations or contexts
- Units read in from a column (check unit conversions are working)
- Bins and ranges have units that are converted correctly

Test_2023_5 is a copy of Richards_2008 with the following modifications:
- Added `individual_id` column and changed `entity_type` to individual (made up some values to test `individual_id`)
- Added all `bibtype` options to the dataset via `original_source` references (the five studies records are taken from, each named in the `source_id` column) and one fictional `secondary` reference (a later publication; no record points at it)
- Changed the `source_id` column and metadata to include more variety of sources
- Removed locations, contexts, and substitutions
- Subsetted to SLA and added units column
- Added some bin values in leaf lifespan data and add units column

See output/ for expected output files.
