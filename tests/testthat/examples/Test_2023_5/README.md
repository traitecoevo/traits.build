
# Test Dataset 5: `Test_2023_5`

This dataset is for testing:
- `individual_id` for wide datasets
- Different `bibtype`s and secondary and original sources (also check methods table), with `source_id` column (check joining to methods table somewhere else?)
- No locations or contexts
- Units read in from a column (check unit conversions are working)
- Bins and ranges have units that are converted correctly

Test_2023_5 is a copy of Richards_2008 with the following modifications:
- Added all `bibtype` options to the dataset via secondary/original sources
- Subsetted to SLA and leaf lifespan data, added units column for both
- Changed some values to bins in leaf lifespan data
- Removed locations, contexts, and substitutions

See output/ for expected output files.
