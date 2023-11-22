
# Test Dataset 6: `Test_2023_6`

This dataset is for testing:
- `individual_id` for long datasets
- Different `bibtype`s and secondary and original sources (also check methods table)
- Locations (latitude and longitude ranges instead of latitude and longitude points)
- Check `observation_id` is different for different locations (Eucalyptus grandis) and the same for the same location (Archidendron whitei) or no location (Grevillea robusta)
- Replicates in a mix of character and numeric format

Test_2023_6 is a copy of Richards_2008 modified for Test_2023_5 with the following modifications:
- Test_2023_5 pivoted to long format
- Added locations with latitude and longitude ranges
- Added replicates in metadata with character and numeric format

See output/ for expected output files.
