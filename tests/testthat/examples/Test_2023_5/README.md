
# Test Dataset 5: `Test_2023_5`

This dataset is for testing:
- `individual_id` for wide datasets
- Different `bibtype`s and secondary and original sources (also check methods table), with `source_id` column (check joining to methods table somewhere else?)
- No locations or contexts
- Units read in from a column (check unit conversions are working)
- Bins and ranges have units that are converted correctly

Lizzy recommends this dataset for testing units:
*"And if you're hunting for a dataset with good info to test units & source_id, maybe take some snippets from Richards_2008. There are separate columns for SLA and LMA. You could take just the rows with data for one of those and then merge them into a single column with a second column for units. And have some dummy data that is a bin - like made up lifespan data with a required unit conversion from either a column or within the metadata."*

Test_2023_5 is a copy of Falster_2005_1 (maybe change to Richards_2008) with the following modifications:
- Added all `bibtype` options to the dataset via secondary/original sources

See output/