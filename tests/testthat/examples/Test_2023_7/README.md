
# Test Dataset 7: `Test_2023_7`

This dataset is for testing:
- `repeat_measurements_id` at the dataset level for response curve data in wide format

Test_2023_7 is a copy of 'Cernusak_2011/raw/full_curve_data.csv' with the following modifications:
- Subsetted to one location: Howard Springs
- Removed columns after column 15
- `data.csv`'s `Date` column held non-ISO dates (`2-Sep-08`, `3-Sep-08`, i.e. day-Mon-yy) inherited from the source file, unnoticed until the `collection_date`-parses check was added to `dataset_test`. Reformatted to `2008-09-02`/`2008-09-03`

See output/ for expected output files.
