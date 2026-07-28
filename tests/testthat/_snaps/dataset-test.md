# `dataset_test` reports the errors Test_2023_9 was built to provoke

    Code
      writeLines(dataset_test_failures("Test_2023_9"))
    Output
      examples/Test_2023_9/metadata.yml	metadata - disallowed characters in metadata detected:
      - ln 149:     leaves at the tip of each individual. Testing of disallowed characters - Ψ◦℃.
      examples/Test_2023_9/metadata.yml	dataset - `dataset` field names include(s) invalid terms: 'basis_of_value', 'value_type'
      examples/Test_2023_9/metadata.yml	contexts - `value`'s should only have one unique description each: 'NA'
      examples/Test_2023_9/metadata.yml	contexts - duplicate `find` values detected: 'NA'
      examples/Test_2023_9/metadata.yml	contexts - `find: bottom slope` in the `context_property` 'slope position' is not accompanied by a `value` field
      examples/Test_2023_9/metadata.yml	contexts - `find` value in the `context_property` 'nutrient treatment' should not be NA
      Please replace NAs with desired value using `custom_R_code`
      examples/Test_2023_9/metadata.yml	contexts - `find` value in the `context_property` 'instrument used' should not be NA
      Please replace NAs with desired value using `custom_R_code`
      examples/Test_2023_9/metadata.yml	contexts - `description: Measured on a 50 mm branch segment` in the `context_property` 'branch length' is not accompanied by `find` and `value` fields
      examples/Test_2023_9/metadata.yml	contexts - `description: Measured on a 1000 mm branch segment` in the `context_property` 'branch length' should not be accompanied by an NA `value` field
      Please use `custom_R_code` to assign a proper value to NA fields
      examples/Test_2023_9/metadata.yml	contexts - values of 'nutrient treatment' in metadata not detected in context values from data file: 'NA'
      examples/Test_2023_9/metadata.yml	contexts - context values of 'sampling time of day' from data file not present in metadata contexts: '09:00:00'
      'sampling time of day' has been detected as a time data type and reformatted
      -> Please make sure context metadata matches reformatting
      examples/Test_2023_9/metadata.yml	contexts - values of 'sampling time of day' in metadata not detected in context values from data file: '9:00:00'
      examples/Test_2023_9/metadata.yml	contexts - values of 'instrument used' in metadata not detected in context values from data file: 'instrument 4'
      examples/Test_2023_9/metadata.yml	contexts - context values of 'method_context' from data file not present in metadata: '50 mm branch', '1000 mm branch'
      examples/Test_2023_9/metadata.yml	contexts - values of 'method_context' in metadata not detected in context values from traits metadata: 'NA', 'NA'

