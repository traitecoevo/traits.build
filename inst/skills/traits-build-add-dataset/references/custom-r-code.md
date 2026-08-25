# `custom_R_code` pattern catalogue

Condensed from `adding_data_long.qmd`'s "Custom R code" section. `custom_R_code`
is what makes the `data.csv` fidelity policy possible: `data.csv` matches the
provided file, and everything else -- missing-value codes, unit-bearing
headers, categorical spellings, derived columns, de-duplication, date
reformatting -- happens here instead of by editing the CSV. If a change to
`data.csv` could instead be expressed as one of the patterns below, it
belongs here, not in the file.

## How it runs

`process_custom_code()` (`R/process.R`) evaluates `metadata$dataset$custom_R_code`
immediately after reading `data.csv`, before anything else touches the data:

```r
data <-
  readr::read_csv(filename_data_raw, col_types = cols(), guess_max = 100000, progress = FALSE) %>%
  process_custom_code(metadata[["dataset"]][["custom_R_code"]])()
```

## Syntax rules

- Wrap the whole block in a single apostrophe (`'`) at start and end -- this is
  what allows line breaks between pipes in the YAML.
- Start with `data %>%`, then chain whatever fixes are needed.
- Use `dplyr`, `tidyr`, `stringr` (`mutate`, `rename`, `summarise`,
  `str_detect`, ...) or the repo's own `R/custom_R_code.R` helpers
  (`separate_range`, `replace_duplicates_with_NA`, etc. -- check what a given
  host repo actually provides with `scripts/probe_repo.R`, since
  `ausinvertraits.build`/`AusFizz` don't ship the same helper set as
  `austraits.build`). Avoid other packages.
- **Never reads files.** Merging or reading multiple source files happens
  before `data.csv` is written, not here -- this is the reason "multiple
  files that must be joined" is one of the few legitimate reasons to depart
  from the source file in Phase 2.
- Chain with pipes where possible; use `;` between statements only if the
  data genuinely needs to be split into multiple frames and rejoined.
- Verify with `metadata_check_custom_R_code(dataset_id)` before running
  `dataset_test()`.

## The eleven patterns

### 1. Phenology strings -> 12-character `NY` vectors

Herbaria commonly record `flowering_time`/`fruiting_time` as a month span;
`traits.build` encodes phenology as 12 `N`/`Y` characters, one per month.
`austraits.build`'s `R/custom_R_code.R` provides (where present --
`probe_repo.R` reports this):

- `format_flowering_months(start, end)` -- start/end month pair -> binary string
- `convert_month_range_string_to_binary(str)` -- a month-range string -> binary string
- `convert_month_range_vec_to_binary(vec)` -- a vector of month ranges -> binary strings
- `collapse_multirow_phenology_data_to_binary_vec` -- multi-row phenology -> one binary string

### 2. Splitting a range into min/max columns

`leaf_length`, `seed_length`, etc. recorded as `"2-8"` need separate
`minimum`/`maximum` columns:

```r
separate_range <- function(data, x, y1, y2, sep = "-", remove = TRUE) {
  data <- tidyr::separate(data, !!x, sep = "-", into = c(y1, y2), remove = remove, fill = "right")
  data[[y2]] <- ifelse(is.na(data[[y2]]), data[[y1]], data[[y2]])
  data
}
```

### 3. De-duplicating within a dataset

A species-level value repeated on every individual-level row (`plant growth
form`), or a bulked-sample value repeated per contributing individual, needs
the repeats replaced with `NA` -- group by whatever identifies the true
unique unit first (species, population, etc.):

```r
data %>%
  dplyr::group_by(Species) %>%
  dplyr::mutate(across(c(`leaf_percentN`, `plant growth form`), replace_duplicates_with_NA)) %>%
  dplyr::ungroup()
```

where `replace_duplicates_with_NA <- function(x) base::replace(x, duplicated(x), NA)`.
For cross-dataset duplicates (the same measurement contributed by two
studies), see `references/troubleshooting.md` -- no general automated
function exists for that yet.

### 4. Missing-value placeholders -> `NA`

```r
data %>%
  dplyr::mutate(across(c(`height (cm)`, `leaf area (mm2)`), ~ na_if(., 0)))
```

### 5. Copying values into a second trait

Some values in one column are *also* a value for a different trait (e.g.
`fruit_type == "pome"` also means `fruit_fleshiness == "fleshy"`):

```r
data %>%
  dplyr::mutate(fruit_fleshiness = ifelse(`fruit type` == "pome", "fleshy", NA))
```

### 6. Moving values into a second trait

Some values *belong to* a different trait rather than merely implying one --
use `move_values_to_new_trait()` (create the destination column first):

```r
move_values_to_new_trait <- function(data, original_trait, new_trait, original_values,
                                      values_for_new_trait, values_to_keep) {
  for (j in seq_along(original_values)) {
    i <- data[[original_trait]] == original_values[[j]]
    i <- ifelse(is.na(i), "FALSE", i)
    data[[new_trait]] <- ifelse(i, values_for_new_trait[[j]], data[[new_trait]])
    data[[original_trait]] <- ifelse(i, values_to_keep[[j]], data[[original_trait]])
  }
  data
}
```

```r
data %>%
  dplyr::mutate(new_trait = NA_character_) %>%
  move_values_to_new_trait(
    original_trait = "growth form", new_trait = "parasitic",
    original_values = "parasitic", values_for_new_trait = "parasitic",
    values_to_keep = "xx"
  ) %>%
  dplyr::mutate(across(c(`growth form`), ~ na_if(., "xx")))
```

Known wart, straight from the book: `values_to_keep` doesn't accept `NA`,
hence the placeholder `"xx"` + follow-up `na_if()` -- not a nicer way to
write this today.

### 7. Deriving a new trait from existing columns

```r
data %>%
  dplyr::mutate(root_mass_fraction = `root mass` / (`root mass` + `shoot mass`))
```

### 8. Mutating a `location_name` column

When the source has location information but no usable location name
column:

```r
data %>%
  dplyr::mutate(
    location_name = dplyr::case_when(
      longitude == 151.233056 ~ "heath",
      longitude == 151.245833 ~ "terrace",
      longitude == 151.2917   ~ "diatreme"
    )
  )
# rows matching no condition become NA with `case_when` -- make sure that's
# either intended or that every value is covered.
```

or, for a compound name from two columns:

```r
data %>%
  dplyr::mutate(
    location_name = ifelse(location_name == "Mt Field" & habitat == "Montane rainforest",
                           "Mt Field_wet", location_name)
  )
```

or, generating a name straight from coordinates:

```r
data %>%
  dplyr::mutate(location_name = paste0("lat_", round(latitude, 3), "_long_", round(longitude, 3)))
```

### 9. `measurement_remarks` from a notes column

For per-row free text that isn't a `methods` fact (fixed per trait) or a
`context` (a controlled category):

```r
data %>%
  dplyr::mutate(measurement_remarks = paste0("maternal lineage ", Mother))
```

### 10. Reformatting dates to `yyyy-mm-dd` / `yyyy-mm`

Pick the parser that matches the source format -- guessing wrong silently
transposes day and month:

```r
data %>% dplyr::mutate(Date = Date %>% lubridate::mdy())   # "Dec 3 2015" -> "2015-12-03"
data %>% dplyr::mutate(Date = Date %>% lubridate::dmy())   # "3-12-2015" -> "2015-12-03"

# "Dec 2015" -> "2015-12"
data %>% dplyr::mutate(Date = lubridate::parse_date_time(Date, orders = "my") %>% base::format.Date("%Y-%m"))

# Excel reinterpreted a month-only date as a full date, e.g. "12-01-2015" -> "2015-12"
data %>% dplyr::mutate(Date = lubridate::parse_date_time(Date, orders = "mdy") %>% base::format.Date("%Y-%m"))
```

Mixed-resolution dates (some rows `yyyy-mm`, others `yyyy-mm-dd`) -- parse
what parses, fall back to the raw value otherwise:

```r
data %>%
  dplyr::mutate(
    weird_date = ifelse(stringr::str_detect(gathering_date, "^[0-9]{4}"), gathering_date, NA),
    gathering_date = gathering_date %>% lubridate::mdy(quiet = TRUE) %>% as.character(),
    gathering_date = dplyr::coalesce(gathering_date, weird_date)
  ) %>%
  dplyr::select(-weird_date)
```

### 11. Numeric range formatting

`format_min_max_as_range(data, min_column, max_column, range_column,
column_value_type)` -- the inverse of pattern 2, for when the database
wants a combined range string rather than separate min/max columns. Check
`probe_repo.R`'s report before relying on it; it lives in
`austraits.build`'s `R/custom_R_code.R`, not the package.

## `individual_id` — a pattern to avoid, not use

Not one of the eleven, but the most consequential mistake to make in
`custom_R_code`: never mutate a placeholder `individual_id` column and fill
it with `"unknown"`. `individual_id: unknown` (or a mutated column full of
the same value) silently assigns every row in the dataset to one individual.
Omit `individual_id` entirely rather than filling it with a constant.
