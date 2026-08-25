---
name: traits-build-add-dataset
description: Use when adding a new dataset to a traits.build database repo (austraits.build, ausinvertraits.build, AusFizz, or similar) -- the user hands over one or more data files plus a source (a paper/report PDF, a DOI, or a book/website citation) and wants it turned into a `data/<dataset_id>/` folder with `data.csv` and `metadata.yml`, tested, built, and reported. Triggers on requests like "add this dataset", "import this data into AusTraits", "add a new dataset_id", or a curator handing over data + a reference and asking for it to be added to the database.
---

# Adding a dataset to a traits.build database

**The skill builds the infrastructure; the curator makes the decisions.**
This skill creates the folder, writes a `data.csv` as close as possible to
the file the contributor sent, scaffolds `metadata.yml` via the same
`user_responses`-driven helpers a curator would drive by hand, prepares the
review files for GATE 1, and runs the whole test/build/report loop. It does
**not** decide which trait concept a column maps onto, what a categorical
value should be replaced with, or what a taxon name should resolve to.
Those cells arrive empty, on purpose, and a human fills them at the gates
below.

Read this file's non-negotiables before starting. They are checked in the
gates, but the cost of finding a violation at a gate instead of before it is
redoing the phase.

## Before Phase 0: confirm the repo

Run `scripts/probe_repo.R` from the repo root:

```
Rscript inst/skills/traits-build-add-dataset/scripts/probe_repo.R .
```

This is not optional and not a formality: `ausinvertraits.build` and
`AusFizz` do not carry the same `R/custom_R_code.R` helper functions, taxon
alignment files, or `APCalign` availability that `austraits.build` does. Every
later phase that reaches for a host-repo helper (de-duplication functions,
`build_align_taxon_names()`, `check_new_taxa()`) must check the probe's
report first and fall back to the package-level equivalent named in that
report, rather than assuming `austraits.build`'s toolset is universal.

If the probe reports this isn't a database repo (no `data/`, no
`config/traits.yml`), stop -- don't guess at a repo root.

## Phase 0 -- Intake

Collect: the data file(s), the source PDF/citation, a DOI if there is one.

Derive `dataset_id` as `Surname_year`. Check `data/` for a collision; if
`Surname_year` already exists, suffix `_2`, `_3`, ... For a compilation with
multiple original sources bundled under one contributed dataset, see
`references/metadata-fields.md` §1 for the `original_01`/`original_02` `source`
pattern (worked example: `Richards_2008`).

Create `data/<dataset_id>/`. Record the incoming filename for
`dataset.original_file`.

**Do not create `raw/` yet.** Whether it's needed isn't knowable until Phase
2 has established how far `data.csv` had to depart from the source -- see
Phase 2a. Creating it speculatively and leaving it empty is itself a thing to
avoid; the absence of `raw/` is meaningful (see the non-negotiable below).

## Phase 1 -- Read the source

Extract into a scratch notes file, each item carrying its page/section
anchor: citation details, `sampling_strategy` (**verbatim**), per-trait
`methods` (**verbatim**), locations, treatments/contexts, collection dates,
life stage, basis of record.

**`sampling_strategy` and per-trait `methods` come from different parts of
the paper's Methods section, not the same block copied twice.**
`sampling_strategy` is site selection and overall study design (where, how
many, why those sites/individuals) — dataset-level, applies across every
trait. Each trait's `methods` is trimmed to just the sentences describing how
*that* trait was measured — instrument, protocol, timing — not the whole
Methods section pasted into every trait entry. A short passage can
legitimately appear in both, but if `sampling_strategy` and every trait's
`methods` end up holding the same copy-pasted block, that's a sign the source
wasn't re-read per field — see `references/metadata-fields.md`'s scoping
note.

**One exception to verbatim-only:** `dataset.description` is a *written*
one-sentence summary of what the study did -- it can't be lifted cleanly
from an abstract. Compose it, and flag at GATE 1 that it's generated prose,
unlike every other text field here.

Anything not found in the source stays `.na` and becomes an entry under
`questions:` for the contributor (see `references/metadata-fields.md` §7).
Nothing about methods or sampling strategy is paraphrased or invented --
`.na` plus a question, or verbatim, never a third option.

If extracting from a PDF, sanitise confusable characters as you go --
see "Character handling" below and `references/troubleshooting.md`'s
confusables table before they ever reach a note or a metadata field.

## Phase 2 -- Build `data.csv`, as close to the source as possible

**The goal is a traceable line back to the file the contributor sent.**
Do not reshape, rename, recode, filter, or summarise in `data.csv` if the
same effect can be achieved by `custom_R_code`, a substitution, or a trait
mapping -- all three are declarative and reviewable, and all three leave
`data.csv` matching the source. `references/custom-r-code.md` has the eleven
worked patterns for the manipulations a curator might reflexively apply to
the CSV instead; check there first.

The **whole list** of legitimate reasons to depart from the provided file:

- it isn't a readable CSV as-is (`.xlsx`/`.xls`/`.ods` with sheets, multi-row
  or merged headers, a metadata block above the data);
- non-UTF-8 or otherwise unreadable characters that block reading at all
  (distinct from characters that are readable but disallowed -- see
  "Character handling" below, which is a `metadata.yml`/`data.csv` fix, not a
  reason to touch the source file);
- **multiple files that must be joined**, because `custom_R_code` cannot read
  files -- species-code keys, per-trait spreadsheets, separate location
  tables all fall here.

Everything else -- missing-value codes, unit-bearing column headers,
categorical spellings, derived columns, de-duplication, date reformatting --
stays exactly as supplied and is handled declaratively in `metadata.yml`.

## Phase 2a -- Archive `raw/` only if it holds something `data.csv` doesn't

If `data.csv` is essentially the file the contributor sent, the archive would
be a byte-for-byte duplicate: **no `raw/` folder is created.** This follows
directly from Phase 2 -- the better that phase goes, the less there is to
archive.

Keep `raw/` when the source genuinely differs:

- `.xlsx`/`.xls`/`.ods` -- sheets, formatting, formulae, any sheet not
  carried into `data.csv`;
- multiple files merged into one -- each original, plus a `build_data_csv.R`
  script that performs the join, so the merge is reproducible (146 `.R`
  files already live in `raw/` folders across the databases for this reason
  -- match the convention);
- a file that needed character/encoding repair -- keep the pre-repair
  original;
- supplementary material not otherwise retrievable -- an unpublished report,
  a data dictionary, a location table supplied separately.

**The published paper itself is not archived** -- it's identified by DOI in
`source`. Archive a document only when it's the dataset's own supporting
material, not the literature (only 5 of 411 `austraits.build` datasets keep
a PDF).

## Phase 3 -- Locations

Four places to look, in order -- take the first that has them:

1. **Columns in the main data file** -- `distinct()` them out, feed to
   `metadata_add_locations()`. See `references/curator-recipes.md`'s
   "Locations straight off `data.csv`" idiom.
2. **A secondary file supplied with the dataset** -- transcribe into the
   YAML; the file itself is archived in `raw/`.
3. **The source PDF** -- a site table or methods description, transcribed
   with its page anchor.
4. **None of the above** -- leave `.na` and raise it under `questions:`.
   Missing location data is a question for the contributor, not something to
   reconstruct.

Coordinates end up as decimal degrees under `latitude (deg)` /
`longitude (deg)`. DMS in the source (`42° 49.4'S, 147° 30.6'E` is a recurring
form) is converted -- see `references/curator-recipes.md` for the
conversion -- and the original string is kept in the notes so the conversion
is checkable.

**Location properties have no controlled vocabulary, but the list should
stay short and consistent.** Reuse a property name already in
`vocab_index.tsv` (from `scripts/trait_index.R`) wherever one fits. A
genuinely new property is fine, but flag it at GATE 1 as new vocabulary so
the choice is deliberate.

## Phase 4 -- Scaffold `metadata.yml`

First, run `scripts/trait_index.R` against the repo to get `traits_index.tsv`,
`units_index.tsv`, and `vocab_index.tsv` in a scratch directory -- this is
what lets a curator be *shown* plausible trait concepts and existing
context/location properties without reading `config/traits.yml` (700+ KB,
500+ concepts in `austraits.build`) into context:

```
Rscript inst/skills/traits-build-add-dataset/scripts/trait_index.R . <scratch_dir>
```

Then write an answers file (see the worked example and format notes in
`scripts/scaffold_metadata.R`'s header, and the field-by-field reference in
`references/metadata-fields.md`) and run:

```
Rscript inst/skills/traits-build-add-dataset/scripts/scaffold_metadata.R <answers.yml>
```

from the repo root. This drives `metadata_create_template()`,
`metadata_add_source_doi()` (or a hand-built `source` block for non-article
types), `metadata_add_traits()`, and `metadata_add_locations()` --
`user_responses`-driven, not hand-authored YAML -- so `metadata.yml` ends up
in exactly the shape the interactive path would produce.

**Gotcha, worth internalising before writing an answers file:** bare `NA` in
YAML parses to the *string* `"NA"`, not R's real `NA` -- only `.na` does
that. `scaffold_metadata.R` checks for and rejects the literal string `"NA"`
in template/location answers, but write `.na` from the start. See
`references/metadata-fields.md`'s gotcha callout.

Then fill in what's knowable by hand: `contributors`, `methods` (verbatim),
`description` (the written sentence, flagged as such), `replicates`,
`unit_in` as given in the source.

`trait_name` is left `.na`. So is `replace` in every substitution, and
`context_property` after contexts are added. Those are the curator's, and
they are the subject of GATE 1.

## GATE 1 -- mapping review

**Stop.** Write review files to a **scratch directory**, never
`data/<id>/raw/` -- under the Phase 2a policy that folder may legitimately
not exist, and creating it just to hold working CSVs defeats the point of
that policy. (The house practice this generalises did write them to
`raw/`; the scratch directory is the same workflow without the side effect
on the repo. If a particular dataset's decisions are worth preserving
afterward, that's a deliberate copy at the end, not the default.)

| File | Pre-filled by the skill | Left empty for the curator |
|---|---|---|
| `trait_mapping.csv` | `var_in`, `unit_in` as given, sample values, min/max, n, and shortlisted `candidates` from `traits_index.tsv` in their own column | `trait_name`, `entity_type`, `value_type`, `basis_of_value` |
| `substitutions.csv` | `trait_name`, `find` | `replace` |
| `contexts.csv` | `var_in`, distinct values found, a suggested `category` | `context_property`, `value`, `description` |
| `locations.csv` | names, coordinates, properties as found, new-vocabulary properties marked | any correction |

One file per concern, edited in place -- the curator fills the blank column
and saves; read the same file back. No `_needed`/`_added` pair of files.

The `candidates` column in `trait_mapping.csv` is a shortlist from
`traits_index.tsv`, never a filled-in answer -- it exists because no one can
eyeball 500+ concepts, and it's fine to drop if a particular curator finds it
anchors them on a wrong suggestion rather than helping.

Alongside the files, report in chat: which `unit_in` values have no
conversion in `units_index.tsv`; which location/context properties are new
vocabulary; the generated `description` sentence, for approval; the list of
things the source didn't answer, as draft contributor questions.

**Nothing proceeds until the files come back.**

## Phase 5 -- Test loop

1. Round-trip formatting: `write_metadata(read_metadata("data/<id>/metadata.yml"), "data/<id>/metadata.yml")`
   -- the documented "clear formatting" step for a *new* file. (Never do this
   to a one-character fix in an existing file elsewhere in the database --
   it buries the change in an unreviewable reflow. That rule is about editing
   other datasets, not this one.)
2. `metadata_check_custom_R_code(dataset_id)` -- confirms `custom_R_code` runs
   and produces what's expected, before the full test.
3. `dataset_test(dataset_id)` -- runs per-dataset without a full build; this
   is the fast inner loop. Fix and repeat until `[ FAIL 0 | WARN 0 ]`.

When it fails, `references/troubleshooting.md` has the exact `stop()` and
testthat-failure strings this package produces, organised for lookup by
symptom.

## Phase 6 -- Build this dataset, and the second round of substitutions

```r
build_setup_pipeline(method = "remake", database_name = "<db>")
tmp <- remake::make(current_study)   # builds just this dataset -- far faster than a full rebuild
tmp$excluded_data
```

**Substitutions**, appending to the same `substitutions.csv` the curator has
already seen, `replace` again left blank -- see
`references/curator-recipes.md`'s substitutions round-trip for the exact
idiom:

```r
database$excluded_data %>%
  filter(dataset_id == current_study, error != "Observation excluded in metadata") %>%
  distinct(trait_name, value) %>% arrange(trait_name, value) %>%
  rename(find = value) %>% mutate(replace = NA_character_) %>%
  write_csv("<scratch>/substitutions.csv")
# curator fills `replace` in place, then:
metadata_add_substitutions_list(current_study, read_csv("<scratch>/substitutions.csv"))
```

**Taxonomy** -- only where `probe_repo.R` reports `APCalign` available (and,
for the strongest signal, `austraits.build`'s own alignment helpers); use
the canonical filter from `references/curator-recipes.md`'s taxonomy
round-trip:

```r
resources <- APCalign::load_taxonomic_resources()
checked <- align_taxa(taxa$taxon_name, resources = resources, identifier = current_study) %>%
  arrange(alignment_code, original_name) %>%
  filter(original_name != cleaned_name, !str_detect(original_name, "\\["))
```

APCalign's proposed `aligned_name` is written to `taxonomic_updates.csv` for
confirmation, never applied directly -- it's a machine alignment, and the
curator confirms it before `metadata_add_taxonomic_changes_list()` runs. The
`!str_detect(original_name, "\\[")` filter drops names already carrying a
`dataset_id` in square brackets -- those are already aligned and must not be
re-aligned; list them at the gate as "already aligned", not silently
dropped.

## GATE 2 -- excluded data and any config change

**Stop** and present: values still needing substitutions; out-of-range
numerics (a long list means a units error, not real outliers --
`references/troubleshooting.md` has the full `excluded_data` catalogue);
taxon names not aligning; anything blocking the pivot
(`check_pivot_duplicates()`, `dataset_check_not_pivoting()` from
`scripts/dataset_checks.R`).

**Any change to `config/traits.yml`, `config/unit_conversions.csv`, or
`config/taxon_list.csv` is database-wide and is proposed here, never made
unilaterally** -- a new trait concept, a new unit conversion, a widened
allowable range, or new taxon-list rows all need explicit approval. Where
`probe_repo.R` reports `build_update_taxon_list()` is available, propose
running it rather than hand-editing the taxon list.

## Phase 7 -- Report

Full database rebuild, then:

```r
dataset_report(current_study, database, overwrite = TRUE)   # dataset_id FIRST, database SECOND
```

`adding_data_long.qmd:1332` shows the arguments reversed -- that's a bug in
the book, not in the function.

Then the "what did this actually add" check -- what a curator most wants to
see. Prefer `new_taxa_trait_combinations()` from `traits.build`'s
`R/reports.R` (exported): it reads `database$taxa` rather than taking
APCalign `resources`, so it works in all three database repos without
needing APCalign. Fall back to the host repo's `check_new_taxa()` /
`check_new_taxa_accepted()` only where `probe_repo.R` finds them and a
repo-specific view is wanted.

Always pass `partial_matches_allowed = FALSE` to `extract_dataset()` --
without it, `extract_dataset("Bryant_2021")` also pulls in `Bryant_2021_2`
and `Bryant_2021_3`, silently inflating every check that follows. A
compilation split into `_2`/`_3` suffixes reports as one `dataset_report()`
call with a vector of ids, not one call per id.

## GATE 3 -- report review

**Stop** on: do the locations plot sensibly on the map; do numeric traits
sit sensibly against other studies already in the database; is the
unmatched-species list free of names that should have aligned; is every
metadata field populated (no stray `unknown` left over from the template
scaffold).

## Phase 8 -- Finalise

1. `dataset_replace_disallowed_chars(dataset_id)` -- re-run with
   `dry_run = TRUE` first and read the report before applying; see
   "Character handling" below.
2. Re-clear formatting (the same round-trip as Phase 5, now that the file is
   final).
3. Re-run `dataset_test(dataset_id)`.
4. Confirm the `questions:` block holds the contributor questions accumulated
   across every phase (Phase 1's unanswered items, Phase 3's missing
   locations, Phase 4's `additional_traits`, GATE 1's flagged new
   vocabulary).
5. Hand back a summary and a suggested branch/PR. Default branch is
   `develop`; see `AGENTS.md` for the commit-message convention (PR title +
   body become the permanent commit message -- keep the subject short).

---

## Non-negotiables

Stated once, in full, here -- referenced by short name from every phase
above.

- **Decision cells stay empty.** `trait_name`, substitution `replace`, and
  `context_property` are the curator's. The skill fills what the *source*
  says -- `var_in`, `unit_in` as given, `find`, sample values -- never what
  the *database* should say. A `candidates` shortlist is fine and wanted; a
  filled-in `trait_name` is not.
- **`data.csv` matches the file the contributor sent** unless it can't be
  read as a CSV, has characters/encoding that block reading, or requires a
  multi-file join. Everything else is declarative, in `metadata.yml` or
  `custom_R_code` (`references/custom-r-code.md`).
- **Never fill a `trait_name` absent from `config/traits.yml`.** Leave
  `.na`, record the column under `questions: additional_traits:`.
- **Never write `individual_id: unknown`.** It silently assigns the whole
  dataset to one individual. Omit the field instead of filling it with a
  placeholder.
- **Always pass `partial_matches_allowed = FALSE` to `extract_dataset()`.**
- **Never edit `config/*.csv`/`config/*.yml` without approval at GATE 2** --
  those files are database-wide.
- **Reuse existing location and context property names** wherever one fits;
  a new one is allowed but must be flagged at the relevant gate.
- **Never invent or paraphrase `methods`/`sampling_strategy`** -- verbatim
  from the source, or `.na` plus a `questions:` entry. `description` is the
  one field that's composed, and it's flagged as such at GATE 1.
- **`sampling_strategy` and per-trait `methods` are different slices of the
  Methods section, not the same text twice.** `sampling_strategy` is site
  selection/study design (dataset-level); each trait's `methods` is trimmed to
  only what describes measuring *that* trait. Short overlap is fine; wholesale
  duplication across every field means the source wasn't re-split per field.
- **Round-tripping `metadata.yml` (`read_metadata` |> `write_metadata`) is
  correct for a new dataset** (Phase 5/8's "clear formatting" step) but wrong
  for a one-character fix to an *existing* dataset elsewhere in the database
  -- it buries the change in an unreviewable reflow.

### Character handling

- **Degree symbol is U+00B0 (`°`), not U+00BA (`º`).** The commonest
  disallowed character across the databases, and the book itself has this
  exact typo. PDF-extracted text needs sanitising for confusables --
  `º`→`°`, `∼`→`~`/kept as-is, `◦`→`°`, assorted non-breaking/zero-width
  spaces → plain space or removed, and the `¬†` mojibake (a UTF-8 NBSP
  re-decoded as Mac OS Roman) -- replace longest-key-first, or a
  per-character pass reproduces the mangling. Full table:
  `references/troubleshooting.md`.
- **Author names and real symbols are never rewritten** -- `ñ`, `š`, `‰`,
  `Ó`, `′`, `″` are meaningful, not errors. Letters are never in the
  replacement map for exactly this reason.
- **Trait *values* in `data.csv` must be pure ASCII.** `metadata.yml` allows
  a wider set (accented letters, `°`, en/em dash, curly quotes, and more --
  see `references/troubleshooting.md`). These are different rules for
  different files -- don't conflate them.

### Units

- **Follow UCUM.** `/` separates numerator/denominator; curly braces
  annotate without changing the conversion (`umol{CO2}/m2/s`).
- **Quote a unit starting with punctuation:** `unit_in: '{count}/mm2'`, not
  bare `{count}/mm2`.
- **Use `neg_MPa`, never `-MPa`** -- a leading `-` reads as a sign, not part
  of the unit string.

## Reference files

- `references/metadata-fields.md` -- every `metadata.yml` field, the
  `source` shapes per bibtype, the controlled vocabularies in full (not the
  abbreviated lists in this file or the book).
- `references/custom-r-code.md` -- the eleven `custom_R_code` patterns; check
  here before touching `data.csv` for anything reshaping/recoding/deriving.
- `references/curator-recipes.md` -- the working idioms behind Phases 3, 6,
  and 7 (load/build, substitutions and taxonomy round-trips, locations,
  DMS conversion, the test/report loop) -- these exist nowhere else in
  writing.
- `references/troubleshooting.md` -- the full `excluded_data` error
  catalogue, encoding/character issues, and a symptom-indexed lookup for
  `dataset_test()` failures.

## Scripts

- `scripts/probe_repo.R` -- run first; reports host-repo capabilities.
- `scripts/trait_index.R` -- compact indices of `config/traits.yml`,
  `config/unit_conversions.csv`, and in-use context/location properties.
- `scripts/scaffold_metadata.R` -- drives `metadata_create_template()` and
  friends from an answers YAML file.
- `scripts/dataset_checks.R` -- the book's `dataset_check_*()` functions,
  source this after building, for GATE 2.
