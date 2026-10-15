# traits.build — streamlining review & plan

*Review dated 2026-07-27. Tracked by the epic issue linked from this file's PR.*

> ## Corrections, 2026-07-28
>
> Stage 0 is done (#230), and executing it proved six statements below wrong. They are corrected
> here rather than edited away, because *why* the review was wrong is the more useful record.
> Everything else held up.
>
> 1. **`fix/rdevel-check-identifiers-citation` was not unPR'd.** It was raised as #217 and merged
>    into **`master`, not `develop`**, on 8 July. So were #214 and #215. Nothing enforces the base
>    branch on a PR, and this cost three weeks of `develop` carrying WARNINGs that were already
>    fixed. Ported by #229 and #231.
> 2. **The identifiers feature is broken in four ways, not three.** `write_plaintext()` — the
>    public exporter — had a hardcoded table list that omitted `identifiers`, so every export
>    silently dropped 2.1.0's headline table for the whole release. The function had no test.
>    Fixed in #230.
> 3. **There are three Rd `\usage` mismatches, not two.** `process_parse_data()` gained an
>    `identifiers` parameter whose roxygen was never updated. With it fixed, a fresh-clone
>    `R CMD check` reports **0 errors, 0 warnings, 3 notes**, so `error-on: "warning"` can be
>    turned on now rather than in Stage 2.
> 4. **The `expect_snapshot_value()` migration should not happen.** Its premise — that
>    `expect_equal` "dumps hundreds of rows for a one-cell change" — was true under testthat
>    edition 2 but not edition 3, which is already active. Measured: waldo gives a 15-line diff
>    naming the exact cell in the 527×26 traits table. The regeneration benefit is provided
>    instead by `tests/testthat/regenerate-examples.R`, which goes through `write_plaintext()` and
>    so covers the exporter too. Snapshots are now used where they do fit: `dataset_test`'s report.
> 5. **The `R/pivot.R` detached-roxygen hazard is already gone.** "Fix before anyone runs
>    `document()`" no longer applies — roxygen2 8.0.0, adopted in #228, associates the block across
>    the blank line. Verified by deleting `man/check_pivot_wider.Rd` and re-documenting: it comes
>    back identically.
> 6. **The real edition-3 gap is `local_edition(2)` in `dataset_test_worker()`**
>    (`R/testdata.R:60`), not the two deprecated `context()` calls. Those are a symptom. See #234.
>
> Two things the review predicted were understated rather than wrong. The unasserted
> `identifiers.csv` fixtures were not merely stale, they were **wrong**: Test_2023_1 maps
> observation 235 to `Plant 66` where the verified `traits.csv` puts it on taxon *Syzygium*, the
> `Plant 73` row. And Test_2023_2's 506-row fixture could not be reproduced from committed inputs
> at all, because the column it read from was never committed. Follow-ups from Stage 0 are
> #232–#236.

A structured review of the package against four goals — **easy to use, well documented, easily
maintained, and stable**. Every claim below was verified against the code, the three downstream
database repos (`austraits.build`, `ausinvertraits.build`, `AusFizz`), the published paper
(Wenk et al. 2024), and the CRAN package index at the time of writing. Line numbers refer to
`develop` as of that date and will drift.

Decisions already taken by the maintainers are marked *(decided)* inline; the Open Questions section
at the end lists what remains.

## Context

`traits.build` is the generic workflow engine spun out of AusTraits in 2023. The goal is to make it
**easy to use, well documented, easily maintained — and stable**, because other repositories depend
on it.

Maintainer decisions shaping this plan:

- the README's **"Lifecycle: deprecated" badge is a mistake** — the package is active;
- **CRAN submission is a real goal**;
- there is appetite for a larger restructure, and **long-term design quality is preferred over
  backwards compatibility** where the two conflict;
- **`austraits.build`, `AusFizz` and `ausinvertraits.build` must continue to build.**

Those last two are compatible, not contradictory, because the three downstream repos are *in the
family*. "Stability" here means **no silent breakage** — not frozen APIs. Breaking changes are
acceptable when they are deliberate, versioned, and the family repos are migrated in the same
release. What is not acceptable is a change that quietly alters built output or breaks a database
nobody noticed until a rebuild. That distinction drives the plan: aggressive on design, strict on
verification.

Development cadence is declining (98 commits in 2023 → 27 in 2024 → 15 in 2025) across three main
contributors. So the plan is ordered by *leverage per hour*, and prefers changes that reduce the
cost of every future change over changes that are merely tidy.

### The stability constraint is sharper than it looks

Those three repos contain **601 datasets**, and their `metadata.yml` files carry `custom_R_code`
snippets that traits.build evaluates via `eval(parse(text = ...), new.env())` (`R/process.R:415`).
Because `new.env()` chains to the search path, those snippets resolve unqualified function names
through **attached** packages. Counting them:

| Repo | Datasets | Unqualified `mutate()`/`filter()`/`str_*()` calls | Qualified |
|---|---|---|---|
| `austraits.build` | 411 | 970 | 427 |
| `ausinvertraits.build` | 160 | 170 | 0 |
| `AusFizz` | 30 | 100 | 0 |

**1,240 call sites work only because `Depends:` attaches dplyr/tidyr/stringr.** This kills the
conventional "move `Depends` → `Imports`" advice: doing so would break every one of them. See
Priority 1, item 5 — the right move is to *document* `Depends` as a deliberate contract, not remove
it. Fortunately CRAN treats a heavy `Depends` as a style smell, not a blocker, so this costs nothing
on the CRAN path.

---

## Where to start: no, the tests do not cover the whole output structure — and the gap predicts the bugs

This is the right place to begin, and the evidence is unusually clean.

`test-xamples.R` builds nine example datasets and compares each against committed golden files. That
is a genuinely strong regression net — but it compares **8 tables**:

```r
tables <- c("traits", "locations", "contexts", "methods", "excluded_data",
            "taxonomic_updates", "taxa", "contributors")
```

Each `examples/Test_2023_*/output/` directory contains **twelve** artefacts. So the following are
generated and committed but **never asserted against**:

| Artefact | Compared? |
|---|---|
| `identifiers.csv` | **only for `Test_2023_8`** — 1 of 9 datasets |
| `sources.bib` | never |
| `definitions.yml` | never |
| `metadata.yml` (output) | never |
| `build_info` / `schema` | never |
| `Test_2023_9` (whole dataset) | commented out (`test-xamples.R:315-333`) |

**`Test_2023_1` has a 209 KB `identifiers.csv` golden file that nothing compares.** It is also the
dataset with two identifiers — precisely the multi-identifier case that crashes `dataset_test()`.

That single gap explains the entire Priority 0 section below. Three independent identifiers bugs
shipped in the headline 2.1.0 feature because: the identifiers table is unasserted for 8 of 9
datasets; `metadata_add_identifiers()` is the only `metadata_add_*` function with no test at all; and
CI was configured to ignore the check NOTEs that named two of the three faults.

**So yes — start here.** Closing this gap is cheap (add the missing names to the `tables` vector,
add comparisons for the three non-CSV artefacts, re-enable `Test_2023_9`), and it is the
precondition for everything else: the golden files are what make the larger restructure tractable at
all. Extending coverage *before* refactoring means the safety net exists before it is needed.

### The examples are also not representative of what the real databases use

Comparing the 9 `Test_2023_*` examples against the 601 real datasets in `austraits.build`,
`ausinvertraits.build` and `AusFizz`, every controlled vocabulary has values that ship in production
but are **never exercised by any test**:

| Field | Used in real data but not in any example |
|---|---|
| `taxonomic_resolution` | **13 of 14 ranks** — examples only ever use `species`. Real data uses `genus`, `family`, `order`, `class`, `variety`, `form`, `subspecies`, plus the invertebrate ranks `subfamily`, `suborder`, `subgenus`, `superfamily`, `supertribe`, `tribe` |
| `basis_of_record` | `captive_cultivated`, `lab`, `preserved_specimen`, `unknown` |
| `life_stage` | `sapling`, `seedling`, `unknown` |
| `entity_type` | `metapopulation`, `unknown` |
| `value_type` | `median` (and `standard_error`/`standard_deviation`, added in the 2.1.0 schema) |
| `basis_of_value` | `unknown` |

The `taxonomic_resolution` gap is the one to worry about, because
`R/process.R:1966` hardcodes a **botanical** rank vocabulary:

```r
name_to_match_to = ifelse(!.data$taxon_rank %in% c("species", "subspecies", "series", "variety", "form"), ...
```

None of the invertebrate ranks `ausinvertraits.build` actually uses appears in that list, and no
test would catch it if the branch were wrong. Given the paper's claim that the workflow is
generalised beyond plants, this is worth an explicit test per rank.

Two smaller findings from the same census:

- **`identifier_type` is not validated.** `metadata_add_identifiers()` offers a DwC-derived list
  (`catalogNumber`, `collectionID`, `institutionCode`, `institutionID`, `materialSampleID`,
  `occurrenceID`) at `R/setup.R:611-612`, but the examples contain `personal` and `specimentID` —
  neither is in that list, and **`specimentID` is a typo** for `specimenID`. Nothing rejects them.
- Only 5 of 9 examples declare `identifiers` at all, versus 41 of 601 real datasets — thin, but
  proportionate.

**Recommendation:** rather than inventing new example datasets, add the missing vocabulary values to
the *existing* ones (a `taxonomic_resolution` per rank is the priority), so coverage grows without
adding fixtures to maintain. This is the natural companion to closing the output-artefact gap above.

Two caveats to handle while doing it:

- **Expect the newly-compared files to fail immediately.** The identifiers pipeline is broken, so
  the existing `identifiers.csv` fixtures may encode buggy output. They must be regenerated and
  reviewed, not blessed.
- **Fix the diff ergonomics at the same time.** ~~`expect_equal(built[[v]], expected[[v]])` on a
  whole tibble dumps hundreds of rows for a one-cell change... moving to `expect_snapshot_value()`
  gives readable diffs and a one-command `snapshot_accept()` regeneration path.~~
  **Superseded — see correction 4.** The premise was edition-2 behaviour; under edition 3, already
  active, `expect_equal` routes through waldo and gives a 15-line diff naming the exact cell. What
  was actually missing was the regeneration path, now `tests/testthat/regenerate-examples.R`, which
  additionally exercises `write_plaintext()`. The real point stands: nothing should be hand-edited
  to match observed output.

---

## Headline finding: CRAN submission is currently blocked

This is not a matter of polish — it is structural.

| Blocker | Evidence | Resolution (decided) |
|---|---|---|
| `austraits` is in `Depends` and **is not on CRAN** | verified against CRAN index | **Option A** — move the helpers back |
| `remake` is in `Suggests` and **is not on CRAN** | verified against CRAN index | **guard the 6 test calls now** (unblocks CRAN); **drop remake** once partners migrate |
| `Remotes:` — **forbidden on CRAN** | `DESCRIPTION:61-63` | falls away once both above are done |
| `LICENCE` grants **two different licences** | CC-BY section, then BSD-2 section | **BSD-2** — data now lives in `austraits.build` |
| `ontology/` (11 MB) ships in the tarball | no `.Rbuildignore` rule | add `^ontology$` — over CRAN's 5 MB limit |

Both non-CRAN dependencies are now decided, so **the CRAN path is unblocked in principle** — what
remains is execution. Note the heavy `Depends:` is deliberately *not* on this list: it is a style
smell CRAN tolerates, and per the stability constraint above it must stay.

### The `austraits` dependency is also backwards

In the *data* pipeline traits.build is upstream of austraits; in the *package* graph the edge runs
`traits.build → austraits`. It exists for five small helpers, 45 call sites:

| Function | Calls |
|---|---|
| `convert_list_to_df2` | 31 |
| `convert_df_to_list` | 5 |
| `convert_list_to_df1` | 4 |
| `flatten_database` | 2 |
| `bind_databases` | 2 |

These are data-structure utilities for the *traits.build schema*. NEWS 2.0.0 records that they were
deliberately moved out to `austraits`; that move is what created both the reversed edge and the CRAN
blocker.

**Decision: Option A** — move the conversion helpers back into `traits.build`; `austraits` depends on
`traits.build`, not the reverse.

This is smaller than the 45-call figure suggests. ~90% of the coupling is 47 calls to
`convert_list_to_df1`/`df2`, which together are **17 lines** of trivial
`tibble()`/`bind_rows(lapply(...))` construction — and which *used to live here*, as the deprecated
`util_list_to_df1`/`util_list_to_df2` shims at `R/utils.R:294,316` attest. Restoring them as
internals removes 52 of 59 call sites.

The remainder:

- `flatten_database` — reached only from a dead alias and a deprecated shim; drop both.
- `bind_databases` — in real code appears only in the generated pipeline templates
  (`build_base.whisker:23`, `build_furrr.whisker:39`). An `austraits::` call is legitimate *there*,
  because the user's own pipeline may reasonably depend on `austraits` even when the package does
  not.
- `plot_trait_distribution_beeswarm` — only in `report_dataset.Rmd`, the most AusTraits-specific
  artefact in the repo; handle with the template rewrite.

End state: **`austraits` moves from `Depends` to `Suggests`**, guarded by the same
`util_require_package()` helper introduced for `rcrossref` in PR #219. Because `austraits` is only
*attached* today as a side effect of `Depends`, and no downstream `custom_R_code` calls its functions
unqualified, this specific move does not endanger the three downstream builds — but that must be
verified, not assumed (see Verification).

### Retire `remake` in favour of `targets`

**Goal: drop `remake`.** It is unmaintained, GitHub-only, and the reason for
`Remotes: richfitz/remake`. The constraint is timing, not desirability — database partners currently
build with it, so they have to be migrated before the method is removed:

| Repo | Pipeline |
|---|---|
| `austraits.build` (411 datasets) | plain `build.R` — no remake |
| `ausinvertraits.build` (160) | `remake.yml` |
| `AusFizz` (30) | `remake.yml`, explicit `method = "remake"` |
| `traits.build-template` | `remake.yml` — the paper's designated starting point |

The good news on inspection: **`traits.build` never calls `remake` at all.** It only *writes* a
`remake.yml` from `inst/support/build_remake.whisker` (`R/setup.R:1302-1304`). Generating a remake
pipeline works whether or not remake is installed — the *user* needs remake, the package does not.
The only `remake::` calls in the entire repo are six in `tests/testthat/test-setup.R` (`:660, :661,
:717, :718, :723, :732`).

**That decoupling is what makes dropping it cheap.** Because the package only emits a text file,
removing remake support is deleting one whisker template, one branch in `build_setup_pipeline()`, and
six test calls — not unpicking a runtime dependency.

Sequence, so partners are never left broken:

1. **Add a `targets` template** as a fourth method. `targets` is the maintained successor to remake
   by the same author, on CRAN, and a natural fit for this pipeline.
2. **Unblock CRAN immediately, before any migration.** Add `skip_if_not_installed("remake")` to the
   six test call sites and remove `Remotes:`. The suite currently has **zero skips**, so on a machine
   without remake the tests *error* rather than skip — that is CRAN's actual objection, and it
   applies equally to `furrr` and `zip`. This step alone means remake's presence stops blocking
   submission, so the CRAN path does not wait on partner timelines.
3. **Migrate the three consumers** — `ausinvertraits.build`, `AusFizz`, and `traits.build-template`
   — to `targets` (or plain `base`, which `austraits.build` already uses successfully for 411
   datasets). Coordinate with partners; this is the long pole.
4. **Then deprecate `method = "remake"`** with a `lifecycle` warning, and **remove it** a release
   later along with the template and the `Suggests` entry.

Steps 1-2 are ours alone and can happen immediately; only steps 3-4 need partner coordination.

---

## Constraint: stay true to the published paper

Wenk et al. 2024 (*Ecological Informatics* 83:102773) is the public specification of the data model
and workflow. Names and structures it documents should be treated as a **contract**: expand
functionality freely, but do not silently rename or remove what the paper describes.

**This constraint is currently being violated.** Of the seven functions named in the paper's Fig. 1
pipeline diagram, three are not usable as published:

| Function in Fig. 1 | Actual state | Decision |
|---|---|---|
| `build_setup_pipeline()` | ✅ exported | — |
| `dataset_configure()` | ✅ exported | — |
| `dataset_process()` | ✅ exported | — |
| `data_update_taxonomy()` | ❌ **does not exist** | **keep `dataset_update_taxonomy()`; add `data_update_taxonomy()` as an alias** so the published name also works |
| `build_combine()` | ⚠️ deprecated, and the shim calls the wrong function (`R/utils.R:359-362`) | **current design preferred** — keep the deprecation toward `bind_databases()`, but *fix the shim*, which today calls `convert_df_to_list()` and passes its arguments twice |
| `database_create_combined_table` | ❌ **not exported** — assigned at `R/utils.R:372`, otherwise dead | export it, since the paper presents it as the route to the combined table |

So a reader who follows the published figure cannot run the documented pipeline. Reconciling this is
cheap — one alias, one shim fix, one export — and should happen before any other API work.

Note the interaction with Option A: once `austraits` moves to `Suggests`, both `build_combine()` and
`database_create_combined_table` need `util_require_package("austraits", ...)` guards rather than
bare `austraits::` calls, exactly as done for `rcrossref` in PR #219.

**Consequence for this plan:** the naming-consistency cleanup in Priority 3 must be *additive* —
introduce consistent names as aliases and deprecate gently with `lifecycle`, never rename in place.
Paper requirements 17 ("code is generalised into package format, so that it can easily be reused")
and 18 ("extensive tests and automated testing to ensure it behaves as described") are the authors'
own stated goals, and directly justify the de-AusTraits-ing and testing work below.

Note also that the paper (§3.2) designates
[traits.build-template](https://github.com/traitecoevo/traits.build-template) as the starting point
for a new database. Any in-package scaffolding helper should complement that repo, not compete with
it.

---

## Priority 0 — The identifiers feature is broken in three independent ways

The identifiers table is the headline feature of the 2.1.0 release notes. It does not work, and
**R CMD check has been reporting two of the three faults all along**:

| Fault | Location | Effect |
|---|---|---|
| `institution_code = .na` — `.na` is not defined anywhere in the package | `R/setup.R:624` | `metadata_add_identifiers()` throws `object '.na' not found` as soon as the user picks a column |
| `process_format_identifiers()` refers to `schema`, but its parameter is named `traits` and callers pass `data` | `R/process.R:1677,1690` | `dataset_test()` reports `object 'schema' not found` for every dataset with identifiers |
| `if (!is.na(metadata$identifiers))` on a list of >1 identifier | `R/testdata.R:227,238` | `dataset_test()` aborts entirely: `the condition has length > 1` |

Check output on `develop` already contains:

```
metadata_add_identifiers: no visible binding for global variable '.na'   (R/setup.R:620-625)
process_format_identifiers: no visible binding for global variable 'schema' (R/process.R:1688-1692)
```

`metadata_add_identifiers()` is also **the only `metadata_add_*` function with no test** — precisely
because it is the only one lacking a `user_responses` hook to test through.

This reframes Priority 1: clearing the check is not cosmetic hygiene, it is **turning a bug detector
back on**. Two shipped crashes were sitting in its output.

Fixes 2 and 3 are already written (`fix/rdevel-check-identifiers-citation` and PR #222). Fix 1 (`.na`
→ `NA_character_`) plus a `user_responses` hook and a test are new work, and small.

---

## Priority 1 — Truth in advertising, and the CRAN path

These are small, and two of them are actively harmful today.

1. **Fix the lifecycle badge** (`README.md:7`). It currently renders
   *"Lifecycle: deprecated"* on the landing page of an actively maintained package, telling every
   visitor not to adopt it. Change to `stable`. **This is the single highest-value one-line change in
   the repo.**
2. **Settle the licence as BSD-2** *(decided)*. `LICENCE` currently grants two licences in
   sequence — a `# Software` section stating CC-BY, then a `# Software licenced under the "BSD
   2-clause license"` section with the full BSD text. The CC-BY paragraph dates from when data lived
   alongside the code; the data now lives in `austraits.build`, so the package is unambiguously
   BSD-2. **Delete the CC-BY paragraph** and replace the file with the two-line DCF stub CRAN
   expects (`YEAR`, `COPYRIGHT HOLDER`), which also clears the *"License stub is invalid DCF"* NOTE.
3. **Clear the 2 WARNINGs and 4 NOTEs.** Detail in the R CMD check table below.
4. **Execute Option A** for `austraits`; **guard the `remake`/`furrr`/`zip` test calls** with
   `skip_if_not_installed()`, then remove `Remotes:`. Dropping `remake` itself follows once the
   partner repos are migrated — it is not a prerequisite for submission.
5. **`Depends` — decouple it properly rather than either keeping or naively removing it.**
   - Immediate, safe: **delete `base`** (a no-op), **remove `forcats`** from `Imports` (never used),
     and **move `austraits`** out per Option A.
   - The tidyverse five are the interesting case. The naive fix (move to `Imports`) breaks **1,240
     unqualified calls across 601 downstream datasets**, because `custom_R_code` is evaluated with
     `eval(parse(text = ...), new.env())` (`R/process.R:415`) and resolves names via the *search
     path*. But simply keeping `Depends` forever leaves the package's dependency declaration
     entangled with a user-code contract — which is the actual design flaw.
   - **Long-term fix: make the `custom_R_code` environment explicit.** Have `process_custom_code()`
     build its evaluation environment by populating it from the namespaces user code is entitled to
     use, instead of relying on whatever happens to be attached:

     ```r
     custom_code_env <- function(parent = globalenv()) {
       e <- new.env(parent = parent)
       for (p in c("dplyr", "tidyr", "stringr", "readr", "lubridate")) {
         ns <- asNamespace(p)
         for (nm in getNamespaceExports(p)) assign(nm, get(nm, envir = ns), envir = e)
       }
       e
     }
     ```

     Build it once and cache it. This makes the contract **explicit, testable and independent of the
     user's search path** — `custom_R_code` then behaves identically under `library(traits.build)`,
     `traits.build::`, `Rscript`, and `targets` workers, which it does not today. Once in place,
     `Depends` → `Imports` is safe and the 1,240 call sites keep working.
   - Either way, **document the contract** in `?traits.build` and the book: an undocumented smell
     becomes a stated guarantee about what `custom_R_code` may call.

### R CMD check inventory (current state on `develop`)

| Type | Item | Fix | Status |
|---|---|---|---|
| WARNING | non-ASCII in `R/test_functions.R` | `\uXXXX` escapes | **already fixed** on `fix/rdevel-check-identifiers-citation` |
| WARNING | Rd `\usage` mismatch, `metadata_add_identifiers` + `process_format_identifiers` | sync roxygen | **already fixed** on same branch |
| NOTE | License stub invalid DCF | see item 2 | open |
| NOTE | non-standard top-level dir `ontology` | add to `.Rbuildignore` | open (trivial) |
| NOTE | `forcats` in Imports, never used | remove from `DESCRIPTION` | open (trivial) |
| NOTE | ~11 "no visible binding for global variable" | add `utils::globalVariables()` or `.data$` pronouns | open |

Note the `no visible binding` NOTE includes `process_format_identifiers: no visible binding for
global variable 'schema'` — static analysis independently flagging the real bug fixed in #222's
sibling branch.

---

## Priority 2 — Maintainability: stop the same logic living in two places

**The single worst maintainability risk in the repo — and it has already drifted.** The
location→traits merge is duplicated between the build path (`R/process.R:130-173`) and the
validation path (`R/testdata.R:741-781`). Of 41 lines, 31 are character-identical. The 10 that
differ are live divergences, not cosmetic:

| | `dataset_process` | `dataset_test_worker` |
|---|---|---|
| Guard | `nrow(locations) > 0` | `!is.null(names(metadata$locations))` |
| Final cleanup | drops mapped `location_property` rows | **absent** |
| `locations` built with | `add_error_column = FALSE` | **`TRUE`** — extra column |

The guard divergence is a real behavioural fork: `process_format_locations` returns a 0-row tibble
when `length(unlist(my_list)) == 1`, so a single location with a single property makes the *build*
skip the block while the *validator* enters it and left-joins against a 0-row table, nulling every
`location_id`. **The validator and the builder disagree about the same input.** The comment
*"If process.R changes, this needs to be updated"* is documentation of a defect, not a mitigation —
and issue #60 is the same two paths disagreeing about intended precedence.

**Good news: this is the only such duplication.** A sweep confirmed `dataset_test_worker` correctly
*calls* the shared builder for every other stage (`process_format_identifiers`,
`process_format_locations`, `process_format_contexts`, `process_custom_code`, `process_parse_data`).
The copy-paste exists at exactly one place for a structural reason: **the location merge is the one
build step never extracted into a named function**, so `testdata.R` had nothing to call.

**That makes findings 2a and 2b the same problem.** Extracting
`process_merge_location_properties(traits, locations)` and `process_remap_location_ids()` and calling
them from both sites removes the duplication, the drift, and the first slice of `dataset_process` in
one small change. Decomposition here is not tidying — it is the mechanism by which validation stops
drifting from behaviour.

### `dataset_test_worker` is a ~890-line function

`R/testdata.R:47` to end of file. It is already sectioned by comment banners (`## Identifiers`,
`## Locations`, `## Source`, …), so it decomposes naturally into one function per section, each
taking `(metadata, data, schema, f)` and emitting expectations. Benefits: each check becomes
independently testable, the file becomes navigable, and the duplication above becomes easy to spot.

Also in scope: `dataset_process` (~295 lines) and `process_parse_data` (~277 lines) in `R/process.R`.

**Risk control:** the `examples/Test_2023_*/output/*.csv` golden files plus the new locale test
(#221) pin current behaviour across all nine example datasets, so a decomposition that preserves
output is verifiable. Do this *after* the open PRs land to avoid conflicts.

---

## Priority 3 — Ease of use

**Correctness-adjacent (do first):**

- **No `interactive()` guard exists anywhere in `R/`.** Under `Rscript`/knitr/CI, `readline()`
  returns `""` immediately, so `metadata_user_select_names` (`R/setup.R:201-208`) succeeds with an
  *empty* selection and the caller writes an empty `traits`/`contexts` block with no complaint.
  Scripted runs should fail loudly, not silently produce empty metadata.
- **Interactive and scripted paths disagree.** `metadata_add_contexts`'s interactive branch
  (`R/setup.R:487-519`) asks `description_needed`; the `user_responses` branch (`:525-552`) ignores
  it. The same dataset curated by hand and by script yields different `metadata.yml` — which defeats
  the reproducibility goal stated in `README.md`.
- **`build_setup_pipeline` silently drops datasets** missing `data.csv`/`metadata.yml`
  (`R/setup.R:1265-1273`), reporting only a count. One typo'd folder and the user is never told which
  one vanished. Name them. Likewise, report `getwd()` in the "cannot find data directory" error
  (`:1261`).
- **Destructive overwrites are `message()`, never `warning()`** (`R/setup.R:359,690,817,974`), so
  they vanish under `suppressMessages()` or in a knitr chunk. Note
  `metadata_add_substitutions_list` *replaces all existing substitutions* and, unlike its siblings,
  has no `overwrite` argument.
- **`build_combine()`'s deprecation shim calls the wrong function** (`R/utils.R:359-362`): it points
  users to `bind_databases()` but calls `convert_df_to_list()`, returning nonsense.
- **Documented examples don't run.** `read_yaml`/`write_yaml` are `importFrom`-ed but never
  exported, yet they appear in `_pkgdown.yml`'s user-facing section and in the `@examples` at
  `R/process.R:22,72,365`.

**Structural:**

- **Parameterise hardcoded paths.** Several `metadata_add_*` functions hardcode
  `file.path("data", dataset_id, "data.csv")`; `write_metadata_dataset` (`R/utils.R:253`) has no
  `path_data` although its reader does, so read/write is not round-trippable outside the default
  layout. `dataset_test_worker` hardcodes `"config/"` while *accepting a `path_config` argument it
  half-ignores* (`R/testdata.R:892-896`) — so `dataset_test(path_config = "x")` reads `traits.yml`
  from `x/` for schema checks and from `config/` for the build check.
- **Rename `user_responses` → `responses`** and document it as *the* supported non-interactive
  interface. It is currently labelled "mainly for testing purposes" while being load-bearing for
  reproducible builds — and `metadata_add_identifiers` documents it without implementing it.
- **Naming consistency.** There is currently no rule a user can infer: `_list` means "data frame,
  one dataset" while `_table` means "data frame, many datasets"; the YAML section is
  `taxonomic_updates` but the functions say `taxonomic_change`; `dataset_*` covers build steps, QA,
  *and* a query over a built compilation (`dataset_find_taxon`). Settle a convention and deprecate
  toward it.
- **Shrink the apparent API.** 47 exports but ~88 topics on the reference site, because ~45
  internals carry full man pages without `@keywords internal`. Adding that tag roughly halves the
  surface a new user thinks they must learn.

## Priority 4 — Documentation

The structure is sound — `_pkgdown.yml` has eight hand-curated topical sections, and the README is
well organised. The problem is that almost nothing is *executed*, so it rots silently.

- **Only 3 lines of user-facing example code actually run** in the whole 5,525-line package. 16 of
  the 20 documented examples are wrapped in `\dontrun{}`. Proof of the cost: the example at
  `R/process.R:1673` calls a 3-argument function with 1 argument and has never errored, because it
  never runs. Several `\dontrun{}` blocks are cargo-cult — `util_separate_and_sort("z y x")` is
  wrapped, yet the identical call runs in `tests/testthat/test-utils.R:70`. **Un-`\dontrun` the ones
  that already work; that is the cheapest permanent guard against doc rot.**
- **13 exported functions have no `@return`**, concentrated in the `metadata_add_*` family — exactly
  the functions that return invisibly, where it matters most.
- **`docs/` is committed (140 files) and stale**, still serving pages for functions deleted in 2.0.0
  (`build_combine.html`, `plot_trait_distribution_beeswarm.html`, `util_strip_taxon_names.html`,
  `build_update_taxonomy.html`). Add a pkgdown deploy workflow and let CI own the site.
- **`VignetteBuilder: knitr` is declared with no `vignettes/` directory.** Either add one worked
  vignette (which doubles as an integration test and is expected for CRAN) or drop the field.
- **The book and package have already drifted.** `traits.build-book` calls
  `traits.build::dataset_reports()` (twice) and `dataset_check_taxonomic_updates()` — neither
  exists. Nothing cross-checks book code against the package API; a small CI job in the book repo
  that asserts every `traits.build::` call resolves in `NAMESPACE` would prevent recurrence.
- Small fixes: `hhttp://` dead link (`README.md:78`), stray backtick (`:38`), the commented-out
  "Get started in 5 minutes" placeholder (`:56-61`), 3 topics missing from the pkgdown index, and
  `check_pivot_wider` listed in the public reference index while not exported.

## Priority 5 — Tests & CI

- **CI has been deliberately loosened to hide the WARNINGs.** `R-CMD-check.yml:53` sets
  `error-on: '"error"'`; the `r-lib` default is `"warning"`. That is why a documented-but-nonexistent
  `@param` and the `.na` crash shipped. **Fix the WARNINGs, then set `error-on: "warning"`.** Treat
  the NOTEs as a follow-up once the licence and `austraits` decisions land.
- **Coverage never runs on PRs** — `test-coverage.yaml:3-5` triggers only on push to `develop`, so no
  pull request shows a coverage delta.
- **Only R `release` is tested.** `Depends: R (>= 4.3.0)` is declared but never exercised, and there
  is no `r-devel` job to catch upstream breakage early. `master` has no CI at all.
- **The test-ordering trap is worse than issue #224 records.** The file is named `test-xamples.R`,
  not `test-examples.R` — and since `e` sorts before `s`, *the typo is the only reason it runs after
  `test-setup.R` and finds the fixture*. Renaming it to the obvious spelling would break the suite.
  Fix the fixture in `helper.R`, then the file can be named properly.
- **Tests leave nine artifacts in the working tree** and there are zero uses of `withr`/`on.exit`;
  cleanup happens at the *start* of the next run. Included is a nested `.git` unzipped into the
  package source tree (`test-setup.R:594`) and a 77 KB `test_austraits.rds` (`:664`) that nothing
  ever reads. All are gitignored, so the pollution is invisible rather than absent.
- **Zero `skip_if_not_installed()` calls.** `remake` (non-CRAN), `furrr` and `zip` are Suggests but
  hard-required by tests, so they error rather than skip where absent.
- **Coverage gaps:** `R/notetaker.R` 0/8 functions, `R/pivot.R` 0/1, `R/test_functions.R` 2/30 —
  the last being the engine behind `dataset_test`, the most-taught function in the book, with
  nothing verifying that a failing check actually fails.
- **Golden files: keep, but modernise.** The `examples/Test_2023_*` expected outputs are the single
  strongest asset in the suite (~2,900 rows across 9 datasets, each with a README documenting the
  behaviours it pins). But whole-tibble `expect_equal` gives unreadable diffs and there is no
  regeneration path, which pressures maintainers to hand-edit expected output to match observed
  output. `Config/testthat/edition: 3` is set and `_snaps/` exists but is empty — migrating to
  `expect_snapshot_value()` keeps the coverage, adds readable diffs and `snapshot_accept()`, and
  would collapse `test-xamples.R`'s eight near-identical 30-line blocks into one parameterised loop.
- ✅ **Already resolved:** the nondeterminism confessed in the comment at `test-xamples.R:24-27`
  ("running the tests line by line generates different ids") is the locale bug fixed in PR #221.
  That comment should be deleted when #221 merges.
- Also: `testthat::context()` (`R/testdata.R:69,935`) is deprecated under edition 3; and `testthat`,
  `styler`, `rmarkdown`, `kableExtra` sit in **Imports**, making testthat a hard runtime dependency.
  That is defensible given `dataset_test` runs it at runtime, but should be a documented choice.

---

## Suggested order of work

**Stage 0 — close the coverage gap first (days, low risk).** Two halves:

*Output artefacts:* add `identifiers` to the `tables` vector in all nine blocks; add assertions for
`sources.bib`, `definitions.yml` and the output `metadata.yml`. Re-enable `Test_2023_9` — and
implement it as the *failure-output* test it was always meant to be (see Open Questions), which is
the cheapest way to get any coverage at all of `R/test_functions.R`'s 30 validation helpers.

*Vocabularies:* extend the existing example datasets to cover the values the real databases use but
the tests never touch — **`taxonomic_resolution` per rank first** (including the invertebrate ranks,
which probe the hardcoded botanical list at `R/process.R:1966`), then the missing `basis_of_record`,
`life_stage`, `entity_type`, `value_type` and `basis_of_value` values.

Expect failures — regenerate and *review* the affected fixtures rather than blessing them. Migrate
to `expect_snapshot_value()` while here, so diffs are readable and regeneration is one command.
**Everything downstream depends on this net existing.**

**Stage 1 — fix what the net now catches (days, low risk).** Land the four open PRs and raise the
unPR'd `fix/rdevel-check-identifiers-citation`. Fix `.na` (`R/setup.R:624`) plus a test for
`metadata_add_identifiers`. Fix the lifecycle badge. Reconcile the paper: add the
`data_update_taxonomy()` alias, fix `build_combine()`'s shim, export
`database_create_combined_table`. Fix the `specimentID` typo and validate `identifier_type` against
the allowed list. Delete `forcats` and `base`; add `^ontology$` to `.Rbuildignore` (11 MB, currently
shipped — well over CRAN's 5 MB limit). ~~Fix the detached roxygen block at `R/pivot.R:8` **before
anyone runs `document()`**, since the next re-document silently deletes
`man/check_pivot_wider.Rd` and breaks the pkgdown build.~~ **No longer applies — see correction
5:** roxygen2 8.0.0 (adopted in #228) associates the block across the blank line, verified by
deleting the Rd and re-documenting.

**Stage 2 — turn the safety nets back on (days).** Resolve the licence. Clear the remaining check
NOTEs. Set `error-on: "warning"`, add `pull_request` to the coverage workflow, add `oldrel-1` and
`r-devel`, extend CI to `master`. Fix the `helper.R` fixture (#224) and rename `test-xamples.R` to
`test-examples.R`. Add `CONTRIBUTING.md` and `.lintr` — there is currently no style enforcement or
contributor guide for 5,525 lines of R.

**Stage 3 — the CRAN path (weeks).** Execute Option A: restore `convert_list_to_df1`/`df2` (17 lines)
as internals, move `austraits` to `Suggests` behind `util_require_package()`, and update `austraits`
to depend on `traits.build`. Guard the `remake`/`furrr`/`zip` test calls with
`skip_if_not_installed()`, add a `targets` template, and remove `Remotes:`. Decouple the
`custom_R_code` environment (Priority 1 item 5) so `Depends` → `Imports` becomes safe. Then submit.
Each step validated against all three downstream repos.

**Stage 3b — drop `remake` (paced by partners).** Migrate `ausinvertraits.build`, `AusFizz` and
`traits.build-template` off remake, then deprecate and remove `method = "remake"`. Deliberately
*after* submission: CRAN does not wait on partner timelines once the test calls are conditional.

**Stage 4 — structural (weeks, after the PRs merge).** Extract the location functions and unify the
two call sites; decompose `dataset_test_worker` along its existing `##` seams one section per commit;
then `dataset_process`; leave `process_parse_data` last (its wide and long branches contain
near-parallel loops that should be unified *before* extraction). Add the vignette.

**Stage 5 — finish the genericisation (a 3.0.0 release).** Since long-term design beats
back-compat, do the rename the 2023 spin-out never completed. The package claims to be generic; its
schema and public API still say `austraits`:

- **Schema key `austraits` → `database`** — `traits.build_schema.yml:83` plus 18 call sites across
  `R/process.R`, `R/testdata.R`, `R/test_functions.R`, `report_dataset.Rmd` and the tests. Every
  database built by this "generic" package currently carries a table group named after one
  particular database.
- **Five exported arguments named `austraits` → `database`** — `dataset_report()`,
  `build_add_version()`, `write_plaintext()`, `dataset_find_taxon()`, and `dataset_update_taxonomy(austraits_raw)`
  → `database_raw`. Real-world breakage is small: the pipeline templates call these positionally.
- **Parameterise `report_dataset.Rmd`** from `config/metadata.yml` — 51 `austraits` mentions plus 7
  Australia-specific ones. Two are load-bearing, not prose: `:87` hardcodes the AusTraits GitHub URL
  so *every* generated report links to the wrong repo, and `:174` reserves a metadata key named
  `austraits`. Retire the fragile line-index title patch at `R/reports.R:53`. Add a smoke test that
  renders a report first — nothing currently does.
- **Move the hardcoded phenology rule** (`R/process.R:1142`, `flowering_time`/`fruiting_time`) into
  `config/traits.yml` as a per-trait value-format rule, and the botanical rank list
  (`R/process.R:1966`) into the schema — the invertebrate ranks in `ausinvertraits.build` are the
  live evidence that a hardcoded botanical vocabulary is wrong.

**Migration is the deliverable, not an afterthought.** Ship a reader shim that accepts either schema
key so existing `.rds` databases still load, migrate all three family repos in the same release, and
give `austraits` a matching update. This is the one stage where the downstream build gate is
mandatory rather than advisory.

**Stage 6 — strengthen the two companion repos (the user-facing front door).**

The paper (§3.2) designates
[`traits.build-template`](https://github.com/traitecoevo/traits.build-template) and
[`traits.build-book`](https://github.com/traitecoevo/traits.build-book) as the training resources —
for most new users these *are* the product. Both need work, and both are affected by decisions above.

**`traits.build-template`** — currently 4 config files, 2 example + 7 tutorial datasets:

- **It is built on `remake`** (`remake.yml` and a `.remake/` cache at top level), so it is one of the
  three repos Stage 3b must migrate. Until it is on `targets`, `method = "remake"` cannot be removed
  without breaking the paper's designated starting point for new users.
- **It has no build CI** — `.github/workflows/` contains only `add-to-project.yml`. Nothing verifies
  that the template still builds against current `traits.build`. Adding a workflow that builds the
  template on every `traits.build` release would be **the single best end-to-end integration test
  available**, because it exercises the real zero-to-database path a new user follows.
- Check its `config/` files against the current schema, and whether the 7 tutorial datasets still
  match the book chapters that reference them.

**`traits.build-book`** — 52 `.qmd` files, the de facto manual:

- **It already calls functions that do not exist**: `traits.build::dataset_reports()` (twice, in
  `adding_data_long.qmd`) and `dataset_check_taxonomic_updates()`. Nothing cross-checks book code
  against the package API. A small CI job that extracts every `traits.build::` call and asserts it
  resolves in `NAMESPACE` would prevent recurrence — worth doing *before* the Stage 4 refactor, so
  the book is verified against the API it documents.
- It will need updating for every decision above: the `targets` migration, the
  `data_update_taxonomy` alias, the `Depends` contract (the book should state explicitly that
  `custom_R_code` may use unqualified dplyr/tidyr/stringr, since that is now a documented guarantee),
  and any vocabulary/validation changes from Stage 0.
- Its `tutorial_dataset_1..7.qmd` mirror `tests/testthat/examples/Test_2023_1..7` but are maintained
  independently — worth deciding whether one should generate the other.

Sequencing note: do this **last**, but plan it as part of the same release rather than as a
follow-up, because the template breaking is a user-visible regression even when the package itself
is fine.

## Sequencing against work already in flight

Four open PRs, all `MERGEABLE`, plus one unPR'd branch:

| Branch / PR | Note |
|---|---|
| #219 (#178 rcrossref), #220 (#49 time), #221 (#29 locale), #222 (#218 identifiers) | all green, land these first |
| `fix/rdevel-check-identifiers-citation` | **3 commits ahead of develop, no PR** — clears *both* R CMD check WARNINGs and the `schema` NOTE. Should be raised as a PR and merged early; #222 needs it for `dataset_test` to fully work |
| `origin/dataset_check-functions` | 7 finished functions for #137, unmerged ~19 months |
| `origin/adding-synonym-automated-replacements` | complete word-boundary code relevant to #21 |
| `origin/add_postprocessing_scripts-demo` | contains nothing of value — safe to delete |

**Do the refactoring in Priority 2 only after the four PRs merge**, since they touch
`process.R`/`testdata.R`.

---

## Small fixes worth batching into one PR

All verified, all low-risk, none individually worth a PR:

- **Dead code:** `create_tree_branch` (`R/utils.R:270`), `test_expect_list_names_contain`
  (`R/test_functions.R:302`), `database_create_combined_table` (`R/utils.R:372`) — none exported,
  none called. Plus orphaned `inst/figures/traits_build_hex.png` and `hex.R` (a sticker generator
  installed into every user's library).
- **Broken URLs:** `autraits.build` (misspelled, 404s) in `DESCRIPTION:14`, `NEWS.md:43` and
  `man/traits.build-package.Rd:9` — the DESCRIPTION one is CRAN-visible; `hhttp://` in `README.md`.
- **`@keywords internal`** on the ~47 documented-but-unexported topics — halves the apparent API
  surface on the pkgdown site at zero risk.
- **`.Rbuildignore` cruft:** `^docs$` duplicated; 15 of 28 patterns match nothing; `.Rhistory` and
  12 `.DS_Store` files are gitignored but not Rbuildignored.
- **`testthat::context()`** (`R/testdata.R:69,935`) is deprecated under edition 3.
- **Delete the stale comment** at `test-xamples.R:24-27` ("running the tests line by line generates
  different ids") once PR #221 merges — that nondeterminism was the locale bug, now fixed.
- **Prune 4 stale branches**; `fix-bug-in-'read_metadata'` has a `gone` upstream. Two branches carry
  backticks in their names.

## Verification

Ordered to match the stages:

1. **Stage 0 (the new net).** Every one of the 12 output artefacts is asserted for all nine example
   datasets. Prove the net works by deliberately breaking something small in `process.R` and
   confirming the suite fails — an unasserted golden file gives false confidence, which is exactly
   how we got here.
2. **Stage 1.** `metadata_add_identifiers()` runs end-to-end without error and has a test;
   `dataset_test()` completes on `Test_2023_1` reporting only the intentional `wrong_trait_name`
   failure; the published Fig. 1 workflow is runnable as written.
3. **Stage 2.** `R CMD check --as-cran` reports 0 errors and 0 warnings (notes tracked separately
   until the licence and `austraits` calls land). CI green on macOS/Ubuntu/Windows and on
   `oldrel-1`.
4. **Stage 3/4 (behaviour preservation).** Golden files unchanged *except* where a fix deliberately
   changes output — those must be reviewed diff-by-diff, never bulk-accepted. The locale test from
   #221 must still pass, since it independently pins cross-machine reproducibility.
5. **Throughout:** a fresh-clone check (`git clone && R CMD build && R CMD check`) each stage —
   several problems in this review reproduce *only* on a clean checkout, not on a developer machine
   where generated fixtures already exist.

### The stability gate — run before every merge

The three downstream databases are the real acceptance test, and they catch things the package's own
suite cannot (1,240 `custom_R_code` call sites, 601 datasets, the full vocabulary range):

```
austraits.build       411 datasets
ausinvertraits.build  160 datasets
AusFizz                30 datasets
```

For each: build against current `develop`, capture the output, build against the branch, and diff.
**Any change in built output is a regression unless it is a fix we intended.** This is the only
mechanism that will catch a `Depends` change, an `austraits` move, a rank-vocabulary edit or a
`remake` retirement breaking a real database — and it should gate Stages 3 and 4 in particular.

Worth automating as a nightly or on-demand workflow, since running it by hand will not survive
contact with a 15-commit year.

## What I'd expect this to achieve

Against the four goals:

- **Easy to use** — a working headline feature, errors that name the problem, a runnable example, a
  reference site showing 47 functions instead of ~88, and a published workflow that actually runs.
- **Well documented** — examples that execute (so they cannot rot), a CI-owned pkgdown site instead
  of a hand-committed stale one, and a guard against book/package drift.
- **Easily maintained** — one definition of each build step instead of two, a check that fails when
  it should, and a test net covering the whole output structure so the larger restructure is safe.
- **Stable** — in the sense that matters: no *silent* breakage. Breaking changes happen (Stage 5 is
  one), but they are versioned, shipped with migration shims, applied to the family repos in the
  same release, and gated by a downstream build check that makes any change in built output visible
  before it ships.

## Open questions

Resolved during review: licence → **BSD-2**; `austraits` → **Option A**; `data_update_taxonomy` →
**add alias**; `build_combine` → **keep current deprecation, fix the shim**; `remake` → **retire for
`targets`**.

Also resolved: the `austraits` → `database` genericisation is **in scope**, as Stage 5 / v3.0.0 —
long-term design over back-compat, with a reader shim and lockstep family migration.

Also resolved: **`remake` is to be dropped**, but paced by partner migration —
`ausinvertraits.build`, `AusFizz` and `traits.build-template` all build with it
(`austraits.build` already uses a plain `build.R`). Crucially, `traits.build` itself never calls
remake, so guarding the six test calls unblocks CRAN *now* and the removal can follow at partner
pace. And **ownership of the `austraits` side of Option A sits with Daniel or Fonti Kar**.

Nothing is currently blocking a start on Stage 0. **Stage 0 is now done (#230); Stage 1 is in progress.** Its one open decision is whether `database_create_combined_table` should be exported as the thin `austraits::flatten_database` alias it is today, or reimplemented here first. Reimplementing means porting 288 lines of `join_*` query-layer code out of austraits, which is that package's job, not this one's -- so exporting the alias and guarding it in Stage 3, exactly as this review already prescribes for `build_combine()`, looks right.

### Resolved while writing this: what `Test_2023_9` is for

Not broken, and not abandoned — **parked awaiting a capability that was never built.** Its README
says:

> This dataset is for testing `dataset_test` (not yet implemented). Erroneous metadata entry has been
> added to check the expected output of `dataset_test`.

It was added and immediately commented out in the same commit (`9e4ceb9`, "Add
`repeat_measurements_id`"), and it **still builds cleanly today** (379 trait rows). It is a
purpose-built fixture carrying deliberate errors — NA `trait_name`, duplicate context values,
duplicated `flowering_time`, a numeric column coerced to character, all five context categories.

That matters because it is the ready-made answer to the worst gap in the suite: **nothing currently
verifies that a failing check actually fails.** `R/test_functions.R` — the engine behind
`dataset_test`, the most-taught function in the book — has 2 of 30 functions referenced by any test,
and the one near-miss (`expect_output(dataset_test(...))` at `test-xamples.R:17`) passes on *any*
output because it has no `regexp`.

**Recommendation:** in Stage 0, implement the parked block as a snapshot test of `dataset_test`'s
reporter output on `Test_2023_9`, asserting the specific errors it was designed to provoke. That
converts an abandoned fixture into coverage of the package's own validation layer, at low cost.
