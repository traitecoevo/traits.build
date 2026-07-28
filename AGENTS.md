# traits.build — agent & contributor guide

`traits.build` is an R package providing a **workflow to harmonise trait data** from diverse
sources into a documented, relational standard structure. It is the generic engine spun out of the
AusTraits project in 2023 (see Wenk et al. 2024, doi:10.1016/j.ecoinf.2024.102773).

## Repo-local guidance

- **Code:** `R/` (functions), `tests/` (testthat), `man/` (generated docs), `NAMESPACE`.
- **Data-model ontology:** `ontology/` documents the ontology of the *data model*
  (entities/relations). This is **not** APD's trait-definition vocabulary — they're parallel, don't
  conflate them (see family context below).
- **Schema:** the relational `traits.build` schema is the source of truth for the data structure
  that other family repos conform to.
- **Docs:** user manual at the [traits.build-book](https://traitecoevo.github.io/traits.build-book/);
  function reference at <http://traitecoevo.github.io/traits.build/>.

Dev follows the standard R-package workflow: `devtools::load_all()`, `devtools::test()`,
`devtools::check()`. Default development branch is `develop`.

> The README's **deprecated** lifecycle badge is a mistake, not a statement of intent — the package
> is actively maintained and CRAN submission is a goal. Don't treat it as a reason to hold back on
> new work. Fixing the badge is tracked in #225.

**Test fixtures:** the nine `tests/testthat/examples/Test_2023_*` datasets are golden-file
regression tests covering the whole output structure. Never hand-edit an expected file towards the
output you observed — run `Rscript regenerate-examples.R` from `tests/testthat/` and read the diff.
Every diff is either a fix you meant to make or a regression.

### `Depends` is a contract, not an oversight — read this before editing `DESCRIPTION`

This is the easiest way to break every downstream database, and it looks exactly like tidying up.

`DESCRIPTION` has `dplyr`, `lubridate`, `readr`, `stringr` and `tidyr` in **`Depends`**, so they are
*attached* when traits.build loads. That is load-bearing. Datasets carry `custom_R_code` snippets in
their `metadata.yml`, which the build evaluates with `eval(parse(text = ...), new.env())` in
`process_custom_code()` (`R/process.R`). `new.env()` chains to the **search path**, so those snippets
resolve unqualified names — `mutate()`, `filter()`, `str_detect()` — through whatever is attached.

Across the three database repos that is **~1,240 unqualified call sites in 601 datasets**:

| Repo | Datasets | Unqualified calls |
|---|---|---|
| `austraits.build` | 411 | 970 |
| `ausinvertraits.build` | 160 | 170 |
| `AusFizz` | 30 | 100 |

**Moving those five to `Imports` breaks all of them**, at build time, in repos whose tests do not run
here. CRAN treats a heavy `Depends` as a style smell rather than a blocker, so there is no deadline
forcing the change.

What is safe: adding to `Imports`, and removing entries that are genuinely unused — `base` (a no-op)
and `forcats` (referenced nowhere) came out this way. What is not safe: moving any of the tidyverse
five out of `Depends` without first making the `custom_R_code` environment explicit, i.e. having
`process_custom_code()` populate its evaluation environment from the namespaces user code is entitled
to use instead of relying on what happens to be attached. Do that and `Depends` → `Imports` becomes
safe, and `custom_R_code` starts behaving identically under `library()`, `traits.build::`, `Rscript`
and `targets` workers — which it does not today. #225 sketches the fix; it is not done.

Any change here wants the downstream gate: build all three repos before and after, and diff the
output. Nothing in this repo's own suite will catch it.

> Separately: `austraits` is also in `Depends`, for a few re-exported conversion helpers, so the
> package graph runs `traits.build → austraits` even though in the *data* pipeline traits.build is
> upstream of austraits. That edge is a known wart with a plan attached (#225 Option A); it is not
> the contract above, and moving `austraits` to `Suggests` does not endanger `custom_R_code`, since
> no downstream snippet calls it unqualified. Run this package's tests after touching those helpers.

---

## AusTraits family — cross-package context

`traits.build` is part of the **AusTraits family** (a subset of the
[`traitecoevo`](https://github.com/traitecoevo) org) — here, the generic workflow engine + relational
data model/schema. Family-wide concerns are documented centrally in
**[austraits-meta](https://github.com/traitecoevo/austraits-meta)** — don't restate them here, read
them there:

- **Start with [`AGENTS.md`](https://github.com/traitecoevo/austraits-meta/blob/main/AGENTS.md)** —
  pipeline order, who owns what, dependency direction (incl. the reversed `traits.build → austraits`
  edge), source-of-truth rules, cross-boundary artifacts, gotchas.
- **[`dependencies.yml`](https://github.com/traitecoevo/austraits-meta/blob/main/dependencies.yml)** —
  machine-readable package graph + cross-boundary artifacts.
- **[`governance/`](https://github.com/traitecoevo/austraits-meta/tree/main/governance)** —
  label taxonomy, board #9 conventions, release playbooks, triage.

**Filing issues:** the whole family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9) (new issues auto-add to it). Follow
the [issue & labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md):
pick one work-type label (`bug` / `task` / `epic`); Status and Priority are set on the board, not as
labels.

> austraits-meta is hand-maintained prose — a map, not ground truth. Verify specifics against the
> actual repos.
