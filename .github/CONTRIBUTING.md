# Contributing to traits.build

Thanks for your interest in `traits.build`. This document covers the mechanics of working on the
package. For what the package *is* and how the pieces fit together, read
[`AGENTS.md`](../AGENTS.md); for using it to build a database, read the
[traits.build book](https://traitecoevo.github.io/traits.build-book/).

By contributing you agree to abide by the
[Contributor Code of Conduct for the AusTraits projects](https://github.com/traitecoevo/austraits.build/blob/develop/.github/CODE_OF_CONDUCT.md).

## Getting set up

```r
# install.packages("pak")
pak::pak()          # installs dependencies from DESCRIPTION, including Remotes
devtools::load_all()
devtools::test()
devtools::check()
```

`austraits` is installed from GitHub via the `Remotes` field, so a plain
`install.packages()` of the dependency list is not enough.

## Branches and pull requests

- `develop` is the default branch. **Open pull requests against `develop`**, not `master`.
- `master` holds released versions. Both branches now run CI.
- One logical change per pull request, with a description that says what was broken and how you
  know it is fixed.

## What CI enforces

`R CMD check --as-cran` runs on macOS, Windows and Ubuntu (release), plus Ubuntu on `oldrel-1` and
`r-devel`. **The check fails on warnings, not just errors** — that threshold was loosened once
before, and a crash and a documented-but-nonexistent argument shipped as a direct result (#225). If
your change introduces a warning, fix the warning rather than the threshold.

`oldrel-1` exists because `DESCRIPTION` declares `R (>= 4.3.0)` and nothing else exercises that
floor. `r-devel` exists so upstream R changes surface here rather than on r-universe (#217).

Test coverage is reported on every pull request.

## Tests

Tests live in `tests/testthat/` and run with `devtools::test()`.

**The nine `tests/testthat/examples/Test_2023_*` datasets are golden-file regression tests** that
pin the whole output structure. Never hand-edit an expected file towards the output you observed.
Regenerate and read the diff:

```r
# from tests/testthat/
Rscript regenerate-examples.R
```

Every line of that diff is either a fix you meant to make or a regression you just introduced.

Test files must not depend on running in a particular order. `helper.R` seeds
`config/taxon_list.csv` (a build artefact, not a repository file) before any test file runs, so if
your test needs it, just read it. Historically the suite only worked because `test-xamples.R` was
misspelled and therefore sorted after `test-setup.R`, which generated that file as a side effect
(#224).

## Style

The package uses `%>%`, snake_case, and a 120-character line limit. `.lintr` is configured to match
that existing style rather than an ideal one, so:

```r
lintr::lint_package()
```

should give you a short, actionable list. Use `lint_package()` rather than `lint_dir("R")` — the
latter cannot see the package namespace and reports the package's own functions as undefined.

A few linters are switched off deliberately:

| Linter | Why |
|---|---|
| `pipe_consistency_linter` | `%>%` is house style, and downstream `custom_R_code` snippets depend on magrittr being attached |
| `indentation_linter`, `pipe_continuation_linter` | Would rewrite the whole codebase to no benefit |
| `commented_code_linter` | Some commented-out code is deliberately retained with an explanation |

There is a backlog of roughly 100 pre-existing lints, mostly over-long lines in `R/testdata.R` and
`R/test_functions.R`. Clean up what you touch; you are not expected to fix the rest.

Prefer `.data$column` inside tidyverse verbs and quoted names inside tidyselect (`all_of(c("x"))`)
over bare column names. This is not only style: bare names produce `no visible binding for global
variable` from `R CMD check`, and the pronoun catches typos that `utils::globalVariables()` would
silently permit — one such typo was a real crash (#227).

## Before you edit `DESCRIPTION`

`dplyr`, `lubridate`, `readr`, `stringr` and `tidyr` are in **`Depends`** on purpose. Datasets carry
`custom_R_code` snippets that are evaluated against the search path, so moving those packages to
`Imports` breaks roughly 1,240 call sites across 601 datasets in three downstream repositories —
none of which are tested here. Read the `Depends` section of [`AGENTS.md`](../AGENTS.md) first.

## Filing issues

The AusTraits family is tracked on one board,
[AusTraits #9](https://github.com/orgs/traitecoevo/projects/9). Pick one work-type label (`bug` /
`task` / `epic`); Status and Priority are set on the board, not as labels. See the
[issue and labelling guide](https://github.com/traitecoevo/austraits-meta/blob/main/governance/issue-guide.md).

For bug reports, include a reproducible example and the output of `sessionInfo()`.
