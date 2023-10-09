
# `traits.build`

<!-- badges: start -->
[![R-CMD-check](https://github.com/traitecoevo/traits.build/actions/workflows/R-CMD-check.yml/badge.svg)](https://github.com/traitecoevo/traits.build/actions/workflows/R-CMD-check.yml)
[![Codecov test coverage](https://codecov.io/gh/traitecoevo/traits.build/branch/develop/graph/badge.svg)](https://app.codecov.io/gh/traitecoevo/traits.build?branch=develop)
<!-- badges: end -->

The `traits.build` package provides a workflow for harmonising data from 
disconnected primary sources and arises from the AusTraits project [austraits.org](https://austraits.org). In 2023 this package was spun out as a separate package from the `traits.build` repository.

## Goal

This repository (`traits.build`) contains the raw data and code used to compile AusTraits from diverse, original sources.

To handle the harmonising of diverse data sources, we use a reproducible
workflow to implement the various changes required for each source to
reformat it suitable for incorporation in AusTraits. Such changes
include restructuring datasets, renaming variables, changing variable
units, changing taxon names. For the sake of transparency and continuing
development, the entire workflow is made available here.

## Prerequisites

1.  Familiarity with the [R programming language](https://www.r-project.org/), covered in [R for Data Science](https://r4ds.had.co.nz/).
2.  [Data science workflow management techniques](https://rstats.wtf/index.html).
3.  [How to write functions](https://r4ds.had.co.nz/functions.html) to prepare data, analyse data, and summarise results in a data analysis project.

## Installation 

There are multiple ways to install the `traits.build` package itself, and both the latest release and the development version are available.

| Type        | Source   | Command                                                           |
|-------------|----------|-------------------------------------------------------------------|
| Release     | CRAN     | coming                                     |
| Development | GitHub   | `remotes::install_github("traitecoevo/traits.build")`                     |


## Acknowledgements

**Funding**: The AusTraits project received investment (<https://doi.org/10.47486/TD044>, <https://doi.org/10.47486/DP720>) from the Australian Research Data Commons (ARDC). The ARDC is funded by the National Collaborative Research Infrastructure Strategy (NCRIS).

