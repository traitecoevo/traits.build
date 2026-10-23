
#' Build reports for listed datasets
#'
#' Builds a detailed report for every dataset with a unique `dataset_id`, based on a Quarto (.qmd) template.
#' The reports are rendered as html files and saved in the specified output folder.
#'
#' Run this from the root of the database's repository: the report reads each
#' dataset's `data/<dataset_id>/metadata.yml` from there.
#'
#' @section Report template:
#' traits.build ships a generic template, which introduces the database from
#' its own metadata (the `metadata` block of `config/metadata.yml`: title,
#' version, description, DOI, reference, repository and licence) and works for
#' any database the package builds.
#'
#' A database can use its own template instead by saving it as
#' `config/report_dataset.qmd`, beside its other build inputs. When
#' `input_file` is `NULL`, `dataset_report()` uses that file if it exists in the
#' working directory, and otherwise the generic template. To start one, copy
#' the generic template:
#'
#' ```r
#' file.copy(system.file("support", "report_dataset.qmd", package = "traits.build"),
#'           "config/report_dataset.qmd")
#' ```
#'
#' A template receives the parameters `dataset_id`, `austraits_path` (the
#' compiled database, saved as `.rds`) and `show_questions`, and must declare
#' all three under `params:` in its YAML header, or rendering fails. It also
#' needs a `title:` line there, which `dataset_report()` replaces with the
#' dataset's id. The template is copied into the working directory and
#' rendered from there, so any path it uses, such as an image or an included
#' file, is relative to the working directory, not to `config/`. `keep = TRUE`
#' leaves that copy, `tmp_<dataset_id>_report.qmd`, in the working directory.
#'
#' @param dataset_id Name of specific study/dataset
#' @param austraits Compiled austraits database
#' @param overwrite Logical value to determine whether to overwrite existing report
#' @param output_path Location where rendered report will be saved
#' @param input_file Report template (.qmd) to build the study report from. The
#'   default, `NULL`, uses `config/report_dataset.qmd` if the working directory
#'   has one, and otherwise the generic template shipped with traits.build (see
#'   the Report template section). A path given here is always used.
#' @param quiet An option to suppress printing during rendering from knitr, Quarto and pandoc
#' @param keep Keep the temporary, per-dataset copy of the template (see the Report template section)?
#' @param show_questions Include the questions to the data contributor? The
#'   default, `TRUE`, gives the review copy; `FALSE` gives a final copy with all
#'   questions (and the explanatory text about them) omitted.
#'
#' @rdname dataset_report
#' @return Invisibly, a logical named by `dataset_id`, `TRUE` where the report
#'   was written. Reports are rendered as html files in `output_path`. A dataset
#'   whose report fails to render warns and returns `FALSE` rather than aborting
#'   the batch.
#' @export
dataset_report <- function(dataset_id, austraits, overwrite = FALSE,
                           output_path = "export/reports",
                           input_file = NULL,
                           quiet = TRUE, keep = FALSE, show_questions = TRUE) {

  input_file <- dataset_report_template(input_file)

  built <- vapply(
    dataset_id,
    function(d)
      dataset_report_worker(
        dataset_id = d,
        austraits = austraits,
        overwrite = overwrite,
        output_path = output_path,
        input_file = input_file,
        quiet = quiet,
        keep = keep,
        show_questions = show_questions
      ),
    logical(1)
  )

  invisible(built)
}

# The template `dataset_report()` renders: `input_file` when given; otherwise
# the database's own `config/report_dataset.qmd` if the working directory has
# one; otherwise the generic template shipped with the package.
dataset_report_template <- function(input_file = NULL,
                                    local_file = file.path("config", "report_dataset.qmd")) {
  if (!is.null(input_file)) {
    return(input_file)
  }

  if (file.exists(local_file)) {
    message("Using the database's own report template, ", local_file)
    return(local_file)
  }

  system.file("support", "report_dataset.qmd", package = "traits.build")
}

dataset_report_worker <- function(dataset_id, austraits, overwrite = FALSE,
                                  output_path = "export/reports",
                                  input_file = dataset_report_template(),
                                  quiet = TRUE, keep = FALSE, show_questions = TRUE) {

  if (!file.exists(input_file)) {
    stop("Report template not found: ", input_file, call. = FALSE)
  }

  util_require_package("quarto", "to render dataset reports")

  if (!file.exists(output_path)) {
    dir.create(output_path, FALSE, TRUE)
  }

  # Filenames
  input_qmd <- sprintf("tmp_%s_report.qmd", dataset_id)
  output_filename <- sprintf("%s.html", dataset_id)
  output_html <- file.path(output_path, output_filename)

  if (overwrite || !file.exists(output_html)) {

    message(sprintf("Building report for %s ", dataset_id))

    # Copy the template, embedding the dataset name in its title. The title has
    # to be edited into the YAML header itself: a document's own `title` beats
    # anything passed via `quarto_render(metadata = )`.
    x <- readLines(input_file, encoding = "UTF-8")
    title_line <- grep("^title:", x)[1]
    if (is.na(title_line)) {
      stop("Report template has no `title:` line in its YAML header: ", input_file, call. = FALSE)
    }
    x[title_line] <- sprintf("title: Report on study `%s`", dataset_id)
    writeLines(x, input_qmd)

    # Pass the database by path, not by value. `execute_params` is serialised
    # to YAML for the external `quarto render` process, and YAML cannot
    # represent the `NA`s that `austraits$traits` always contains. The template
    # reads it back with `readRDS(params$austraits_path)`.
    austraits_rds <- tempfile(fileext = ".rds")
    saveRDS(austraits, austraits_rds)

    # The output is a single self-contained file; that comes from
    # `embed-resources: true` in the template's YAML.
    #
    # This used to render in two Quarto passes -- knit-and-discard, then
    # re-render the knitted `.md` -- to work around the taxon-list tables'
    # `<a>` links (and their bootstrap classes) not surviving a single
    # `.qmd` -> html render. That was fixed at the source instead (the
    # tables are now emitted as explicit ` ```{=html} ` raw blocks with
    # `kable(format = "html", escape = FALSE)`; see the taxon-list chunks in
    # the template), which is what actually made those links/classes
    # survive -- the two-pass re-render was never the fix, just a
    # workaround alongside it. It's gone now because it broke htmlwidgets
    # (e.g. the locations `leaflet` map): a widget's JS/CSS dependencies are
    # tracked as knitr/pandoc-level metadata during the *original* render,
    # not as text in the knitted `.md`, so re-rendering that `.md` alone in
    # a fresh `quarto_render()` call had no way to know a map needed
    # bundling and silently dropped it -- the map's `<div>` and data were
    # still there, just inert with no leaflet.js to draw it.
    #
    # Rendering is allowed to fail without aborting, so that one bad dataset
    # does not stop a batch of reports. It must still be reported: this was a
    # bare `try()` whose result was discarded, so a failed render printed the
    # success line anyway and the only trace was whatever `try()` happened to
    # write to stderr (#244).
    result <- try(
      quarto::quarto_render(
        input_qmd,
        output_file = output_filename,
        quiet = quiet,
        execute_params = list(
          dataset_id = dataset_id,
          austraits_path = austraits_rds,
          show_questions = show_questions
        )
      ),
      silent = TRUE
    )

    unlink(austraits_rds)

    # quarto_render() writes its output beside the input (the current
    # directory, since `input_qmd` names no path), not into `output_path`.
    # Guard on the freshly-rendered file existing, not on `output_html`
    # *not* existing -- the latter broke `overwrite = TRUE` for a dataset
    # that already had a report: the rebuild would render fine but this
    # rename was skipped (because the old `output_html` was still there),
    # silently leaving the stale report in place and orphaning the new one
    # beside the input. `file.rename()` overwrites an existing destination.
    if (!inherits(result, "try-error") && file.exists(output_filename)) {
      file.rename(output_filename, output_html)
    }

    # Remove the temporary, per-dataset copy of the template
    if (!keep) {
      unlink(input_qmd)
    }

    if (inherits(result, "try-error")) {
      warning(
        sprintf(
          "Report for %s failed to build; no file written to %s.\n  %s",
          dataset_id, output_html, conditionMessage(attr(result, "condition"))
        ),
        call. = FALSE
      )
      return(invisible(FALSE))
    }

    message(" -> ", output_html, "\n")
  } else {
    message(sprintf(red("Report for %s") %+% red(" already exists -> %s\n"), blue(dataset_id), blue(output_html)))
  }

  invisible(TRUE)
}

#' Format table with kable and default styling for html
#'
#' @param ... Arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
util_kable_styling_html <- function(...) {
    txt <-
      kableExtra::kable(...) %>%
      kableExtra::kable_styling(
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  position = "left"
                  )

    # Hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', txt)
}

#' New taxa added by a dataset
#'
#' Function to indicate how many new taxa by trait combinations are added with a dataset.
#' Only accepted species and infraspecific taxa are counted.
#'
#' Which names are accepted is read from `resources$APC` when `resources` is
#' given, and otherwise from the database's own `taxa` table. The taxa table is
#' only as current as the database's taxon list, so pass `resources` to check
#' against the current APC.
#'
#' @param database traits.build database
#' @param dataset dataset_id for dataset of interest
#' @param resources Taxonomic resources from `APCalign::load_taxonomic_resources()`,
#'   or `NULL` (the default) to use `database$taxa`
#'
#' @returns table with number of new taxa added for each trait
#'
#' @examples
#' \dontrun{
#' dataset_new_taxa_trait_combinations(austraits, "Falster_2003")
#' resources <- APCalign::load_taxonomic_resources()
#' dataset_new_taxa_trait_combinations(austraits, "Falster_2003", resources = resources)
#' }
#' @export
dataset_new_taxa_trait_combinations <- function(database, dataset, resources = NULL) {

  # extract accepted species and infraspecific taxa
  ranks <- c("species", "subspecies", "variety", "form")

  if (is.null(resources)) {
    accepted_species <- database$taxa %>%
      dplyr::filter(.data$taxonomic_status == "accepted") %>%
      dplyr::filter(.data$taxon_rank %in% ranks) %>%
      dplyr::select("taxon_name")
  } else {
    accepted_species <- resources$APC %>%
      dplyr::filter(.data$taxonomic_status == "accepted") %>%
      dplyr::filter(.data$taxon_rank %in% ranks) %>%
      dplyr::select(taxon_name = "canonical_name")
  }

  # extract new dataset
  new_data <- (database %>% austraits::extract_dataset(dataset))$traits %>%
    dplyr::distinct(.data$dataset_id, .data$taxon_name, .data$trait_name) %>%
    dplyr::mutate(combined = paste0(.data$taxon_name, "_", .data$trait_name)) %>%
    dplyr::filter(.data$taxon_name %in% accepted_species$taxon_name)

  # Create an empty tibble for instances where none of the input names are `accepted`.
  if (nrow(new_data) == 0) {
    return(
      tibble::tibble(
        dataset_id = character(),
        trait_name = character(),
        new_taxa = integer(),
        existing_taxa = integer()
      )
    )
  }

  # taxa present in new dataset
  traits_to_check <- new_data %>% dplyr::distinct(.data$trait_name)

  # data in database prior to new dataset
  preexisting_data <- (database %>% austraits::extract_trait(traits_to_check$trait_name))$traits %>%
    dplyr::filter(.data$dataset_id != dataset) %>%
    dplyr::distinct(.data$taxon_name, .data$trait_name) %>%
    dplyr::mutate(combined = paste0(.data$taxon_name, "_", .data$trait_name)) %>%
    dplyr::filter(.data$taxon_name %in% accepted_species$taxon_name)

  # counts of taxa per trait prior to new dataset
  preexisting_taxa <- preexisting_data %>%
    dplyr::select(-dplyr::all_of(c("taxon_name", "combined"))) %>%
    dplyr::group_by(.data$trait_name) %>%
    dplyr::mutate(existing_taxa = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()

  # number of new taxa added per trait once the new dataset is added
  new_taxa <- new_data %>% dplyr::filter(!.data$combined %in% preexisting_data$combined) %>%
    dplyr::select(-dplyr::all_of(c("taxon_name", "combined"))) %>%
    dplyr::group_by(.data$dataset_id, .data$trait_name) %>%
    dplyr::mutate(new_taxa = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    dplyr::left_join(preexisting_taxa, by = "trait_name")

  new_taxa

}
