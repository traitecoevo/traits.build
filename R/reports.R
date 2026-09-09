
#' Build reports for listed datasets
#'
#' Builds a detailed report for every dataset with a unique `dataset_id`, based on the template Qmd file provided.
#' The reports are rendered as html files and saved in the specified output folder.
#'
#' Rendering happens in two Quarto passes rather than one `quarto_render()` call straight to
#' html: the taxon-list tables' `<a>` links (and their bootstrap classes) do not survive a
#' single `.qmd` -> html render, but the same knitted content renders correctly a second time
#' as markdown -> html. The first pass knits and discards its own (broken) html; the second
#' renders the knitted `.md` it leaves behind.
#'
#' @param dataset_id Name of specific study/dataset
#' @param austraits Compiled austraits database
#' @param overwrite Logical value to determine whether to overwrite existing report
#' @param output_path Location where rendered report will be saved
#' @param input_file Report script (.qmd) file to build study report
#' @param quiet An option to suppress printing during rendering from knitr, Quarto and pandoc
#' @param keep Keep the intermediate `.qmd`, the knitted `.md` and its
#'  `_files` directory (see Details)?
#'
#' @rdname dataset_report
#' @return Invisibly, a logical named by `dataset_id`, `TRUE` where the report
#'   was written. Reports are rendered as html files in `output_path`. A dataset
#'   whose report fails to render warns and returns `FALSE` rather than aborting
#'   the batch.
#' @export
dataset_report <- function(dataset_id, austraits, overwrite = FALSE,
                           output_path = "export/reports",
                           input_file = system.file("support", "report_dataset.qmd", package = "traits.build"),
                           quiet = TRUE, keep = FALSE) {

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
        keep = keep
      ),
    logical(1)
  )

  invisible(built)
}

dataset_report_worker <- function(dataset_id, austraits, overwrite = FALSE,
                                  output_path = "export/reports",
                                  input_file = system.file("support", "report_dataset.qmd", package = "traits.build"),
                                  quiet = TRUE, keep = FALSE) {

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
    # Rendered in two steps, not one `quarto_render(input_qmd, ...)` call.
    # Going straight from `.qmd` to html breaks the taxon-list tables' `<a>`
    # links (and their bootstrap classes): Quarto's pandoc invocation for a
    # `.qmd` input reprocesses their raw ```{=html} blocks differently than
    # it does for a plain `.md` file, discarding the tags `as_link()` writes
    # into them. The exact same knitted content, rendered a second time as
    # markdown, does not have this problem -- so step one knits and discards
    # its own (broken) html, `debug = TRUE` being what leaves the knitted
    # `.md` around for step two to render properly.
    #
    # Rendering is allowed to fail without aborting, so that one bad dataset
    # does not stop a batch of reports. It must still be reported: this was a
    # bare `try()` whose result was discarded, so a failed render printed the
    # success line anyway and the only trace was whatever `try()` happened to
    # write to stderr (#244).
    knitted_md <- sub("\\.qmd$", ".html.md", input_qmd)
    knitted_files <- sub("\\.qmd$", "_files", input_qmd)
    result <- try(
      {
        quarto::quarto_render(
          input_qmd,
          output_file = output_filename,
          quiet = TRUE,
          debug = TRUE,
          execute_params = list(
            dataset_id = dataset_id,
            austraits_path = austraits_rds
          )
        )
        if (!file.exists(knitted_md)) {
          stop("Expected knitted markdown not found: ", knitted_md, call. = FALSE)
        }
        quarto::quarto_render(knitted_md, output_file = output_filename, quiet = quiet)
      },
      silent = TRUE
    )

    unlink(austraits_rds)

    # quarto_render() writes its output beside the input (the current
    # directory, since `input_qmd` names no path), not into `output_path`
    if (!inherits(result, "try-error") && !file.exists(output_html)) {
      file.rename(output_filename, output_html)
    }

    # Remove temporary qmd and the intermediates the two-step render leaves
    if (!keep) {
      unlink(input_qmd)
      unlink(knitted_md)
      unlink(knitted_files, recursive = TRUE)
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
#'
#' @param database traits.build database
#' @param dataset dataset_id for dataset of interest
#'
#' @returns table with number of new taxa added for each trait
#'
#' @examples
#' \dontrun{
#' new_taxa_trait_combinations(austraits, "Falster_2003")
#' }
#' @export
new_taxa_trait_combinations <- function(database, dataset) {

  # extract accepted species from APC
  accepted_species <- database$taxa %>%
    dplyr::filter(.data$taxonomic_status == "accepted") %>%
    dplyr::filter(.data$taxon_rank %in% c("species", "subspecies", "varietas", "forma")) %>%
    dplyr::select("taxon_name")

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
