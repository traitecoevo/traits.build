
#' Build reports for listed datasets
#'
#' Builds a detailed report for every dataset with a unique `dataset_id`, based on the template Rmd file provided.
#' The reports are rendered as html files and saved in the specified output folder.
#'
#' @param dataset_id Name of specific study/dataset
#' @param austraits Compiled austraits database
#' @param overwrite Logical value to determine whether to overwrite existing report
#' @param output_path Location where rendered report will be saved
#' @param input_file Report script (.Rmd) file to build study report
#' @param quiet An option to suppress printing during rendering from knitr, pandoc command line and others
#' @param keep Keep intermediate Rmd file used?
#'
#' @rdname dataset_report
#' @return Invisibly, a logical named by `dataset_id`, `TRUE` where the report
#'   was written. Reports are rendered as html files in `output_path`. A dataset
#'   whose report fails to render warns and returns `FALSE` rather than aborting
#'   the batch.
#' @export
dataset_report <- function(dataset_id, austraits, overwrite = FALSE,
                           output_path = "export/reports",
                           input_file = system.file("support", "report_dataset_2026.Rmd", package = "traits.build"),
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
                                  input_file = system.file("support", "report_dataset_2026.Rmd", package = "traits.build"),
                                  quiet = TRUE, keep = FALSE) {

  if (!file.exists(input_file)) {
    stop("Report template not found: ", input_file, call. = FALSE)
  }

  if (!file.exists(output_path)) {
    dir.create(output_path, FALSE, TRUE)
  }

  # Filenames
  input_Rmd <- sprintf("tmp_%s_report.Rmd", dataset_id)
  output_html <- sprintf("%s/%s.html", output_path, dataset_id)

  if (overwrite || !file.exists(output_html)) {

    message(sprintf("Building report for %s ", dataset_id))

    # Create a new Rmd file with name embedded in title
    x <- readLines(input_file, encoding = "UTF-8")
    x[2] <- sprintf("title: Report on study `%s`", dataset_id)
    writeLines(x, input_Rmd)

    # Knit and render. Note, call render directly
    # in preference to knit, then render, as leaflet widget
    # requires this to work.
    #
    # Rendering is allowed to fail without aborting, so that one bad dataset
    # does not stop a batch of reports. It must still be reported: this was a
    # bare `try()` whose result was discarded, so a failed render printed the
    # success line anyway and the only trace was whatever `try()` happened to
    # write to stderr (#244).
    result <- try(
      rmarkdown::render(
        input_Rmd,
        output_file = output_html,
        quiet = quiet,
        params = list(
          dataset_id = dataset_id,
          austraits = austraits
        )
      ),
      silent = TRUE
    )

    # Remove temporary Rmd
    if (!keep)
      unlink(input_Rmd)

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
    filter(taxonomic_status == "accepted") %>%
    filter(taxon_rank %in% c("species", "subspecies", "varietas", "forma")) %>%
    select(taxon_name)
  
  # extract new dataset
  new_data <- (database %>% extract_dataset(dataset))$traits %>%
    distinct(dataset_id, taxon_name, trait_name) %>%
    mutate(combined = paste0(taxon_name,"_", trait_name)) %>%
    filter(taxon_name %in% accepted_species$taxon_name)

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
  traits_to_check <- new_data %>% distinct(trait_name)

  # data in database prior to new dataset
  preexisting_data <- (database %>% extract_trait(traits_to_check$trait_name))$traits %>% 
    filter(dataset_id != dataset) %>%
    distinct(taxon_name, trait_name) %>% 
    mutate(combined = paste0(taxon_name,"_", trait_name)) %>%
    filter(taxon_name %in% accepted_species$taxon_name)
  
  # counts of taxa per trait prior to new dataset
  preexisting_taxa <- preexisting_data %>%
    select(-taxon_name, -combined) %>%
    group_by(trait_name) %>%
    mutate(existing_taxa = n()) %>%
    ungroup() %>%
    distinct()
  
  # number of new taxa added per trait once the new dataset is added
  new_taxa <- new_data %>% filter(!combined %in% preexisting_data$combined) %>%
    select(-taxon_name, -combined) %>%
    group_by(dataset_id, trait_name) %>%
    mutate(new_taxa = n()) %>%
    ungroup() %>%
    distinct() %>%
    left_join(preexisting_taxa, by = "trait_name")
  
  new_taxa

}
