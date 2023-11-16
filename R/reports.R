
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
#' @return Html file of the rendered report located in the specified output folder
#' @export
dataset_report <- function(dataset_id, austraits, overwrite = FALSE,
                           output_path = "export/reports",
                           input_file = system.file("support", "report_dataset.Rmd", package = "traits.build"),
                           quiet = TRUE, keep = FALSE) {

  for (d in dataset_id)
    dataset_report_worker(
      dataset_id = d,
      austraits = austraits,
      overwrite = overwrite,
      output_path = output_path,
      input_file = input_file,
      quiet = quiet,
      keep = keep
    )
}

dataset_report_worker <- function(dataset_id, austraits, overwrite = FALSE,
                                  output_path = "export/reports",
                                  input_file = system.file("support", "report_dataset.Rmd", package = "traits.build"),
                                  quiet = TRUE, keep = FALSE) {

  if (!file.exists(output_path)) {
    dir.create(output_path, FALSE, TRUE)
  }
  browser()
  # Filenames
  input_Rmd <- sprintf("tmp_%s_report.Rmd", dataset_id)
  output_html <- sprintf("%s/%s.html", output_path, dataset_id)
  traits.build::plot_trait_distribution_beeswarm(austraits, "leaf_mass_per_area", "dataset_id", highlight = dataset_id, hide_ids = TRUE)
  if (overwrite || !file.exists(output_html)) {

    message(sprintf("Building report for %s ", dataset_id))

    # Create a new Rmd file with name embedded in title
    x <- readLines(input_file, encoding = "UTF-8")
    x[2] <- sprintf("title: Report on study `%s` from", dataset_id)
    writeLines(x, input_Rmd)

    # Knit and render. Note, call render directly
    # in preference to knit, then render, as leaflet widget
    # requires this to work
    # Warning: result assigned but may not be used
    result <- try(
      rmarkdown::render(
        input_Rmd,
        output_file = output_html,
        quiet = quiet,
        params = list(
          dataset_id = dataset_id,
          austraits = austraits
        )
      )
    )

    # Remove temporary Rmd
    if (!keep)
      unlink(input_Rmd)
    message(" -> ", output_html, "\n")
  } else {
    message(sprintf(red("Report for %s") %+% red(" already exists -> %s\n"), blue(dataset_id), blue(output_html)))
  }

}

#' Format table with kable and default styling for html
#'
#' @param ... Arguments passed to `kableExtra::kable()`
#' @importFrom rlang .data
#' @export
util_kable_styling_html <- function(...) {
    txt <-
      kableExtra::kable(...) %>%
      kableExtra::kable_styling(...,
                  bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                  full_width = FALSE,
                  position = "left"
                  )

    # Hack to add margin to plot
    gsub('style="width: auto ', 'style="margin-left:30px; width: auto ', txt)
}
