#' Plot the distribution of a trait
#'
#' Dispatches to [plot_categorical_trait_distribution()] for a categorical trait (one with
#' no `unit`) or [plot_trait_distribution_jitter()] for a numeric one, so callers do not need
#' to know which kind of trait they are plotting.
#'
#' @param data A data frame containing trait data for a single trait, with `unit` and
#'  `value_type` columns.
#' @param trait A character string specifying the name of the trait to plot.
#' @param ... Additional arguments passed to the function.
#'
#' @return A ggplot2 object representing the distribution of the specified trait, or a
#'  placeholder plot noting "No data available" if `data` is empty.
#' @export
plot_trait_distribution <- function(data, trait, ...) {

  # For empty data
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No data available for this trait",
                            size = 6) +
           ggplot2::theme_void())
  }

  if (is.na(data$unit[1]))
    return(plot_categorical_trait_distribution(data, trait, family_count = 50))
  else {
    data_plot <- data |>
    filter(!value_type %in% c("bin", "range")) |>
    mutate(value = as.numeric(value))

    return(plot_trait_distribution_jitter(data_plot, trait, "family", hide_ids = FALSE))
  }
}

#' Plot Distribution of a Categorical Trait
#'
#' Generates a plot showing the distribution of a specified categorical trait across families.
#'
#' @param data A data frame containing trait data.
#' @param trait A character string specifying the name of the categorical trait to plot.
#' @param family_count An integer specifying the number of families to include in the plot.
#'
#' @return A ggplot2 object representing the distribution of the specified categorical trait.
#' @export
#'
#' @examples
#' # Example usage:
#' # plot_categorical_trait_distribution(my_data, "leaf_shape", 10)
plot_categorical_trait_distribution <- function(data, trait, family_count) {
  # if relational database, retain traits layer only
  if (is.null(dim(data))) {
    data <- (data |> austraits::join_taxa(vars = "family"))$traits
  }

  # filter to relevant trait
  data <- data |> dplyr::filter(trait_name == trait)

  # determine proportion of observations for each categorical trait value by taxon
  prop_by_species <- data |>
    dplyr::select(family, taxon_name, value, dataset_id) |>
    # separate instances with multiple strings in a value cell; as in polymorphic scorings for a single observation
    tidyr::separate_longer_delim(value, delim = " ") |>
    # probably not necessary, but good to retain
    dplyr::distinct() |>
    # for each observation, if multiple values, give each of them a fractional weight
    dplyr::group_by(family, taxon_name, dataset_id) |>
    dplyr::mutate(
      total_per_obs = n(),
      scaled_by_obs = 1 / total_per_obs
    ) |>
    dplyr::ungroup() |>
    dplyr::select(-total_per_obs) |>
    dplyr::group_by(family, taxon_name, value) |>
    # calculate counts for each categorical value, for each taxon, across all observations
    dplyr::mutate(counts = sum(scaled_by_obs)) |>
    dplyr::ungroup() |>
    dplyr::distinct(family, taxon_name, value, counts) |>
    dplyr::group_by(family, taxon_name) |>
    dplyr::mutate(total = sum(counts)) |>
    dplyr::ungroup() |>
    dplyr::mutate(prop = counts / total) |>
    dplyr::select(-total, -counts)

  # create dataframe of the 50 families with the greatest number of taxa with trait scores
  most_common_families <- prop_by_species |>
    dplyr::distinct(family, taxon_name) |>
    dplyr::group_by(family) |>
    dplyr::mutate(species_per_family = n()) |>
    dplyr::ungroup() |>
    dplyr::distinct(family, species_per_family) |>
    dplyr::arrange(-species_per_family) |>
    dplyr::slice(1:family_count)

  # now combine species-level proportions for each categorical trait value,
  # outputting family-level proportions instead
  prop_by_family <- prop_by_species |>
    dplyr::filter(family %in% most_common_families$family) |>
    dplyr::group_by(family, value) |>
    dplyr::mutate(
      counts_per_value = sum(prop),
      # actual number of taxa in the family with a record for this value.
      # NB: counts_per_value is a fractional, prop-weighted total - taxa whose
      # records disagree (e.g. "annual" in one dataset, "perennial" in another)
      # get split and down-weighted across every value they touch, so it
      # under-counts taxa and must not be used to size the bubbles
      n_taxa = dplyr::n_distinct(taxon_name)
    ) |>
    dplyr::ungroup() |>
    dplyr::distinct(family, value, counts_per_value, n_taxa) |>
    dplyr::group_by(family) |>
    dplyr::mutate(
      total = sum(counts_per_value)
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(prop = counts_per_value / total) |>
    dplyr::select(-total)

  # create ordered vector, indicating frequency of each categorical trait value
  # for ordering the x-axis
  counts_by_value <- prop_by_family |>
    dplyr::group_by(value) |>
    dplyr::mutate(total_counts = sum(counts_per_value)) |>
    dplyr::ungroup() |>
    dplyr::distinct(value, total_counts) |>
    dplyr::arrange(-total_counts) |>
    dplyr::pull(value)

  # turn categorical trait value into a factor, order by relative common-ness across all taxa
  prop_by_family <- prop_by_family |>
    dplyr::mutate(value = factor(value, levels = counts_by_value))

  # vs lowest prop of data for the most common categorical trait value,
  # for families without any records for most common categorical trait value, order by next value, etc
  family_vector <- prop_by_family |>
    # filter dataframe to only be the most common categorical trait value
    dplyr::arrange(value, -prop) |>
    dplyr::mutate(row_counter = row_number()) |>
    dplyr::group_by(family) |>
    dplyr::mutate(family_order = min(row_counter)) |>
    dplyr::arrange(family_order) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::arrange(family_order) |>
    dplyr::pull(family)

  prop_by_family <- prop_by_family |>
    # turn family into a factor, ordered by the proportion of species with the most common categorical trait value
    dplyr::mutate(family = factor(family, levels = family_vector)) |>
    # occassional observations don't have families attached, remove those
    dplyr::filter(!is.na(family)) |>
    dplyr::arrange(family)

  ggplot2::ggplot(ggplot2::aes(y = family, x = value, fill = prop, size = n_taxa), data = prop_by_family |> filter(prop > 0)) +
    ggplot2::geom_jitter(shape = 21, width = 0.15, height = 0) +
    ggplot2::scale_size_continuous() +
    ggplot2::scale_fill_viridis_c(option = "D") +
    ggplot2::labs(
      x = NULL, y = NULL,
      fill = "prop of species in family",
      size = "species count"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.line.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.05)
    )
}

#' Jitter Trait distribution
#'
#' Plots distribution of trait values by a grouping variable using geom_jitter
#'
#' @param data data
#' @param trait_name Name of trait to plot
#' @param y_axis_category One of `dataset_id`, `family`
#' @param highlight Specify a group to highlight
#' @param hide_ids Logical for whether to add a label on y_axis?
#'
#' @return A ggplot2 object representing the distribution of the specified trait, jittered
#'  by `y_axis_category`.
#' @export
#'
#' @examples
#' \dontrun{
#' austraits %>% plot_trait_distribution_jitter("wood_density", "dataset_id", "Westoby_2014")
#' }
#' @author Daniel Falster - daniel.falster@unsw.edu.au
plot_trait_distribution_jitter <- function(data,
                                             trait_name,
                                             y_axis_category,
                                             highlight = NA,
                                             hide_ids = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(ggplot2::ggplot() +
           ggplot2::annotate("text", x = 0.5, y = 0.5,
                            label = "No data available",
                            size = 6) +
           ggplot2::theme_void())
  }

  my_shapes <- c("Minimum" = 60, "Mean" = 16, "Maximum" = 62, "Unknown" = 18, "Raw" = 16)

  as_shape <- function(value_type) {
    p <- rep("Unknown", length(value_type))

    p[grepl("mean", value_type)] <- "Mean"
    p[grepl("min", value_type)] <- "Minimum"
    p[grepl("max", value_type)] <- "Maximum"
    p[grepl("raw", value_type)] <- "Raw"
    factor(p, levels = names(my_shapes))
  }

  data <- data %>%
    dplyr::mutate(shapes = as_shape(value_type)) %>%
    dplyr::mutate(value = as.numeric(value))

  # Define grouping variables and derivatives
  if (!y_axis_category %in% names(data)) {
    cli::cli_abort("Incorrect grouping variable! Grouping variable must be a variable in or joined to the traits table. Family and genus are supported if your input is a complete traits.build database.")
  }

  # define grouping variable, ordered by group-level by mean values
  # use log_value where possible

  if (min(data$value, na.rm = TRUE) > 0) {
    data$value2 <- log10(data$value)
  } else {
    data$value2 <- data$value
  }
  data$Group <- forcats::fct_reorder(data[[y_axis_category]], data$value2, na.rm = TRUE)

  n_group <- levels(data$Group) %>% length()

  # set colour to be alternating
  data$colour <- ifelse(data$Group %in% levels(data$Group)[seq(1, n_group, by = 2)],
    "a", "b"
  )

  # set colour of group to highlight
  highlight_present <- !is.na(highlight) & highlight %in% data$Group
  if (highlight_present) {
    data <- dplyr::mutate(data, colour = ifelse(Group %in% highlight, "c", colour))
  }

  vals <- list(
    minimum = 0.8 * min(data$value),
    maximum = 1.2 * max(data$value)
  )

  range <- (vals$maximum / vals$minimum)

  # Check range on y-axis
  y.text <- ifelse(n_group > 20, 0.75, 1)
  heights <- c(1, max(1, n_group / 7))

  # Second plot -- dots by groups, using geom_jitter
  p2 <-
    ggplot2::ggplot(data, ggplot2::aes(
      x = value,
      y = Group,
      colour = colour,
      shape = shapes,
      text = paste0(dataset_id, " | ", taxon_name)  # Add tooltip text
    )) +
    ggplot2::geom_jitter(width = 0) +
    ggplot2::ylab(paste("By ", y_axis_category)) +
    # inclusion of custom shapes: for min, mean, unknown
    # NB: this single line of code makes function about 4-5 slower for some reason
    ggplot2::scale_shape_manual(
      values = my_shapes,
      name = "Value Type"
    ) +
    ggplot2::scale_color_manual(
      values = c(
        "a" = "#0072B2",
        "b" = "#E69F00",
        "c" = "deeppink3"
      )
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 14, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.25)),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(y.text))
    ) +
    ggplot2::guides(colour = "none")

  if (hide_ids) {
    p2 <- p2 + ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

# Define scale on x-axis and transform to log if required
  if (vals$minimum > 0 & !is.infinite(vals$minimum) &
      vals$maximum > 0 & !is.infinite(vals$maximum) &
      range > 20) {

    limits <- c(vals$minimum, vals$maximum)
    breaks_fn <- scales::breaks_log(n = 6)
    my_breaks <- breaks_fn(limits)

    my_breaks <- my_breaks[!is.na(my_breaks) & !is.infinite(my_breaks)]
    my_breaks <- my_breaks[my_breaks >= limits[1] & my_breaks <= limits[2]]

    if (length(my_breaks) < 2) {
      my_breaks <- c(limits[1], limits[2])
    }

    my_labels <- my_breaks # scientific_10(my_breaks)

    p2 <- p2 +
      ggplot2::scale_x_log10(
        name = paste(trait_name, " (", data$unit[1], ")"),
        breaks = my_breaks,
        labels = my_labels,
        limits = limits
      )
  } else {
    p2 <- p2 + ggplot2::scale_x_continuous(limits = c(vals$minimum, vals$maximum)) +
      ggplot2::xlab(paste(trait_name, " (", data$unit[1], ")"))
  }

  # in-plot legend (lower right corner) noting what the deeppink3 points mean
  # NB: use a real geom_text layer (via annotate), not annotation_custom/grid grobs -
  # when this plot is converted with plotly::ggplotly() (see the `text` tooltip
  # aesthetic above), annotation_custom's grid grob is not reliably translated,
  # so edits to it can silently stop having any effect on the rendered widget.
  # x/y = Inf/-Inf anchor the label to the panel's bottom-right corner regardless
  # of the (possibly log-scaled) x-axis or the discrete y-axis.
  if (highlight_present) {
    p2 <- p2 +
      ggplot2::annotate(
        "text",
        x = Inf, y = -Inf, hjust = 1.1, vjust = -0.75,
        label = "\u25CF data from \n   this dataset",
        colour = "deeppink3", size = 5
      )
  }

  p2
}
