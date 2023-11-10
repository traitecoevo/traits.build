

#' @export
database_create_combined_table <- function(database) {

  location_latlon <- 
    database$locations %>%
      dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
      tidyr::pivot_wider(names_from = location_property, values_from = value)         

  location_properties <- 
    database$locations %>%
      dplyr::filter(!location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
      dplyr::mutate(location_properties = paste0(location_property, "='", value,"'")) %>%
      dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
      dplyr::group_by(dataset_id, location_id, location_name) %>%
      dplyr::mutate(location_properties = paste0(location_properties, collapse = "; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()

  contributors <-
    database$contributors %>%
      dplyr::mutate(
        data_collectors = paste0(given_name, " ", last_name),
        data_collectors = ifelse(!is.na(ORCID), paste0(data_collectors, " (ORCID:",ORCID), data_collectors),
        data_collectors = ifelse(is.na(ORCID), paste0(data_collectors, " (affiliation:",affiliation), paste0(data_collectors, ";affiliation:",affiliation)),
        data_collectors = ifelse(!is.na(additional_role), paste0(data_collectors, ";additional_role:",additional_role,")"), paste0(data_collectors,")"))
      ) %>%
      dplyr::select(-dplyr::all_of(c("last_name", "given_name", "ORCID", "affiliation", "additional_role"))) %>%
      dplyr::group_by(dataset_id) %>%
      dplyr::mutate(data_collectors = paste0(data_collectors, collapse = "; ")) %>%
      dplyr::ungroup() %>%
      dplyr::distinct()  

  contexts_tmp <- 
    database$contexts %>%
      dplyr::mutate(
        value = ifelse(is.na(description), paste0(context_property, ":", value), paste0(context_property, ":", value, " (", description, ")"))
      ) %>%
    dplyr::select(-dplyr::all_of(c("description", "context_property", "category"))) %>%
    tidyr::separate_longer_delim(link_vals, ", ") %>% distinct()

  reformat_contexts <- function(contexts_table, context_id) {
    context_category <- gsub("_id", "s", context_id, fixed = TRUE)
    out <- contexts_table %>%
      dplyr::filter(link_id == context_id) %>%
      dplyr::select(-link_id) %>%
      dplyr::distinct(dataset_id, link_vals, .keep_all = TRUE)

    names(out)[which(names(out) == "value")] <- context_category
    names(out)[which(names(out) == "link_vals")] <- context_id
    
    out
  }

  join_contexts <- function(data, contexts_tmp) {
    data %>%
    left_join(
      by = c("dataset_id", "treatment_context_id"),
      reformat_contexts(contexts_tmp, "treatment_context_id")
    ) %>%
      left_join(
        by = c("dataset_id", "plot_context_id"),
        reformat_contexts(contexts_tmp, "plot_context_id")
      ) %>%
      left_join(
        by = c("dataset_id", "entity_context_id"),
        reformat_contexts(contexts_tmp, "entity_context_id")
      ) %>%
      left_join(
        by = c("dataset_id", "temporal_context_id"),
        reformat_contexts(contexts_tmp, "temporal_context_id")
      ) %>%
      left_join(
        by = c("dataset_id", "method_context_id"),
        reformat_contexts(contexts_tmp, "method_context_id")
      )
  }

  combined_table <-
    database$traits %>%
      left_join(location_latlon, by = c("dataset_id", "location_id")) %>%
      left_join(location_properties, by = c("dataset_id", "location_id", "location_name")) %>%
      join_contexts(contexts_tmp) %>%
      left_join(database$methods %>% dplyr::select(-dplyr::all_of(c("data_collectors"))), by = c("dataset_id", "trait_name", "method_id")) %>%
      left_join(database$taxa, by = c("taxon_name")) %>%
      left_join(database$taxonomic_updates, by = c("taxon_name", "dataset_id", "original_name")) %>%
      dplyr::select(-dplyr::all_of(c("location_id", "method_id", "method_context_id", "treatment_context_id", "plot_context_id", "temporal_context_id", "entity_context_id")))

  combined_table
}
  