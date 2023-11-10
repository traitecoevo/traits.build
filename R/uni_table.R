wide_output <- function(austraits) {

lat_lon <- 
  austraits$locations %>%
    dplyr::filter(location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    tidyr::pivot_wider(names_from = location_property, values_from = value)         

locations <- 
  austraits$locations %>%
    dplyr::filter(!location_property %in% c("latitude (deg)", "longitude (deg)")) %>%
    dplyr::mutate(location_properties = paste0(location_property, "='", value,"'")) %>%
    dplyr::select(dplyr::all_of(c("dataset_id", "location_id", "location_name", "location_properties"))) %>%
    dplyr::group_by(dataset_id, location_id, location_name) %>%
    dplyr::mutate(location_properties = paste0(location_properties, collapse = "; ")) %>%
    dplyr::ungroup() %>%
    dplyr::distinct()
  
contributors <-
  austraits$contributors %>%
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
  austraits$contexts %>%
    dplyr::mutate(
      value = ifelse(is.na(description), paste0(context_property, ":", value), paste0(context_property, ":", value, " (", description, ")"))
    ) %>%
  dplyr::select(-dplyr::all_of(c("description", "context_property", "category"))) %>%
  tidyr::separate_longer_delim(link_vals, ", ") %>% distinct()

join_context_type <- function(tmp, contexts_table, context_id, context_category) {
  tmp <- tmp %>%
    left_join(
    by = c("dataset_id", "context_id"),
    contexts_table %>% 
      dplyr::filter(link_id == "context_id") %>%
      dplyr::select(-link_id) %>%
      dplyr::rename(dplyr::all_of(c("context_category" = "value", "context_id" = "link_vals"))) %>% 
      dplyr::distinct(dataset_id, context_id, .keep_all = TRUE))
}

wide_table <-
  austraits$traits %>%
    left_join(lat_lon, by = c("dataset_id", "location_id")) %>%
    left_join(locations, by = c("dataset_id", "location_id", "location_name")) %>%
    left_join(
      by = c("dataset_id", "treatment_context_id"),
      contexts_tmp %>% 
        dplyr::filter(link_id == "treatment_context_id") %>%
        dplyr::select(-link_id) %>%
        dplyr::rename(dplyr::all_of(c("treatment_contexts" = "value", "treatment_context_id" = "link_vals"))) %>% 
        dplyr::distinct(dataset_id, treatment_context_id, .keep_all = TRUE)) %>%
    left_join(
      by = c("dataset_id", "plot_context_id"),
      contexts_tmp %>% 
        dplyr::filter(link_id == "plot_context_id") %>%
        dplyr::select(-link_id) %>%
        dplyr::rename(dplyr::all_of(c("plot_contexts" = "value", "plot_context_id" = "link_vals"))) %>% 
        dplyr::distinct(dataset_id, plot_context_id, .keep_all = TRUE)) %>%  
    left_join(
      by = c("dataset_id", "entity_context_id"),
      contexts_tmp %>% 
        dplyr::filter(link_id == "entity_context_id") %>%
        dplyr::select(-link_id) %>%
        dplyr::rename(dplyr::all_of(c("entity_contexts" = "value", "entity_context_id" = "link_vals"))) %>% 
        dplyr::distinct(dataset_id, entity_context_id, .keep_all = TRUE)) %>%
    left_join(
      by = c("dataset_id", "temporal_context_id"),
      contexts_tmp %>% 
        dplyr::filter(link_id == "temporal_context_id") %>%
        dplyr::select(-link_id) %>%
        dplyr::rename(dplyr::all_of(c("temporal_contexts" = "value", "temporal_context_id" = "link_vals"))) %>% 
        dplyr::distinct(dataset_id, temporal_context_id, .keep_all = TRUE)) %>%
    left_join(
      by = c("dataset_id", "method_context_id"),
      contexts_tmp %>% 
        dplyr::filter(link_id == "method_context_id") %>%
        dplyr::select(-link_id) %>%
        dplyr::rename(dplyr::all_of(c("method_contexts" = "value", "method_context_id" = "link_vals"))) %>% 
        dplyr::distinct(dataset_id, method_context_id, .keep_all = TRUE)) %>%
    left_join(austraits$methods %>% dplyr::select(-dplyr::all_of(c("data_collectors"))), by = c("dataset_id", "trait_name", "method_id")) %>%
    left_join(austraits$taxa, by = c("taxon_name")) %>%
    left_join(austraits$taxonomic_updates, by = c("taxon_name", "dataset_id", "original_name")) %>%
    dplyr::select(-dplyr::all_of(c("location_id", "method_id", "method_context_id", "treatment_context_id", "plot_context_id", "temporal_context_id", "entity_context_id")))

  wide_table

}
  