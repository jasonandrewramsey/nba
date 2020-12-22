# !diagnostics off
#Function to store bovada ref data object
store.bovada.ref.data <- function(path, .data) {
  #Object reassignment
  idf <- .data
  
  #Assign the sub-folder path
  sub_path <- 'data/bovada/'
  
  #Create the indexes used to find new records
  ref_primary_keys <- 
    list(
      ref_competitors = c('path_link', 'competitor_id', 'competitor_name'),
      ref_events = c('path_link', 'event_id'),
      ref_events_competitors = c('event_id', 'competitor_id'),
      ref_outcomes = c('path_link', 'event_id', 'competitor_id', 'displayGroup_id', 'market_key', 'period_description', 'outcome_id', 'outcome_description', 'price_id', 'price_decimal')
    )
  
  #Read in the historical ref data on flat files
  historical_ref_data <-
  list.files(paste0(path, sub_path)) %>%
    map(., ~readRDS(paste0(path, sub_path, .))) %>%
    setNames(list.files(paste0(path, sub_path)) %>%
               str_replace_all(., c('.rds'='')))
  
  #Go through each reference data area and find the NEW records, append and store
  for(x in 1:length(idf)) {
    data_area <- names(idf)[x]
    
    #Anti join/Append to append only new records to the reference table
    ref_data <-
      list(
        historical_ref_data[[data_area]],
        idf[[data_area]] %>%
          anti_join(
            historical_ref_data[[data_area]],
            by = ref_primary_keys[[data_area]]
          )
      ) %>%
      invoke(rbind, .)
    
    #Store the appended unique frame
    saveRDS(ref_data, paste0(path, sub_path, data_area, '.rds'))
  }
}







