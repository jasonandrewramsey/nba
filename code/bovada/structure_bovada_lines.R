# !diagnostics off
#Feed doc
structure.bovada.lines <- function(.data) {
  #Feed doc > events into this function
  structure_events <- function(.data) {
    #Feed doc > events > displayGroups into this function
    structure_displayGroups <- function(.data) {
      #Feed doc > events > displayGroups > markets into this function
      structure_markets <- function(.data) {
        #Feed doc > events > displayGroups > markets > outcomes into this function
        structure_outcomes <- function(.data) {
          if(class(.data) == 'data.frame') {
            if(nrow(.data) > 0) {
              list(
                .data %>%
                  dplyr::select(-price) %>%
                  setNames(paste0('outcomes_', names(.))),
                
                .data %>%
                  .$price %>%
                  setNames(paste0('price_', names(.)))
              ) %>%
                invoke(cbind.data.frame, .)
            } else {
              data.frame(price_id = NA)
            }  
          } else {
            data.frame(price_id = NA)
          }
        }
        
        
        tryCatch({
          list(
            .data %>%
              dplyr::select(-outcomes, -period) %>%
              setNames(paste0('markets_', names(.))),
            .data %>%
              .$period %>%
              setNames(paste0('period_', names(.)))
          ) %>%
            invoke(cbind.data.frame, .) %>%
            mutate(index = row_number()) %>%
            
            left_join(
              Map(cbind.data.frame, 
                  .data %>%
                    .$outcomes %>%
                    map(., ~structure_outcomes(.)),
                  
                  data.frame(index = 1:length(.data %>% .$outcomes)) %>%
                    split(., .$index)
              ) %>%
                invoke(plyr::rbind.fill, .),
              by = 'index'
            ) %>%
            dplyr::select(-index)
          
        }, error = function(e) { print('Markets Structure Failed') })
      }
      
      tryCatch({
        .data %>% 
          dplyr::select(-markets) %>%
          setNames(paste0('displayGroups_', names(.))) %>%
          mutate(index = row_number()) %>%
          left_join(
            suppressWarnings(Map(cbind.data.frame,
                                 lapply(.data %>% .$markets, structure_markets),
                                 data.frame(index = 1:length(.data %>% .$markets)) %>%
                                   split(., .$index)
            )) %>%
              invoke(plyr::rbind.fill, .),
            by = 'index'
          ) %>%
          dplyr::select(-index)
      }, error = function(e) { print("displayGroups Structuring Failed")})
    }
    
    #Feed events > competitors into this function
    structure_competitors <- function(.data) {
      tryCatch({
        if(class(.data) == 'data.frame') {
          if(nrow(.data) > 0) {
            list(
              .data %>%
                dplyr::select(-one_of('pitcher')) %>%
                setNames(paste0('competitors_', names(.))),
              if('pitcher' %in% colnames(.data)) {
                suppressWarnings(.data %>%
                                   .$pitcher %>%
                                   setNames(paste0('pitcher_', colnames(.))))
              } else {
                data.frame(pitcher_id = NA)
              }
            ) %>%
              invoke(cbind.data.frame,.)
          } else {
            data.frame(competitors_id = NA) 
          }
        } else {
          data.frame(competitors_id = NA) 
        }
      }, error = function(e) { print('Competitor Structure Failed') })
    }
    
    .data %>%
      dplyr::select(-displayGroups, -competitors, -sport) %>%
      mutate(startTime = as.POSIXct(startTime/1000, origin = '1970-01-01'),
             lastModified = as.POSIXct(lastModified/1000, origin = '1970-01-01')) %>%
      setNames(paste0('events_', names(.))) %>%
      mutate(index = row_number()) %>%
      left_join(
        Map(
          cbind.data.frame,
          .data %>% 
            .$competitors %>%
            map(., ~structure_competitors(.)),
          data.frame(index = 1:length(.data %>% .$competitors)) %>%
            split(., .$index)
        ) %>% 
          invoke(plyr::rbind.fill, .),
        by = 'index'
      ) %>%
      
      left_join(
        suppressWarnings(Map(cbind.data.frame, 
                             lapply(.data %>% .$displayGroups, structure_displayGroups),
                             data.frame(index = 1:length(.data %>% .$displayGroups)) %>%
                               split(., .$index)
        )) %>%
          invoke(plyr::rbind.fill, .),
        by = 'index'
      ) %>%
      dplyr::select(-index)
  }
  
  #Function to structure the path
  structure_path <- function(.data) {
    .data %>%
      .$path %>%
      map(.,  ~summarise_all(., first)) %>%
      invoke(rbind, .) %>%
      dplyr::select(-leaf, -current, -type, -order) %>%
      setNames(paste0('path_', colnames(.))) %>%
      mutate(index = row_number())
    
  }
  .data %>%
    structure_path() %>%
    left_join(
      suppressWarnings(Map(
        cbind.data.frame,
        .data %>% 
          .$events %>%
          map(., ~structure_events(.)),
        data.frame(index = 1:length(.data %>% .$events)) %>%
          split(., .$index)
      )) %>%
        invoke(plyr::rbind.fill, .),
      by = 'index'
    ) %>%
    dplyr::select(-index)
}