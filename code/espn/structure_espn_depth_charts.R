# !diagnostics off
#Function to structure the tables
structure.espn.solo.table <- function(.data) {
  idf <- .data %>% setNames('init_html_dumpster')
  idf$team <- idf$init_html_dumpster[1]
  idf <- idf[-1,]
  idf %>%
    rowwise() %>%
    mutate(player = str_split(init_html_dumpster, ' - ') %>% unlist %>% last,
           position = str_split(init_html_dumpster, ' - ') %>% unlist %>% first,
           depth = str_extract(position, '[0-9]'),
           position = str_replace_all(position, c('[0-9]'=''))) %>%
    dplyr::select(-init_html_dumpster)
}

#Function to clean up the dirty lists into tables
clean.espn.depth.tables <- function(.data) {
  idf <- .data
  idf <- idf[-1]

  lapply(1:length(idf), function(i) { structure.espn.solo.table(idf[[i]]) }) %>%
    invoke(rbind, .)
}

#Function to return the player information from draftkings table
espn.player.to.nba <- function(espn_input_param_list, nba_rosterInfo) {
  #Sub-function to map and return
  map.return <- function(input_string, output_string) {
    input_string <- prep_player_name(input_string)
    output_string <- prep_player_name(output_string)
    which(input_string == output_string)
  }
  
  #Filter draft kings table for the input team
  nba_rosterInfo <- 
    nba_rosterInfo %>%
    filter(teamId == espn_input_param_list$teamId) %>%
    dplyr::select(-teamId) %>%
    rowwise() %>%
    mutate(map_name = str_to_lower(paste(str_split(fn, '') %>% unlist() %>% first(), ln))) %>%
    as.data.frame
  
  #Now try a secondary mapping method if no rows are returned
  mapped_player <- 
    espn_input_param_list %>% invoke(cbind.data.frame, .) %>% dplyr::select(-teamTricode, -teamId, -teamName) %>%
    cbind.data.frame(., 
                     nba_rosterInfo[map.return(nba_rosterInfo$map_name, 
                                               str_to_lower(str_replace_all(espn_input_param_list$player, c('[\\.] '=' ')))),]
    ) %>%
    dplyr::select(-map_name)
  
  return(mapped_player)
}

#Scrape and structure espn depth chart
scrape.structure.espn.depth <- function(path) {
  #Load rosterInfo from nba
  rosterInfo <-  
    readRDS(paste0(path, 'data/nba/rosterInfo.rds'))
  
  #Load team info from the rosterInfo
  teamInfo <- 
    rosterInfo %>%
    dplyr::select(teamId, teamTricode, teamName, teamCity) %>%
    distinct()
  
  #Scrape structure espn depth chart and merge with the nba.com team ref data
  espn_depth_chart <-
    'http://www.espn.com/nba/depth/_/type/full' %>%
    read_html() %>%
    html_table(fill = T) %>%
    clean.espn.depth.tables() %>%
    mutate(team = ifelse(team == 'LA Clippers', 'LA', team),
           team = ifelse(team == 'LA Lakers', 'Los Angeles', team)) %>%
    left_join(
      teamInfo,
      by = c('team'='teamCity')
    ) %>%
    dplyr::select(-team)
  
  lapply(1:nrow(espn_depth_chart), function(x) {
    tryCatch({espn.player.to.nba(
      espn_input_param_list = espn_depth_chart[x,] %>% as.list(),
      nba_rosterInfo = rosterInfo
    )}, error = function(e) {})
  }) %>%
    plyr::compact() %>%
    invoke(rbind, .)
}