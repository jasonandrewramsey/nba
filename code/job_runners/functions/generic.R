#Function to read in the most recent file extracted from draftkings
draftkings.load.most.recent.extract <- function(path) {
  #List the draft kings files
  files <- list.files(paste0(path, 'data/draftkings/'))
  
  #Transform into time class
  files_time <- as.POSIXct(strptime(gsub(".rds", "", files), format = '%Y%m%d%H%M%S'))
  
  #Find max integer file in terms of datetime
  rec_file <- files[which.max(files_time)]
  
  #Load
  return(
    readRDS(paste0(path, 'data/draftkings/', rec_file)) %>%
      dplyr::select(firstName, lastName, displayName, shortName, playerId, position, teamId, status, salary) %>%
      distinct()
  )
}

#Function to load all bovada data
bovada.load.data <- function(path) {
  list.files(paste0(path, 'data/bovada/')) %>%
    map(., ~readRDS(paste0(path, 'data/bovada/', .))) %>%
    setNames(list.files(paste0(path, 'data/bovada/')) %>%
               str_replace_all(., c('.rds'='')))
}

#Define the string structuring function for search
prep_player_name <- function(string) {
  string %>%
    str_to_lower() %>%
    str_replace_all(., c('[:punct:]'='', ' [i]{1,3}$'=''))
}

#Function to return the player information from draftkings table
nba.player.to.dk <- function(nba_input_param_list, draftkings_df) {
  #Sub-function to map and return
  map.return <- function(input_string, output_string) {
    input_string <- prep_player_name(input_string)
    output_string <- prep_player_name(output_string)
    which(input_string == output_string)
  }
  
  #Filter draft kings table for the input team
  dk <- 
    draftkings_df %>%
    filter(teamId == nba_input_param_list$dk_teamId) %>%
    dplyr::select(-teamId)
  
  #Mapped player
  mapped_player <- 
    dk[map.return(paste(nba_input_param_list$fn, nba_input_param_list$ln), paste(dk$firstName, dk$lastName)),] %>%
    setNames(paste0('dk_', names(.)))
  
  #If there is one record, return it
  if(nrow(mapped_player) == 1) { return(mapped_player) }
  
  #Now try a secondary mapping method if no rows are returned
  mapped_player <- 
    dk[map.return(paste(nba_input_param_list$fn %>% strsplit(., "") %>% unlist %>% first, nba_input_param_list$ln), paste(dk$shortName)),] %>%
    setNames(paste0('dk_', names(.)))
  
  #If there is one record, return it
  if(nrow(mapped_player) == 1) { return(mapped_player) }
  
  #if there is no rows, return NULL player ID
  if(nrow(mapped_player) == 0) { return(data.frame(dk_playerId = NA)) }
  return(mapped_player)
}

#Function to map all roster info to draft kings
nba.roster.to.dk <- function(.data, draftkings_df) {
  .data %>%
    cbind.data.frame(., 
                     lapply(1:nrow(.data), function(i) {
                       nba.player.to.dk(nba_input_param_list = as.list(.data[i,]),  draftkings_df = draftkings_df)
                     }) %>%
                       invoke(plyr::rbind.fill, .)
    )
}
