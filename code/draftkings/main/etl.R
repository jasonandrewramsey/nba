# !diagnostics off
#Function to extract/structure/store draft kings data
extract.structure.store.draftkings <- function(path) {
  #Load the structuring functions
  source(paste0(path, 'code/draftkings/structure_draftkings.R'))
  
  #Get the draft group to use
  draftGroup <- get.dk.draft.group()
  
  #Extract draftkings data and structure
  dk <-
    GET(paste0('https://api.draftkings.com/draftgroups/v1/draftgroups/', draftGroup, '/draftables?format=json')) %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.) %>%
    structure.draftkings(.) %>%
    mutate(timestamp = now())
  
  #Store draftkings extract
  saveRDS(dk, paste0(path, 'data/draftkings/', format(now(), '%Y%m%d%H%M%S'), '.rds'))
}


