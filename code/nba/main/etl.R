# !diagnostics off
#Function to load and execute all extract/transform/loading processes associated with nba.com data
extract.transform.load.nba.com <- function(path) {
  #source the functions
  source(paste0(path, 'code/nba/schedule/extract_transform_load_schedule.R'))
  source(paste0(path, 'code/nba/roster/extract_transform_load_roster.R'))
  
  #Execute the etl processes
  extract.transform.load.nba.schedule(path)
  extract.transform.load.nba.roster(path)
}