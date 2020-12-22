# !diagnostics off

# Packages & Dependencies -------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, zoo, reshape2, lubridate, httr, jsonlite)

# Path Assignment ---------------------------------------------------------

path <- './'

# ETLs ----------------------------------------------------------

#nba.com
source(paste0(path, 'code/nba/main/etl.R'))
extract.transform.load.nba.com(path)

#bovada
source(paste0(path, 'code/bovada/main/etl.R'))
extract.transform.load.bovada(path)

#Bovada ETL Process functions
source(paste0(path, 'code/bovada/extract_transform_load_bovada.R'))

#Team mapping file
full_team_mapping <- readRDS(paste0(path, 'data/full_team_mapping.rds'))


# Extract Transform Load NBA.COM ------------------------------------------

# Scrape/structure scoreboard data from nba.com
scoreboard <-
GET('https://cdn.nba.com/static/json/liveData/scoreboard/todaysScoreboard_00.json') %>%
  content(., "text", encoding = 'UTF-8') %>%
  fromJSON(.) %>%
  structure.nba.api.scoreboard()

#Get the full day's rosters
rosters <-
  get.game.full.rosters(scoreboard, full_team_mapping = full_team_mapping)


# Extract Transform Load Bovada -------------------------------------------

extract.transform.bovada.ref.tables(path)










