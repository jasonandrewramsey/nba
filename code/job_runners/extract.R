# !diagnostics off
path <- './'

# Load all functions for job runners ----------------------------------------------------------
run_files <- c('packages', 'extract_transform_load_data_areas', 'generic')

invisible(
  run_files %>%
  map(., ~source(paste0(path, 'code/job_runners/functions/', ., '.R')))
)

#Load data areas
extract.transform.load.data.areas(path)

# Key Parameter Declaration -----------------------------------------------
date <- as.Date(now(), tz = 'EST')

# Load key data -----------------------------------------------------------

#Read in all key data elements
data_bin <-
  list(
    ref_mapping = list(team_key_mappings = readRDS(paste0(path, 'data/mapping/bovada_nba_key_relationships.rds')) %>% mutate_all(., ~paste(.))),
    nba = 
      list(
        games = readRDS(paste0(path, 'data/nba/gameInfo.rds')),
        competitors = readRDS(paste0(path, 'data/nba/matchupInfo.rds')),
        rosters =  readRDS(paste0(path, 'data/nba/rosterInfo.rds'))
    ),
    espn = readRDS(paste0(path, 'data/espn/depthChart.rds')),
    bovada = bovada.load.data(path),
    draftkings = draftkings.load.most.recent.extract(path)
  )

# NBA data prep --------------------------------------------------

#Prep game data
games <-
  data_bin %>%
  .$nba %>%
  .$games %>%
  mutate(gameDate = as.Date(gameDate, format = '%m/%d/%Y %H:%M:%S %p', tz = 'EST')) %>%
  filter(gameDate == date) %>%
  rowwise() %>%
  #create the calculated game id -- hometeamid-awayteamid
  mutate(self_gameId = paste(home_teamId, away_teamId, sep = '-')) %>%
  as.data.frame

#Prep competitor data
competitors <-
  data_bin %>%
  .$nba %>%
  .$competitors %>%
  inner_join(
    games %>% dplyr::select(gameId),
    by = 'gameId'
  ) %>%
  mutate(teamId = paste(teamId)) %>%
  #add bovada key reference field
  left_join(
    data_bin$ref_mapping$team_key_mappings %>% dplyr::select(competitor_id = bovada_competitorId, teamId = nba_teamId),
    by = 'teamId'
  )

#Extract the rosters for those games
rosters <-
  data_bin %>%
  .$nba %>%
  .$rosters %>%
  inner_join(
    games %>% 
      dplyr::select(gameId, home_teamId, away_teamId) %>%
      melt(., id.vars = c('gameId')) %>%
      dplyr::select(gameId, teamId = value) %>%
      distinct(),
    by = 'teamId'
  ) %>%
  mutate(teamId = paste(teamId)) %>%
  inner_join(
    data_bin$ref_mapping$team_key_mappings %>% dplyr::select(dk_teamId, teamId=nba_teamId),
    by = 'teamId'
  ) %>%
  nba.roster.to.dk(., draftkings_df = data_bin %>% .$draftkings) %>%
  left_join(
    data_bin$espn %>% dplyr::select(pid, depth),
    by = 'pid'
  )


# Bovada data retrieval ---------------------------------------------------

#Extract the events that occur for our in-scope competitors
#Transform to wide to allow for self_gameId
bovada_scoped_events <- 
  data_bin %>%
  .$bovada %>%
  .$ref_events_competitors %>%
  filter(competitor_id %in% competitors$competitor_id) %>%
  dplyr::select(-timestamp) %>%
  #join with key reference mapping
  left_join(
    data_bin %>% .$ref_mapping %>% .$team_key_mapping %>% dplyr::select(competitor_id = bovada_competitorId, teamId = nba_teamId),
    by = 'competitor_id'
  )

#Create the bovada --> nba game mapping
game_event_mapping <-
bovada_scoped_events %>%
  dplyr::select(-competitor_id) %>%
  mutate(competitor_home = ifelse(competitor_home == T, 'home', 'away')) %>%
  dcast(., event_id~competitor_home, value.var = 'teamId') %>%
  rowwise() %>%
  mutate(self_gameId = paste(home, away, sep = '-')) %>%
  as.data.frame %>%
  inner_join(
    games %>% dplyr::select(self_gameId, gameId),
    by = 'self_gameId'
  ) %>%
  dplyr::select(event_id, gameId)

#Reduce the outcomes reference table to the latest information in scoped events (it changes with each etl if the handicap/odds change)
data_bin$bovada$ref_outcomes <-
  data_bin %>%
  .$bovada %>%
  .$ref_outcomes %>%
  filter(event_id %in% game_event_mapping$event_id) %>%
  group_by(path_id, path_link, event_id, competitor_id, displayGroup_id, displayGroup_description,
           market_key, market_description, period_description, period_abbreviation, outcome_id,
           outcome_description, outcome_type, team_total_flag) %>%
  arrange(desc(timestamp)) %>%
  summarise_all(first) %>%
  as.data.frame()

#Create the wide format line information
wide_line_info <-
  list(
    #Moneyline info
    data_bin %>%
      .$bovada %>%
      .$ref_outcomes %>%
      filter(market_description == 'Moneyline',
             period_description == 'Game') %>%
      mutate(outcome_type = ifelse(outcome_type == 'H', 'home_moneyline_odds', 'away_moneyline_odds')) %>%
      dcast(., event_id+competitor_id~outcome_type, value.var = 'price_decimal'),
    
    #Point Spread info
    data_bin %>%
      .$bovada %>%
      .$ref_outcomes %>%
      filter(market_description == 'Point Spread',
             period_description == 'Game') %>%
      mutate(outcome_type = ifelse(outcome_type == 'H', 'home_point_spread_odds', 'away_point_spread_odds')) %>%
      dcast(., event_id+competitor_id~outcome_type, value.var = 'price_decimal'),
    
    data_bin %>%
      .$bovada %>%
      .$ref_outcomes %>%
      filter(market_description == 'Point Spread',
             period_description == 'Game') %>%
      mutate(outcome_type = ifelse(outcome_type == 'H', 'home_point_spread_handicap', 'away_point_spread_handicap')) %>%
      dcast(., event_id+competitor_id~outcome_type, value.var = 'price_handicap'),
    
    
    #Totals info
    data_bin %>%
      .$bovada %>%
      .$ref_outcomes %>%
      filter(market_description == 'Total',
             period_description == 'Game') %>%
      mutate(outcome_type = ifelse(outcome_type == 'O', 'totals_over_odds', 'totals_under_odds')) %>%
      dcast(., event_id+competitor_id~outcome_type, value.var = 'price_decimal'),
    
    data_bin %>%
      .$bovada %>%
      .$ref_outcomes %>%
      filter(market_description == 'Total',
             period_description == 'Game') %>%
      mutate(outcome_type = ifelse(outcome_type == 'O', 'totals_over_handicap', 'totals_under_handicap')) %>%
      dcast(., event_id+competitor_id~outcome_type, value.var = 'price_handicap')
  ) %>%
  reduce(left_join, by = c('event_id', 'competitor_id')) %>%
  dplyr::select(-competitor_id) %>%
  distinct() %>%
  group_by(event_id) %>%
  summarise_all(first) %>%
  as.data.frame

# Merge -------------------------------------------------------------------

game_info <-
games %>%
  left_join(
    game_event_mapping, 
    by = 'gameId'
  ) %>%
  left_join(
    data_bin$bovada$ref_events %>% dplyr::select(-timestamp),
    by = 'event_id'
  ) %>%
  mutate(home_teamId = paste(home_teamId)) %>%
  left_join(
    competitors %>% 
      filter(home_away_flag == 'home') %>% 
      setNames(paste0('home_', names(.))) %>%
      mutate(home_teamId = paste(home_teamId)),
    by = 'home_teamId'
  ) %>%
  mutate(away_teamId = paste(away_teamId)) %>%
  left_join(
    competitors %>% 
      filter(home_away_flag == 'away') %>% 
      setNames(paste0('away_', names(.))) %>%
      mutate(away_teamId = paste(away_teamId)),
    by = 'away_teamId'
  ) %>%
  left_join(
    wide_line_info,
    by = 'event_id'
  )

# Output ------------------------------------------------------------------

#Output to xlsx
xls = xl.get.excel()
xl.workbook.add()
xl.sheet.add("gameInfo")
rng = xls[["Activesheet"]]$Cells(1,1)
xl.write(game_info,
         rng,row.names = FALSE,col.names = TRUE)

xl.sheet.add("rosterInfo")
rng = xls[["Activesheet"]]$Cells(1,1)
xl.write(rosters,
         rng,row.names = FALSE,col.names = TRUE)

xl.workbook.save(paste0(path, 'data/output/', format(now(), '%Y%m%d%H%M%S'), '.xlsx'))
xl.workbook.close()