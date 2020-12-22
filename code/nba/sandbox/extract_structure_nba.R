# !diagnostics off
#Function to structure today's scoreboard
structure.nba.api.scoreboard <- function(.data) {
  #Object reassignment
  idf <- .data
  
  #Structure the team information
  ref_team <-
    list(
      idf %>%
        .$scoreboard %>%
        .$games %>%
        .$homeTeam %>%
        dplyr::select(teamId, teamName, teamCity, teamTricode),
      idf %>%
        .$scoreboard %>%
        .$games %>%
        .$awayTeam %>%
        dplyr::select(teamId, teamName, teamCity, teamTricode)
    ) %>%
    invoke(rbind, .) %>%
    distinct()
  
  #Structure the game information
  ref_game <-
    idf %>%
    .$scoreboard %>%
    .$games %>%
    mutate(homeTeamId = .$homeTeam$teamId,
           awayTeamId = .$awayTeam$teamId) %>%
    dplyr::select(-homeTeam, -awayTeam, -pbOdds, -gameLeaders)
  
  #Output a list
  return(
    list(
      ref_gameDate = idf %>% .$scoreboard %>% .$gameDate,
      ref_game = ref_game,
      ref_team = ref_team
    )
  )
}


#Function to extract and structure nba rosters
extract.structure.nba.roster <- function(team_name) {
  GET(paste0('https://data.nba.com/data/v2015/json/mobile_teams/nba/2020/teams/', team_name, '_roster.json')) %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.) %>%
    structure.nba.roster()
}

#Function to get rosters of today's games
get.game.full.rosters <- function(scoreboard_output, full_team_mapping) {
  ref_today_active_teams <-
    scoreboard_output %>%
    .$ref_team %>%
    mutate_all(., ~paste(.)) %>%
    inner_join(
      full_team_mapping,
      by = c('teamId'='nba_team_id')
    )
  ref_today_active_teams %>%
    .$nba_roster_team_abbr %>%
    map(., ~extract.structure.nba.roster(.)) %>%
    invoke(rbind, .)
}









