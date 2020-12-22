# !diagnostics off
#Function to structure the NBA schedule info
structure.nba.schedule <- function(.data) {
  #Object reassignment
  idf <- .data
  
  #Iterate over each and compile the necessary information
  aggregate_schedule <- 
    lapply(1:length(idf$leagueSchedule$gameDates$games), function(i) {
      #Create the list of home/away info
      home_away_team_ref <-
        list(
          home = 
            idf$leagueSchedule$gameDates$games[[i]] %>%
            .$homeTeam %>%
            mutate(home_away_flag = 'home') %>%
            cbind.data.frame(
              .,
              idf$leagueSchedule$gameDates$games[[i]] %>% dplyr::select(gameId)
            ),
          
          away = 
            idf$leagueSchedule$gameDates$games[[i]] %>%
            .$awayTeam %>%
            mutate(home_away_flag = 'away') %>%
            cbind.data.frame(
              .,
              idf$leagueSchedule$gameDates$games[[i]] %>% dplyr::select(gameId)
            )
        )
      
      #(Sloppy) Add in an opponent ID column to each
      home_away_team_ref$home$opponentId <- home_away_team_ref$away$teamId
      home_away_team_ref$away$opponentId <- home_away_team_ref$home$teamId
      
      
      #Output the information
      list(
        gameInfo = 
          idf$leagueSchedule$gameDates$games[[i]] %>%
          mutate(home_teamId = .$homeTeam$teamId,
                 away_teamId = .$awayTeam$teamId,
                 gameDate = idf$leagueSchedule$gameDates$gameDate[i]) %>%
          dplyr::select(-broadcasters, -homeTeam, -awayTeam, -pointsLeaders),
        
        matchupInfo =
          home_away_team_ref %>%
          invoke(rbind, .)
      )
    })
  
  return(
    list(
      gameInfo = aggregate_schedule %>% map(1) %>% invoke(rbind, .),
      matchupInfo = aggregate_schedule %>% map(2) %>% invoke(rbind, .)
    )
  )
}

#Function to extract, transform the NBA schedule info
extract.transform.load.nba.schedule <- function(path) {
  
  ###TODO
  # Right now this just blindly stores the information, we may need to be more thorough with this
  
  #Extracts nba schedule/team info
  df <- 
    GET('https://cdn.nba.com/static/json/staticData/scheduleLeagueV2.json') %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.) %>%
    structure.nba.schedule()
  
  #Stores gameInfo/matchupInfo separately
  for(i in 1:length(df)) {
    saveRDS(df[[i]], paste0(path, 'data/nba/', names(df)[i], '.rds'))
  }
}

