# !diagnostics off
#Sub-Function to convert the nested data frame draftables into a data frame of vectors
structure.draftables <- function(.data) {
  list(
    .data %>%
      .$draftables %>% 
      dplyr::select(-playerAttributes, -draftAlerts, -playerGameAttributes, -teamLeagueSeasonAttributes,
                    -draftStatAttributes, -competitions, -competition),
    .data %>%
      .$draftables %>%
      .$competition %>%
      dplyr::select(competitionId, competitionName = name, competitionStartTime=startTime) %>%
      as.data.frame
  ) %>%
    invoke(cbind.data.frame, .)
}


#Sub function to convert the competitions to a data frame of vectors
structure.competition <- function(.data) {
  list(
    .data %>%
      .$competitions %>%
      dplyr::select(-homeTeam, -awayTeam, -competitionAttributes),
    
    .data %>%
      .$competitions %>%
      .$homeTeam %>%
      setNames(paste0('home_', names(.))),
    
    .data %>%
      .$competitions %>%
      .$awayTeam %>%
      setNames(paste0('away_', names(.)))
  ) %>%
    invoke(cbind.data.frame, .)
}

#Function to structure the information from draft kings into a data frame
structure.draftkings <- function(.data) {
  .data %>%
    structure.draftables(.) %>%
    left_join(
      .data %>%
        structure.competition(.),
      by = 'competitionId'
    )
}

#Function to extract the draftGroup to use for the lineup from draft kings
get.dk.draft.group <- function() {
  df <- 
    GET('https://api.draftkings.com/draftgroups/v1/?format=json') %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.)
  
  list(
    df %>% 
      .$draftGroups %>%
      dplyr::select(draftGroupId, sportId, startTimeSuffix, startTimeType, minStartTime, maxStartTime),
    df %>%
      .$draftGroups %>%
      .$contestType,
    df %>%
      .$draftGroups %>%
      .$games %>%
      map(., ~nrow(.)) %>%
      invoke(c, .) %>%
      as.data.frame %>%
      setNames('gameCount')
  ) %>%
    invoke(cbind.data.frame, .) %>%
    filter(sport == 'NBA',
           contestTypeId == 70) %>%
    filter(gameCount == max(gameCount)) %>%
    .$draftGroupId %>%
    first()
}
