# !diagnostics off

# Functions ---------------------------------------------------------------
#Build competitor_frame many:1 w/ name and id
build.competitor.frame <- function(.data) {
  .data %>%
    filter(!is.na(competitors_id)) %>%
    dplyr::select(path_link, competitors_name, competitors_id, events_id) %>%
    distinct() %>%
    mutate(competitor_id = 
             competitors_id %>%
             str_replace_all(., events_id, '') %>%
             str_replace_all(., '[-]', ''),
           competitor_name =
             competitors_name %>%
             str_trim() %>%
             str_replace_all(., c('[(][#][0-9]+[)]$'='')) %>%
             str_trim()) %>%
    dplyr::select(path_link, competitor_id, competitor_name) %>%
    filter(!is.na(competitor_id)) %>%
    na.omit() %>%
    distinct()
}


#Build the events frame
build.event.frame <- function(.data) {
  .data %>%
    filter(!is.na(competitors_id)) %>%
    dplyr::select(path_id, path_link, path_description, path_sportCode, event_id=events_id, event_startTime=events_startTime, 
                  event_description=events_description, event_awayTeamFirst=events_awayTeamFirst) %>%
    distinct() %>%
    rowwise() %>%
    mutate(team_one =
             event_description %>%
             str_split(., '[@]') %>%
             unlist() %>%
             first() %>%
             str_trim() %>%
             str_replace_all(., c('[(][#][0-9]+[)]$'='')) %>%
             str_trim(),
           team_two =
             event_description %>%
             str_split(., '[@]') %>%
             unlist() %>%
             last() %>%
             str_trim() %>%
             str_replace_all(., c('[(][#][0-9]+[)]$'='')) %>%
             str_trim(),
           normalized_event_description = ifelse(event_awayTeamFirst == T, paste(team_one, team_two, sep = ' @ '), paste(team_two, team_one, sep = ' @ '))) %>%
    as.data.frame %>%
    dplyr::select(-team_one, -team_two) %>%
    na.omit() %>%
    distinct()
}


#Function to build out the competitor events frame
build.competitor.events <- function(.data) {
  .data %>%
    filter(!is.na(competitors_id)) %>%
    dplyr::select(path_link, competitors_name, competitors_id, competitors_home, events_id) %>%
    distinct() %>%
    mutate(competitor_id = 
             competitors_id %>%
             str_replace_all(., events_id, '') %>%
             str_replace_all(., '[-]', '')
    ) %>%
    dplyr::select(event_id=events_id, competitor_id, competitor_home=competitors_home) %>%
    filter(!is.na(competitor_id)) %>%
    na.omit() %>%
    distinct()
}

#Function to build the outcome frame
build.outcome.frame <- function(.data) {
  .data %>%
    filter(!is.na(competitors_id),
           markets_key %in% c('2W-OU', '2W-HCAP', '2W-12'),
           displayGroups_id %in% c('100-86', '100-41', '100-101', '100-97'),
           !is.na(outcomes_id)) %>%
    distinct() %>%
    mutate(competitor_id = 
             competitors_id %>%
             str_replace_all(., events_id, '') %>%
             str_replace_all(., '[-]', ''),
           competitor_name =
             competitors_name %>%
             str_trim() %>%
             str_replace_all(., c('[(][#][0-9]+[)]$'='')) %>%
             str_trim(),
           competitor_id_v2 = 
             outcomes_competitorId %>% 
             str_replace_all(., paste0(events_id, '-'), ''),
           outcome_description = 
             outcomes_description %>%
             str_replace_all(., c(' - (1H|1Q|2H|2Q)'='')) %>% 
             str_trim() %>%
             str_replace_all(., c('[(][#][0-9]+[)]$'='')) %>%
             str_trim(),
           team_total_flag = 
             ifelse(markets_key == '2W-OU' & displayGroups_description %in% c('Alternate Lines', 'Period/Alternate Lines') & 
                      !is.na(outcomes_competitorId) & competitor_id == competitor_id_v2, TRUE, FALSE),
           alternate_line_keep_flag = ifelse(str_detect(displayGroups_description,'Alternate') == F, TRUE, team_total_flag == T)
    ) %>%
    filter(alternate_line_keep_flag == T) %>%
    dplyr::select(path_id, path_link, 
                  event_id=events_id,
                  competitor_id,
                  displayGroup_id=displayGroups_id, displayGroup_description=displayGroups_description,
                  market_key=markets_key, market_description=markets_description,
                  period_description, period_abbreviation,
                  outcome_id=outcomes_id, outcome_description,outcome_type=outcomes_type,
                  price_id, price_handicap, price_american, price_decimal,
                  team_total_flag) %>%
    distinct() %>%
    rowwise() %>%
    mutate(market_competitor_detail = ifelse(team_total_flag == T, str_replace_all(market_description, c('Total Points O/U - '='')) %>% str_trim(), NA),
           market_description =
             market_description %>%
             str_replace_all(., c('Total Points O/U - '=''))) %>%
    as.data.frame
}

#Function to build the reference tables list
build.bovada.ref.tables <- function(.data, as_of_ts) {
  if(missing(as_of_ts)) {
    as_of_ts <- now()
  }
  
  list(
    ref_competitors = 
      .data %>% 
      build.competitor.frame(),
    
    ref_events =
      .data %>%
      build.event.frame(),
    
    ref_events_competitors =
      .data %>%
      build.competitor.events(),
    
    ref_outcomes =
      .data %>%
      build.outcome.frame()
  ) %>%
    #Add a timestamp to each of the data elements
    map(., ~mutate(., timestamp = as_of_ts))
}
