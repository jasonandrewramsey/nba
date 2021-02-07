#Function to unnest nested data frames (single)
hm.unnest.df <- function(.data, string_value) {
  tryCatch({
    idf <- .data[[string_value]]
    
    idf %>%
      setNames(paste0(string_value, '_', make.names(names(.))))
  }, error = function(e) {})
}

#Function to unnest data frames (vectorized)
hm.unnest.all.df <- function(.data, string_values) {
  #Call data
  idf <- .data
  string_values %>%
    map(., ~hm.unnest.df(idf, .)) %>%
    invoke(cbind.data.frame, .)
}


#Get the column names
get.col.struct <- function(.data) {
  cn <- colnames(.data)
  
  cn %>%
    as.data.frame %>%
    cbind.data.frame(., 
                     lapply(1:length(cn), function(x) {
                       class(.data[[cn[x]]])
                     }) %>%
                       invoke(c, .)
    ) %>%
    setNames(c('column_name', 'column_class'))
  
}

#Function to unnest 1 level
unnest.current.lvl <- function(.data) {
  #Object environment reassignment
  idf <- .data
  
  #Get the column structure
  column_struct <- 
  idf %>%
  get.col.struct() 
  
  #Unnest current level
  unnested_level_df <-
    list(
      idf %>%
        dplyr::select(
          one_of(
            column_struct %>%
              filter(column_class != 'data.frame') %>%
              .$column_name %>%
              paste
          )
        ),
     idf %>%
        hm.unnest.all.df(., string_values = column_struct %>% filter(column_class == 'data.frame') %>% .$column_name %>% paste)
    ) %>%
    plyr::compact() %>%
    invoke(cbind.data.frame, .) %>%
    setNames(names(.) %>% str_replace_all(., c('[\\.]{1,10}'='_')))
  
  return(unnested_level_df)
}


#Function to unnest all levels until it is a valid frame
unnest.all.levels <- function(.data) {
  #Object environment reassignment
  idf <- .data
  
  #Get the column structure
  column_struct <- 
    idf %>%
    get.col.struct() 
  
  while(any(column_struct$column_class == 'data.frame')) {
    idf <-
      idf %>%
      unnest.current.lvl()
    
    column_struct <- 
      idf %>%
      get.col.struct() 
  }
  
  return(idf)
}

#Function to structure projections
structure.projections <- function(.data) {
  proj_df <- .data$projection
  lapply(1:ncol(proj_df), function(i) {
    idf <- .data$projection[[colnames(proj_df)[i]]]
    list(
      idf %>%
        dplyr::select(leagueId, teamId, playerId),
      idf %>%
        .$statistic %>%
        setNames(paste0('stat_', names(.))),
      idf %>%
        .$summary
    ) %>%
      invoke(cbind.data.frame, .)
    
  }) %>%
    invoke(rbind, .)
}

#Function to structure the player props
structure.player.props.projections <- function(.data) {
  .data %>% 
    dplyr::select(-projection) %>%
    unnest.all.levels() %>%
    left_join(
      .data %>%
        structure.projections(),
      by = c('id'='playerId', 'leagueId'='leagueId', 'team_id'='teamId')
    ) %>%
    dplyr::select(
      one_of(
        c(
          'id', 'created_at', 'isActive', 'isOverUnderEnabled', 'leagueId', 'sportId', 'status',
          'name', 'number', 'updatedAt', 'position_id', 'position_title', 'team_id', 'team_abbreviation',
          'team_title', 'stat_valueId', 'stat_title', 'calculated', 'manualSS', 'manualH2H', 'manualOU', 'manualInPlay'
        )
      )
    )
  
  
}

df <-
  GET('https://widgets.digitalsportstech.com/api/players?isActive=1&isEnabled=1') %>%
  content(., "text", encoding = 'UTF-8') %>%
  fromJSON(.) %>%
  .$data %>%
  structure.player.props.projections(.)




df %>%
  filter(grepl('doncic', name, ignore.case = T))





dk_score_index <-
data.frame(
  points = c(.1, 1, 6, .1, .04, 4),
  stat_title = c('Receiving Yards', 'Receptions', 'Touchdowns', 'Rushing Yards', 'Passing Yards', 'Passing TDs'),
  stringsAsFactors = F
)

nfl_projections <- 
  df %>%
  #filter(grepl('mahomes', name, ignore.case = T)) %>%
  filter(leagueId == 142) %>%
  mutate(field = calculated,
         field = ifelse(is.na(field), 0, field),
         pass_spec_points = ifelse(stat_title == 'Passing Yards', ifelse(field > 300, 3, 0), 0),
         rush_spec_points = ifelse(stat_title == 'Rushing Yards', ifelse(field > 100, 3, 0), 0),
         rec_spec_points = ifelse(stat_title == 'Receiving Yards', ifelse(field > 100, 3, 0), 0)) %>%
  inner_join(
    dk_score_index,
    by = 'stat_title'
  ) %>%
  mutate(dk_points = (points * field)+pass_spec_points+rush_spec_points+rec_spec_points) %>%
  group_by(id, name, number, team_title, position_title) %>%
  summarise(total_points = sum(dk_points))





#Load the structuring functions
source(paste0(path, 'code/draftkings/structure_draftkings.R'))

#Extract draftkings data and structure
dk <-
  GET(paste0('https://api.draftkings.com/draftgroups/v1/draftgroups/', 42821, '/draftables?format=json')) %>%
  content(., "text", encoding = 'UTF-8') %>%
  fromJSON(.) %>%
  structure.draftkings(.) %>%
  mutate(timestamp = now()) %>%
  dplyr::select(firstName, lastName, displayName, shortName, playerId, position, teamId, status, salary) %>%
  distinct()




nfl_projections <-
nfl_projections %>%
  mutate(dk_salary = dk$salary[match(name, paste(dk$displayName))]) %>%
  filter(!is.na(dk_salary))




nfl_projections %>%

  #filter(position_title == 'TE') %>%
  group_by(position_title) %>%
  mutate(points_scaled = (total_points-min(total_points))/(max(total_points)-min(total_points)),
         dk_scaled = (dk_salary-min(dk_salary))/(max(dk_salary)-min(dk_salary))) %>%
  as.data.frame %>%
  mutate(value = points_scaled-dk_scaled) %>%
  arrange(desc(value)) %>%
  filter(value > 0) %>%
  ggplot() +
  geom_point(aes(dk_salary, value)) +
  geom_text(aes(x=dk_salary, y=value, label=name)) +
  facet_wrap(~position_title, scales = "free")

return((x- min(x)) /(max(x)-min(x)))


nfl_projections %>%
  
  #filter(position_title == 'TE') %>%
  group_by(position_title) %>%
  mutate(points_scaled = (total_points-min(total_points))/(max(total_points)-min(total_points)),
         dk_scaled = (dk_salary-min(dk_salary))/(max(dk_salary)-min(dk_salary))) %>%
  as.data.frame %>%
  mutate(value = points_scaled-dk_scaled) %>%
  arrange((value)) %>%
  filter(value < 0) %>%
  ggplot() +
  geom_point(aes(total_points, value)) +
  geom_text(aes(x=total_points, y=value, label=name)) +
  facet_wrap(~position_title, scales = "free")




nfl_projections %>%
  
  #filter(position_title == 'TE') %>%
  group_by(position_title) %>%
  mutate(points_scaled = (total_points-min(total_points))/(max(total_points)-min(total_points)),
         dk_scaled = (dk_salary-min(dk_salary))/(max(dk_salary)-min(dk_salary))) %>%
  mutate(value = points_scaled-dk_scaled) %>%
  filter(value < 0) %>%
  arrange((value)) %>%
  mutate(rnk = row_number()) %>%
  as.data.frame %>%
  filter(rnk <= 5) %>%
  split(., .$position_title)
  
    
    
    
    
    
nfl_projections %>%
  
  #filter(position_title == 'TE') %>%
  group_by(position_title) %>%
  mutate(points_scaled = (total_points-min(total_points))/(max(total_points)-min(total_points)),
         dk_scaled = (dk_salary-min(dk_salary))/(max(dk_salary)-min(dk_salary))) %>%
  mutate(value = points_scaled-dk_scaled) %>%
  #filter(value < 0) %>%
  arrange((value)) %>%
  mutate(rnk = row_number()) %>%
  as.data.frame %>%
  filter(team_title == 'Philadelphia Eagles')
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
