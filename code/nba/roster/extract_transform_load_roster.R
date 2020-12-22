# !diagnostics off
#Function to structure the nba rosters
structure.nba.roster <- function(.data) {
  #Object reassignment
  idf <- .data
  tryCatch({
    idf %>%
      .$t %>%
      .$pl %>%
      mutate(teamId = idf$t$tid,
             teamTricode = idf$t$ta,
             teamName = idf$t$tn,
             teamCity = idf$t$tc)
  }, error = function(e) {})
}

#Backup html scrape lineup
backup.html.scrape.lineup <- function(param_list) {
  #Scrape the html page
  html_tbl <- 
    paste0('https://www.nba.com/team/', param_list$teamId, '/', param_list$teamSlug) %>%
    read_html() %>%
    html_table(fill = T)
  
  #Structure the html table
  html_tbl[[1]] %>%
    rowwise() %>%
    mutate(fn = str_split(Player, ' ') %>% unlist %>% first(),
           ln = str_replace_all(Player, paste0('^', fn, ' '), ''),
           wt = str_replace_all(Weight, c(' lbs'='')),
           pid = paste0('dummy_', paste(sample(c(letters, 0:9), 6, replace = F), collapse = ''))) %>%
    as.data.frame %>%
    mutate(num = `#`,
           pos = Pos,
           ht = Height,
           hcc = School,
           y = Exp,
           twc = 0,
           dob = as.Date(Birthdate, format = '%b %d, %Y'),
           teamId = param_list$teamId,
           teamTricode = param_list$teamTricode,
           teamCity = param_list$teamCity,
           teamName = param_list$teamName) %>%
    dplyr::select(fn, ln, pid, num, pos, dob, ht, wt, y, twc, hcc, 
                  teamId, teamTricode, teamName, teamCity)
}

#Function to extract and structure nba rosters
extract.structure.nba.roster <- function(teamSlug) {
  GET(paste0('https://data.nba.com/data/v2015/json/mobile_teams/nba/2020/teams/', teamSlug, '_roster.json')) %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.) %>%
    structure.nba.roster()
}

#Function to get rosters of today's games
extract.transform.load.nba.roster <- function(path) {
  #Read in the 1:1 team info from the matchup table
  teamInfo <- 
    readRDS(paste0(path, 'data/nba/matchupInfo.rds')) %>%
    dplyr::select(teamId, teamTricode, teamName, teamCity, teamSlug) %>%
    distinct()
  
  saveRDS(
  lapply(1:nrow(teamInfo), function(i) {
    res <- tryCatch({
      extract.structure.nba.roster(teamInfo$teamSlug[i])
    }, error = function(e) { })
    
    if(is.null(res)) {
      backup.html.scrape.lineup(teamInfo[i,] %>% as.list())
    } else {
      res
    }
  }) %>%
    invoke(rbind, .),
  paste0(path, 'data/nba/rosterInfo.rds')
  )
}