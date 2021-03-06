# !diagnostics off
extract.transform.load.data.areas <- function(path, force_reload) {
  force_reload <- if(missing(force_reload)) { FALSE } else { force_reload }
  
  #nba.com
  source(paste0(path, 'code/nba/main/etl.R'))
  #If nba hasn't been extracted in over 24 hours, extract NBA refreshed schedule/lineups
  if(as.numeric(difftime(now(), file.info(paste0(path, 'data/nba/gameInfo.rds'))$mtime, unit = 'days'))>1 | force_reload == T) {
    extract.transform.load.nba.com(path)
  }
  
  #espn depth
  source(paste0(path, 'code/espn/main/etl.R'))
  if(as.numeric(difftime(now(), file.info(paste0(path, 'data/espn/depthChart.rds'))$mtime, unit = 'days'))>1 | force_reload == T) {
    #extract.transform.load.espn(path)
  }
  
  #bovada
  source(paste0(path, 'code/bovada/main/etl.R'))
  extract.transform.load.bovada(path)
  
  #draft kings
  source(paste0(path, 'code/draftkings/main/etl.R'))
  extract.structure.store.draftkings(path)
  
  #Print progress report
  print(paste0('nba schedule file updated at....', format(file.info(list.files(paste0(path, '/data/nba/'), full.names = T)) %>% arrange(desc(ctime)) %>% .$ctime %>% max(), '%I:%M', tz = 'EST')))
  print(paste0('espn depth file updated at....', format(file.info(list.files(paste0(path, '/data/espn/'), full.names = T)) %>% arrange(desc(ctime)) %>% .$ctime %>% max(), '%I:%M', tz = 'EST')))
  print(paste0('bovada line info file updated at....', format(file.info(list.files(paste0(path, '/data/bovada/'), full.names = T)) %>% arrange(desc(ctime)) %>% .$ctime %>% max(), '%I:%M', tz = 'EST')))
  print(paste0('draftkings file updated at....', format(file.info(list.files(paste0(path, '/data/draftkings/'), full.names = T)) %>% arrange(desc(ctime)) %>% .$ctime %>% max(), '%I:%M', tz = 'EST')))
}









