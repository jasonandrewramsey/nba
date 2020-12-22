extract.transform.load.bovada <- function(path) {
  #Source the bovada transformation functions (structuring JSON into DF)
  source(paste0(path, 'code/bovada/structure_bovada_lines.R'))
  #Source the bovada trandformation functions (structuring merged DF into separate ref tables)
  source(paste0(path, 'code/bovada/construct_bovada_ref_tables.R'))
  #Source the storage functions
  source(paste0(path, 'code/bovada/store_bovada_ref_tables.R'))
  
  
  #Extract the full bovada game lines available
  GET('https://services.bovada.lv/services/sports/event/coupon/events/A/description/?FilterId=def&lang=en') %>%
    content(., "text", encoding = 'UTF-8') %>%
    fromJSON(.) %>%
    
    #Structure/filter
    structure.bovada.lines(.) %>%
    filter(path_description == 'NBA',
           events_live == F,
           events_type == 'GAMEEVENT') %>%
    
    #Transform/Load
    build.bovada.ref.tables() %>%
    store.bovada.ref.data(., path = path)
}