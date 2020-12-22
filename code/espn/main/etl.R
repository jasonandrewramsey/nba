# !diagnostics off
extract.transform.load.espn <- function(path) {
  #Load the structuring functions
  source(paste0(path, 'code/espn/structure_espn_depth_charts.R'))
  #Execute
  espn_depth <- scrape.structure.espn.depth(path)
  #Save
  saveRDS(espn_depth, paste0(path, 'data/espn/depthChart.rds'))
}