load_roster_data <- function(seasons = 2025, positions = NULL) {
  
  rosters_list <- lapply(seasons, function(yr) {
    df <- nflreadr::load_rosters(season = yr) |> 
      dplyr::mutate(season = yr)
    
    if(!is.null(positions)) {
      df <- df |> 
        dplyr::filter(position %in% positions)
    }
    
    df
    
  })
  
  rosters <- rosters_list |> 
    dplyr::bind_rows()
  
  return(rosters)
}