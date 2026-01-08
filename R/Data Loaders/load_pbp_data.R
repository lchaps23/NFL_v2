load_pbp_data <- function(
    seasons = 1999:2025,
    include_postseason = FALSE
) {
  
  pbp <- nflreadr::load_pbp(seasons)
  
  if(!include_postseason) {
    pbp <- pbp |> 
      dplyr::filter(
        season_type == "REG"
      )
  }
}

