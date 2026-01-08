load_player_stats_data <- function(
    seasons
) {
  
  player_stats <- nflreadr::load_player_stats(
    seasons = seasons,
  ) |> 
    
    dplyr::filter(
      season_type == "REG",
      !is.na(team),
      !is.na(opponent_team)
    )
  
}
