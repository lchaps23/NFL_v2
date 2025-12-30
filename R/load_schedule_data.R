load_schedule_data <- function(
    seasons = 1999:2025,
    rounds = "REG"
) {
  
  nflreadr::load_schedules(seasons) |> 
  
  dplyr::filter(
    game_type %in% rounds
  ) 
}