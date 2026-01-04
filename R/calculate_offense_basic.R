calculate_offense_basic <- function(
    data
) {
  
  data |> 
    dplyr::filter(
      !is.na(posteam),
      play_type %in% c("pass", "run", "qb_spike", "qb_kneel")
    ) |> 
    
    dplyr::group_by(posteam) |> 
    
    dplyr::summarise(
      plays = n(),
      
      yards = sum(yards_gained, na.rm = TRUE),
      yards_per_play = mean(yards_gained, na.rm = TRUE),
      
      pass_attempts = sum(play_type == "pass"),
      pass_yards    = sum(yards_gained[play_type == "pass"], na.rm = TRUE),
      yards_per_pass = pass_yards / pass_attempts,
      
      rush_attempts = sum(play_type == "run"),
      rush_yards    = sum(yards_gained[play_type == "run"], na.rm = TRUE),
      yards_per_rush = rush_yards / rush_attempts,
      
      tds = sum(touchdown == 1, na.rm = TRUE),
      
      .groups = "drop"
    ) |> 
    
    dplyr::rename(
      team = posteam
    ) |> 
    
    dplyr::arrange(
      -yards
    )
}