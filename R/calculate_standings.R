calculate_standings <- function(
    season = 2025,
    rounds = "REG"
) {
  
  schedule <- load_schedule_data(
    seasons = season,
    rounds  = rounds
  )
  
  completed_games <- schedule |> 
    dplyr::filter(
      !is.na(home_score),
      !is.na(away_score)
    )
  
  team_results <- completed_games |> 
    dplyr::mutate(
      home_result = dplyr::case_when(
        home_score > away_score ~ "W",
        home_score < away_score ~ "L",
        TRUE                    ~ "T"
      ),
      away_result = dplyr::case_when(
        away_score > home_score ~ "W",
        away_score < home_score ~ "L",
        TRUE                    ~ "T"
      ),
    ) |> 
    
    dplyr::select(
      season,
      home_team,
      home_score,
      home_result,
      away_team,
      away_score,
      away_result
    ) |> 
    
    tidyr::pivot_longer(
      cols     = c(home_team, 
                   away_team),
      names_to = "team_type",
      values_to = "team"
    ) |> 
    
    dplyr::mutate(
      result = ifelse(
        team_type == "home_team",
        home_result,
        away_result),
      points_for = ifelse(
        team_type == "home_team",
        home_score,
        away_score),
      points_against = ifelse(
        team_type == "home_team",
        away_score,
        home_score)
    ) |> 
    
    dplyr::select(
      season,
      team,
      result,
      points_for,
      points_against
    )
  
  standings <- team_results |> 
    dplyr::group_by(team) |> 
    
    dplyr::summarise(
      P     = dplyr::n(),
      W     = sum(result == "W"),
      L     = sum(result == "L"),
      T     = sum(result == "T"),
      PF    = sum(points_for),
      PA    = sum(points_against),
      PD    = PF - PA,
      W_PCT = (W + (0.5 * T)) / P,
      .groups = "drop"
    ) |> 
    
    dplyr::arrange(
      -W_PCT,
      -PD
    )
  
  standings
  
}