season_division_race <- function(
    season,
    conference,
    rounds = "REG"
) {
  
  games <- load_schedule_data(
    seasons = season,
    rounds = rounds
  ) |> 
    
    dplyr::filter(
      !is.na(home_score),
      !is.na(away_score)
    )
  
  max_week <- max(games$week, na.rm = TRUE)
  
  purrr::map_dfr(
    1:max_week,
    function(wk) {
      
      division_race(
        season       = season,
        through_week = wk,
        rounds       = rounds,
        ranks        = "DRAFT",
      ) |> 
        
        dplyr::filter(
          conf == !!conference
        ) |> 
        
        dplyr::mutate(
          week = wk
        )
    })
}