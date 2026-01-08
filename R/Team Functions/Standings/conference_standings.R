conference_standings <- function(
    season,
    rounds = c("REG", "WC", "DIV", "CON", "SB"),
    ranks = "DRAFT",
    tiebreaker_depth = "SOS",
    conference = NULL,
    verbosity = "NONE"
) {
  
  games <- load_schedule_data(
    seasons = season,
    rounds  = rounds
  ) |>
    dplyr::filter(
      !is.na(home_score),
      !is.na(away_score)
    )
  
  standings <- nflseedR::nfl_standings(
    games = games,
    ranks = ranks,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )
  
  if (!is.null(conference)) {
    standings <- standings |>
      dplyr::filter(conf == !!conference)
  }
  
  standings |> 
    dplyr::arrange(
      conf_rank
    )
}

