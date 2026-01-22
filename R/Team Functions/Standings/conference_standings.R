source("R/Data Loaders/load_schedule_data.R")

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
    
    dplyr:: mutate(
      win_pct = round(win_pct, 3),
      div_pct = round(div_pct, 3),
      conf_pct = round(conf_pct, 3),
      sov      = round(sov, 3),
      sos      = round(sos, 3)
    ) |> 
    
    dplyr::rename(
      team_abbr = team
    ) |> 
    
    dplyr::arrange(
      conf_rank
    )
}

