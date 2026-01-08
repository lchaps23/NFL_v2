division_race <- function(
    season,
    division = NULL,
    through_week = NULL,
    rounds = "REG",
    ranks = "DRAFT",
    tiebreaker_depth = "SOS",
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
  
  if (!is.null(through_week)) {
    games <- games |> dplyr::filter(week <= through_week)
  }
  
  standings <- nflseedR::nfl_standings(
    games = games,
    ranks = ranks,
    tiebreaker_depth = tiebreaker_depth,
    verbosity = verbosity
  )
  
  if (!is.null(division)) {
    standings <- standings |> dplyr::filter(division == !!division)
  }
  
  standings <- standings |>
    dplyr::group_by(division) |>
    dplyr::mutate(
      max_wins = max(wins),
      max_losses = min(losses),  # minimum losses = division leader
      GB = ((max_wins - wins) + (losses - max_losses))/2
    ) |>
    dplyr::arrange(division, div_rank) |>
    dplyr::ungroup()
  
  standings
  
}