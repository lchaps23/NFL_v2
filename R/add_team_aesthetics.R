add_team_aesthetics <- function(df) {
  
  if (!"team_abbr" %in% names(df)) stop("df must have team_abbr")
  df$team_abbr <- as.character(df$team_abbr)
  
  teams <- nflreadr::load_teams()
  if (!"team_abbr" %in% names(teams)) stop("load_teams() missing team_abbr")
  teams$team_abbr <- as.character(teams$team_abbr)
  
  df |> dplyr::left_join(teams, by = "team_abbr")
}