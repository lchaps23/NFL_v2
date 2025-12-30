load_pbp_data <- function(
    seasons = 1999:2025
) {
  nflreadr::load_pbp(seasons)
}
