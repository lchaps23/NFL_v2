load_pbp <- function(
    seasons = 1999:2025,
    cache = TRUE
) {
  nflreadr::load_pbp(seasons, cache = cache)
}