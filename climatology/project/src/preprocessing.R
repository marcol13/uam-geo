library(dplyr)

get_measurements_from_year <- function(df, yy) {
  if (!"yy" %in% names(df)) {
    stop("Data frame must contain a 'yy' column.")
  }
  df %>% filter(yy >= 1966)
}

filter_newer_stations <- function(df, yy) {
  if (!"yy" %in% names(df) || !"id" %in% names(df)) {
    stop("Data frame must contain 'yy' and 'id' columns.")
  }

  newer_station_ids <- df %>%
    group_by(id) %>%
    summarize(min_year = min(yy, na.rm = TRUE)) %>%
    filter(min_year > 1966) %>% # nolint: object_usage_linter.
    pull(id)

  df %>% filter(!id %in% newer_station_ids)
}
