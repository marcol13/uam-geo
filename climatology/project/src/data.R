POZNAN_STATION_ID <- "352160330" # nolint: object_name_linter.
YEAR_RANGE <- c(1966:2024) # nolint: object_name_linter.
DF_MEASURES_COLUMNS <- c("tmax_mean", "tmin_mean", "t2m_mean_mon", "t5cm_min", "rr_monthly", "insolation_monthly", "snowcover_max", "snowcover_days", "rain_days", "snow_days", "hail_days", "fog_days", "rime_days", "glaze_days", "ws_10ms_days", "ws_15ms_days", "thunder_days", "dew_days", "hoarfrost_days")
DF_DEFAULT_COLUMNS <- c("id", "X", "Y", "station", "yy", "mm", DF_MEASURES_COLUMNS)

# id - station id
# X - longitude
# Y - latitude
# station - station name
# yy - year
# mm - month
# tmax_mean - mean max temperature in month
# tmin_mean - mean min temperature in month
# t2m_mean_mon - mean temperature in month
# t5cm_min - min temperature at ground level (at 5cm) in month
# rr_monthly - monthly sum of precipitation [mm]
# insolation_monthly - monthly sum of insolation [h]
# snowcover_max - max snow cover in month [cm]
# snowcover_days - number of days with snow cover
# rain_days - number of days with rain
# snow_days - number of days with snow
# hail_days - number of days with hail
# fog_days - number of days with fog
# rime_days - number of days with rime
# glaze_days - number of days with glaze
# ws_10ms_days - number of days with wind speed >= 10 m/s
# ws_15ms_days - number of days with wind speed > 15 m/s
# thunder_days - number of days with thunder
# dew_days - number of days with dew
# hoarfrost_days - number of days with hoarfrost

## Deleted:
# cloud_mean_mon - mean cloud cover in month [octas]
# vapor_press_mean_mon - mean vapor pressure in month [hPa]
# rh_mean_mon - mean relative humidity in month [%]
# press_mean_mon - mean atmospheric pressure in month [hPa]
# slp_mean_mon - mean sea level pressure in month [hPa]
# ws_mean_mon - mean wind speed in month [m/s]


get_pl_raw_data <- function(filename) {
  if (nchar(filename) > 0) {
    if (file.exists(file.path(filename))) {
      message(sprintf("Loading data from cache: %s", filename))
      return(readRDS(file.path(filename)))
    }
  }
  climate::meteo_imgw(
    interval = "monthly", rank = "synop", year = c(1966:2024),
    coords = TRUE
  )
}

get_pzn_raw_data <- function(filename) {
  if (nchar(filename) > 0) {
    if (file.exists(file.path(filename))) {
      message(sprintf("Loading data from cache: %s", filename))
      return(readRDS(file.path(filename)))
    }
  }
  climate::meteo_imgw(
    interval = "monthly", rank = "synop", year = c(1966:2024),
    coords = TRUE, station = "POZNAŃ"
  )
}

save_into_file <- function(df, data_dir, file_name) {
  if (!dir.exists(data_dir)) {
    dir.create(data_dir)
  }
  file_path <- file.path(data_dir, file_name)
  saveRDS(df, file = file_path)
  message(sprintf("Data saved to %s", file_path))
}

get_columns <- function(df, columns) {
  if (is.null(columns)) {
    return(df)
  }
  if (!all(columns %in% names(df))) {
    stop("Some columns are not present in the data frame.")
  }
  df[, columns, drop = FALSE]
}
