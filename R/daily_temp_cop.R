get_dailytempcop <- function(use_cache = TRUE, write_cache = getOption("hs_write_cache"), region = 'W') {

  hs_path <- tools::R_user_dir("hockeystick", "cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path, 'dailytempcop.rds')))

    {cached_temp <- readRDS((file.path(hs_path, 'dailytempcop.rds')))
    cached_region <- attr(cached_temp, "hs_daily_region")

    if (region==cached_region) return(invisible(cached_temp)) }
  }

  file_url <- 'https://sites.ecmwf.int/data/climatepulse/data/series/era5_daily_series_2t_global.csv'

  connected <- .isConnected(file_url)
  if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

  dl <- tempfile()
  download.file(file_url, dl)
  temp_csv <- read.csv(dl,skip = 18)

  colnames(temp_csv) <- c('date', 'temp', '1991-2020 mean', 'temp_anom', 'status' )
  temp_csv$date <- as.Date(temp_csv$date)
  temp_csv$year <- lubridate::year(temp_csv$date)
  temp_csv$dummy_date <- as.Date(paste("1925", lubridate::month(temp_csv$date), lubridate::day(temp_csv$date),sep = '-'))

  temp_csv <- temp_csv |> filter(!is.na(dummy_date))
  temp_csv <- temp_csv |> group_by(year) |> mutate(day_of_year = row_number()) |> ungroup()

  daily_temperature <- temp_csv |> select(year, day_of_year, date, temp, `1991-2020 mean`, temp_anom, dummy_date) |> as_tibble()

  attr(daily_temperature, "hs_daily_region") <- region

  if (write_cache) saveRDS(daily_temperature, file.path(hs_path, 'dailytempcop.rds'))

  invisible(daily_temperature)
}

