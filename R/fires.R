get_fires_area <- function(place='WORLD', year=as.numeric(format(Sys.Date(), "%Y")), use_cache = TRUE,
                      write_cache = getOption("hs_write_cache")) {

  if (year < 2012) stop("year must be greater than 2011")

  url <- "https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/"

    valid_aois <- c(
    "UN_EUR",   # Europe
    "UN_AFR",   # Africa
    "UN_ASI",   # Asia
    "UN_AME",   # Americas
    "UN_OCE",   # Oceania
    "WORLD"     # Global
  )

  if (place %in% valid_aois) (url <- paste0(url, "weeklyaoi","?aoi=", place, "&year=", year)) else
    (url <- paste0(url, "weekly","?country=", place, "&year=", year))

    cachename <- paste0("fires_", place, year, ".rds")

  hs_path <- tools::R_user_dir("hockeystick","cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path, cachename))) {
    cached_fires <- readRDS((file.path(hs_path, cachename)))
    return(invisible(cached_fires)) }}

  connected <- .isConnected('https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend')
  if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

  con <- url(url, open = "rb")
  resp <- readLines(con, warn = FALSE)
  close(con)
  raw <- fromJSON(resp, flatten = TRUE)

  weekly <- raw$banfweekly |> as_tibble()
  cumul <- raw$banfcumulative |> as_tibble()

  cumul <- cumul |> select(cum_events=events, cum_events_min=events_min, cum_events_max=events_max, cum_events_avg=events_avg,
             cum_area_ha=area_ha, cum_area_ha_min=area_ha_min, cum_area_ha_avg=area_ha_avg,  cum_area_ha_max=area_ha_max)

  fires <- bind_cols(weekly, cumul)
  fires <- fires |>  mutate(date= as.Date(mddate, format = "%Y%m%d"), .keep='unused', .after="week")
  lubridate::year(fires$date) = year

  if (write_cache) saveRDS(fires, file.path(hs_path, cachename))
  return(fires)

  }



get_fires_emissions <- function(place='WORLD', year=as.numeric(format(Sys.Date(), "%Y")), use_cache = TRUE,
                           write_cache = getOption("hs_write_cache")) {

  if (year < 2012) stop("year must be greater than 2011")

  url <- "https://api2.effis.emergency.copernicus.eu/statistics/v2/emissions/"

  valid_aois <- c(
    "UN_EUR",   # Europe
    "UN_AFR",   # Africa
    "UN_ASI",   # Asia
    "UN_AME",   # Americas
    "UN_OCE",   # Oceania
    "WORLD"     # Global
  )

  if (place %in% valid_aois) (url <- paste0(url, "weeklyaoi","?aoi=", place, "&year=", year)) else
    (url <- paste0(url, "weekly","?country=", place, "&year=", year))

  cachename <- paste0("emissions_", place, year, ".rds")

  hs_path <- tools::R_user_dir("hockeystick","cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path, cachename))) {
      cached_fires <- readRDS((file.path(hs_path, cachename)))
      return(invisible(cached_fires)) }}

  connected <- .isConnected('https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend')
  if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

  con <- url(url, open = "rb")
  resp <- readLines(con, warn = FALSE)
  close(con)
  raw <- fromJSON(resp, flatten = TRUE)

  weekly <- raw$emissionsweekly |> as_tibble()
  cumul <- raw$emissionsweeklycum |> as_tibble()

  cumul <- cumul |> select(cum_curvs=curv, cum_minv=minv, cum_maxv=maxv, cum_avvg=avgv)

  emissions <- bind_cols(weekly, cumul)
  emissions <- emissions |>  mutate(date= as.Date(dt, format = "%Y%m%d"), .keep='unused', .before = 'plt')
  lubridate::year(emissions$date) = year

  if (write_cache) saveRDS(fires, file.path(hs_path, cachename))
  return(emissions) }
