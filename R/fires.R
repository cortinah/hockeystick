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





#=======
ggplot(df, aes(x = date)) +theme_minimal() +geom_ribbon(data=df,aes(ymin=cum_area_ha_min, ymax=cum_area_ha_max), fill='gray90') + geom_line(aes(y=cum_area_ha),col='red') + geom_line(data=df, aes(y=cum_area_ha_avg),col="dodgerblue") +
  labs(y='Cumulative Burnt Area (thousanfs of ha)', x=NULL, title='Wildfire Burnt Area') + scale_y_continuous(labels = scales::label_comma(scale = .001))



ggplot(df, aes(x = date)) +theme_minimal() +geom_ribbon(data=df,aes(ymin=area_ha_min, ymax=area_ha_max),fill='gray90') + geom_line(aes(y=area_ha),col='red') + geom_line(data=df, aes(y=area_ha_avg),col="blue")




ggplot(df, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_area_ha_min, ymax=cum_area_ha_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_area_ha, col='Year-to-date'), linewidth=1.1, na.rm = T) + geom_line(aes(y=cum_area_ha_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Burnt Area (thousands of ha)', x=NULL, title='2026 Wildfire Area Burnt', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")

# num fires
ggplot(df, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_events_min, ymax=cum_events_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_events, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_events_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Number', x=NULL, title='2026 Number of Wildfires', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma()) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")


####### emissions

e |> filter(plt=='CO2') |> ggplot(aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_minv, ymax=cum_maxv, fill='Min - Max Range\n(since 2003)')) +
  geom_line(aes(y=cum_curvs, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_avvg, col="Average\n(since 2003)")) + labs(y='Cumulative Emissions (millions of tons)', x=NULL, title='2026 Emissions from Wildfires', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .000001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2003)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")

