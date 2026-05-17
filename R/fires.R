#' Download and plot essential climate data
#'
#' Retrieves global wildfire burnt area and fire count data from the
#' Global Wildfire Information System (GWIS), operated by the Copernicus
#' Emergency Management Service.
#' \url{https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend}
#'
#' @name get_fires_area
#' @param place (string) Region or country for which to retrieve data. May be one of the
#'   following regional aggregates: \code{"WORLD"} (default), \code{"UN_EUR"} (Europe),
#'   \code{"UN_AFR"} (Africa), \code{"UN_ASI"} (Asia), \code{"UN_AME"} (Americas),
#'   or \code{"UN_OCE"} (Oceania). Alternatively, supply an ISO 3166-1 alpha-3 country
#'   code such as \code{"USA"}, \code{"CAN"}, or \code{"BRA"}.
#' @param year (numeric) Four-digit year for which to retrieve data. Must be 2012 or later.
#'   Defaults to the current year.
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)
#'
#' @return Invisibly returns a tibble with weekly and cumulative wildfire burnt area and fire
#'   count data for the requested place and year. Weekly columns include \code{date},
#'   \code{events}, \code{events_min}, \code{events_max}, \code{events_avg},
#'   \code{area_ha}, \code{area_ha_min}, \code{area_ha_avg}, and \code{area_ha_max}.
#'   Cumulative columns are prefixed with \code{cum_}.
#'
#' @details `get_fires_area` invisibly returns a tibble with weekly and year-to-date cumulative
#'   wildfire burnt area (in hectares) and fire event counts sourced from the GWIS API.
#'   Historical minimum, maximum, and average baselines are included for comparison.
#'   Data coverage begins in 2012. Cache filenames include the place and year so that
#'   multiple queries can be cached independently.
#'
#' @examples
#' \donttest{
#' # Fetch global data for the current year from cache if available:
#' fires <- get_fires_area()
#' #
#' # Fetch data for Canada in 2023:
#' fires_can <- get_fires_area(place = 'CAN', year = 2023)
#' #
#' # Force cache refresh:
#' fires <- get_fires_area(use_cache = FALSE)
#' #
#' # Review cache contents and last update dates:
#' hockeystick_cache_details() }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols select mutate
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' Global Wildfire Information System (GWIS), Copernicus Emergency Management Service / European Commission Joint Research Centre.
#' \url{https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend}
#'
#' API: \url{https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/}
#'
#' @export
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



#' Download and plot essential climate data
#'
#' Retrieves global wildfire greenhouse gas emissions data from the
#' Global Wildfire Information System (GWIS), operated by the Copernicus
#' Emergency Management Service.
#' \url{https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend}
#'
#' @name get_fires_emissions
#' @param place (string) Region or country for which to retrieve data. May be one of the
#'   following regional aggregates: \code{"WORLD"} (default), \code{"UN_EUR"} (Europe),
#'   \code{"UN_AFR"} (Africa), \code{"UN_ASI"} (Asia), \code{"UN_AME"} (Americas),
#'   or \code{"UN_OCE"} (Oceania). Alternatively, supply an ISO 3166-1 alpha-3 country
#'   code such as \code{"USA"}, \code{"CAN"}, or \code{"BRA"}.
#' @param year (numeric) Four-digit year for which to retrieve data. Must be 2012 or later.
#'   Defaults to the current year.
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)
#'
#' @return Invisibly returns a tibble with weekly and cumulative wildfire emissions data for
#'   the requested place and year. The \code{plt} column identifies the pollutant
#'   (e.g. \code{"CO2"}, \code{"CO"}, \code{"PM2.5"}). Weekly value columns include
#'   \code{curv} (current), \code{minv}, \code{maxv}, and \code{avgv}. Cumulative
#'   columns are prefixed with \code{cum_}.
#'
#' @details `get_fires_emissions` invisibly returns a tibble with weekly and year-to-date
#'   cumulative wildfire emissions by pollutant, sourced from the GWIS emissions API.
#'   Historical minimum, maximum, and average baselines are included for comparison.
#'   Data coverage begins in 2012. Filter on the \code{plt} column to select a specific
#'   pollutant (e.g. \code{filter(plt == "CO2")}). Cache filenames include the place
#'   and year so that multiple queries can be cached independently.
#'
#' @examples
#' \donttest{
#' # Fetch global emissions data for the current year from cache if available:
#' emissions <- get_fires_emissions()
#' #
#' # Fetch CO2 emissions for the USA in 2023:
#' emissions_usa <- get_fires_emissions(place = 'USA', year = 2023)
#' #
#' # Force cache refresh:
#' emissions <- get_fires_emissions(use_cache = FALSE)
#' #
#' # Review cache contents and last update dates:
#' hockeystick_cache_details() }
#'
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_cols select mutate
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' Global Wildfire Information System (GWIS), Copernicus Emergency Management Service / European Commission Joint Research Centre.
#' \url{https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend}
#'
#' API: \url{https://api2.effis.emergency.copernicus.eu/statistics/v2/emissions/}
#'
#' @export
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


dataset <- get_fires_emissions(place = 'USA', year = 2023)
#=======


plot_fires_area <- function(dataset = get_fires_area(), var=c('area', 'count'), style=c('cumulative', 'weekly'), print=TRUE) {

  if (is.null(dataset)) return(invisible(NULL))
  style <- match.arg(style)
  var <- match.arg(var)

  fireyear <- dataset[1,3] |> pull() |> year()
  firelocation <- dataset[1,1] |> pull()
  if (var=='area') firetitle <- paste0('Wildfire Area Burnt in ', fireyear)
  if (var=='count') firetitle <- paste0('Number of Wildfires in ', fireyear)
  plot <- NULL
 
  if(var=='area' && style=='cumulative') {
  plot <- ggplot(dataset, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_area_ha_min, ymax=cum_area_ha_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_area_ha, col='Year-to-date'), linewidth=1.1, na.rm = T) + geom_line(aes(y=cum_area_ha_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Burnt Area (thousands of ha)', x=NULL, title=firetitle, subtitle=firelocation, caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")
  }
      
  if(var=='area' && style=='weekly') {
  plot <- ggplot(dataset, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=area_ha_min, ymax=area_ha_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=area_ha, col='Week'), linewidth=1.1, na.rm = T) + geom_line(aes(y=area_ha_avg, col="Average\n(since 2012)")) +
  labs(y='Weekly Burnt Area (thousands of ha)', x=NULL, title=firetitle, subtitle=firelocation, caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Week","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")
  }

  if(var=='count' && style=='cumulative') {
  plot <- ggplot(dataset, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_events_min, ymax=cum_events_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_events, col='Year-to-date'), linewidth=1.1, na.rm=T) + geom_line(aes(y=cum_events_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Number', x=NULL, title=firetitle, subtitle=firelocation, caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma()) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")
  }
  
  if(var=='count' && style=='weekly') {
  plot <- ggplot(dataset, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=events_min, ymax=events_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=events, col='Week'), linewidth=1.1, na.rm=T) + geom_line(aes(y=events_avg, col="Average\n(since 2012)")) +
  labs(y='Weekly Number', x=NULL, title=firetitle, subtitle=firelocation, caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma()) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Week","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")
  }

  if (print) print(plot)
  invisible(plot) 
}


####### emissions cumulative ##
e |> filter(plt=='CO2') |> ggplot(aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_minv, ymax=cum_maxv, fill='Min - Max Range\n(since 2003)')) +
  geom_line(aes(y=cum_curvs, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_avvg, col="Average\n(since 2003)")) + labs(y='Cumulative Emissions (millions of tons)', x=NULL, title='2026 CO2 Emissions from Wildfires', subtitle='World',caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .000001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2003)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")


####### emissions weekly ##
e |> filter(plt=='CO2') |> ggplot(aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=minv, ymax=maxv, fill='Min - Max Range\n(since 2003)')) +
  geom_line(aes(y=curv, col='Week    '), linewidth=1.1) + geom_line(aes(y=avgv, col="Average\n(since 2003)")) + labs(y='Weekly Emissions (millions of tons)', x=NULL, title='2026 CO2 Emissions from Wildfires', subtitle='World',caption='Source: Global Wildfire Information System') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .000001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Week    ","Average\n(since 2003)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")
