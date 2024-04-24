#' Download and plot essential climate data
#'
#' Retrieves the daily air or sea-surface temperature data since 1940 from ClimateReanalyzer.org
#' Source is University of Maine Climate Change Institute.
#' \url{https://climatereanalyzer.org/clim/t2_daily/}
#'
#' @name get_dailytemp
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)
#' @param region (string) Region selection, defaults to world air temperature. Options are: World Air "W", Northern Hemisphere Air "NW", Southern Hemisphere Air "SW", Tropics Air "TR", Arctic Air "AR", Antarctica Air "AN", World Sea Surface "WS", and North Atlantic Sea Surface "NS".
#' @param mean_start (numeric) Start year for historic mean, defaults to 1979.
#' @param mean_end (numeric) End year for historic mean, defaults to 2000.
#'
#' @return Invisibly returns a tibble with the daily 2-meter air or sea surface temperatures since 1940 as well as historic mean by day-of-year and current anomaly versus mean.
#'
#' `get_dailytemp` invisibly returns a tibble with the daily temperatures since 1940 as well as mean by day-of-year and anomaly. Default to world data, but region can be selected among six options.
#'
#' Region options include world air (default), Northern Hemisphere air, Southern Hemisphere air, Tropics air, Arctic air, Antarctic air, World sea surface and North Atlantic sea surface and is stored in attribute of output.
#' The historic daily mean-by-day period defaults to 1979-2000. This range can be optionally modified.
#'
#' Data are updated daily. For day-of-year mean removes observations from February 29 on leap years.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils download.file head tail
#' @importFrom readr parse_number
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' \donttest{
#' # Fetch temp anomaly from cache if available:
#' dailytemps <- get_dailytemp()
#' #
#' # Force cache refresh:
#' dailytemps <- get_dailytemp(use_cache=FALSE)
#' #
#' # Review cache contents and last update dates:
#' hockeystick_cache_details()
#' #
#' # Plot output using package's built-in ggplot2 settings
#' plot_dailytemp(dailytemps)
#'
#' # Change region to Arctic
#' arctictemp <- get_dailytemp(region='AR', use_cache=FALSE)}
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' \itemize{
#' \item ClimateReanalyzer.org: \url{https://climatereanalyzer.org/clim/t2_daily/}
#'
#' Notes: daily mean surface air temperature (2-meter height) estimates from the ECMWF Reanalysis version 5 (ERA5) for the period January 1940 to present. ERA5 is a state-of-the-art numerical climate/weather modeling framework that ingests surface, radiosonde, and satellite observations to estimate the state of the atmosphere through time.
#' ERA5 files have a horizontal grid resolution of 0.25° x 0.25° (about 31km x 31km at 45°N). Each daily temperature represents an average across all model gridcells within the defined latitude/longitude bounds for the selected domain. The means are area-weighted to account for the convergence of longitude lines at the poles
#'
#' }
#'
#' @export

get_dailytemp <- function(use_cache = TRUE, write_cache = getOption("hs_write_cache"),
                          region = 'W',
                          mean_start = if(region %in% c('WS','NS', 'ws', 'ns')) 1982 else 1979,
                          mean_end = 2000) {

  hs_path <- tools::R_user_dir("hockeystick", "cache")
  region <- toupper(region)

  if (use_cache) {
    if (file.exists(file.path(hs_path, 'dailytemp.rds')))

    {cached_temp <- readRDS((file.path(hs_path, 'dailytemp.rds')))
     cached_region <- attr(cached_temp, "hs_daily_region")

      if (region==cached_region) return(invisible(cached_temp)) }
  }

  file_url <- switch(region,
                      W = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_world_t2_day.json',       # World Air
                     NW = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_nh_t2_day.json',          # Northern Hemi Air
                     SW = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_sh_t2_day.json',          # Southern Hemi Air
                     AR = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_arctic_t2_day.json',      # Arctic Air
                     AN = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_antarctic_t2_day.json',   # #Antarctic Air
                     TR = 'https://climatereanalyzer.org/clim/t2_daily/json/era5_tropics_t2_day.json',     # Tropics Air
                     WS = 'https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json',     # World Sea
                     NS = 'https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json')    # North Atlantic Sea

                     connected <- .isConnected(file_url)
  if (!connected) {message("Retrieving remote data requires internet connectivity.\nAvailable regions are: W, NW, SW, AR, AN, TR, WS, NS."); return(invisible(NULL))}

  dl <- tempfile()
  download.file(file_url, dl)
  temp_json <- tryCatch (jsonlite::fromJSON(dl),
                          error=function(error_msg) {
                            message('Invalid JSON file, please check temperature data url.')
                            return(NULL)
                            } )
  if (is.null(temp_json)) return(invisible(NULL))

leap_years <- seq.int(1940, 2032, 4)

if (region!='WS' && region!='NS') {

suppressWarnings(
daily_temperature <- as_tibble(temp_json) |> head(-3) |>
  separate_wider_delim('data', delim=', ', names_sep = '') |>
  mutate(data1 = parse_number(data1),
         data366 = parse_number(data366, na = 'NA)')) |>
  select(-data_source) |> mutate_all(as.numeric) |>
  pivot_longer(2:367, names_to = 'day_of_year') |>
  rename(year = name,
         temp = value) |>
  mutate(day_of_year = parse_number(day_of_year)) |>
  filter(!(day_of_year==60 & year %in% leap_years)) |>
  drop_na()
) }      else {

  suppressWarnings(
  daily_temperature <- as_tibble(temp_json) |> head(-3) |> tail(-1) |>
      separate_wider_delim('data', delim=',', names_sep = '') |>
    mutate(data1 = parse_number(data1),
             data366 = parse_number(data366, na = 'NA)')) |>
    mutate_all(as.numeric) |>
      pivot_longer(2:367, names_to = 'day_of_year') |>
      rename(year = name,
             temp = value) |>
      mutate(day_of_year = parse_number(day_of_year)) |>
      filter(!(day_of_year==60 & year %in% leap_years)) |>
      drop_na()
  ) }



daily_temperature <- daily_temperature |> group_by(year) |>
  mutate(day_of_year = row_number()) |> ungroup() |>
  group_by(day_of_year) |>
  mutate(mean_temp = mean(temp[year >= mean_start & year <= mean_end]),
         temp_anom = temp - mean_temp) |>
  ungroup()

dates <- data.frame(day_of_year=1:365,
                    dummy_date=seq(as.Date('1925-01-01'), as.Date('1925-12-31'), by = '1 days'))

daily_temperature <- left_join(daily_temperature, dates, by='day_of_year')

colnames(daily_temperature)[4] <- paste0(mean_start, '-', mean_end, ' mean')

daily_temperature <- mutate(daily_temperature, date = as.Date(paste0(year, '-', substr(dummy_date,6,7), '-', substr(dummy_date, 9, 10))))

daily_temperature <- daily_temperature |> relocate(date, .after=day_of_year)

if (region == 'WS' | region == 'NS') daily_temperature <- daily_temperature |> filter(year!=1981)

attr(daily_temperature, "hs_daily_region") <- region

if (write_cache) saveRDS(daily_temperature, file.path(hs_path, 'dailytemp.rds'))

invisible(daily_temperature)
}


#' Download and plot essential climate data
#'
#' Plots the daily temperatures since 1940 and current anomaly data retrieved using `get_dailytempcop()` with ggplot2. The output ggplot2 object may be further modified.
#'
#'
#' @name plot_dailytemp
#' @param dataset Name of the tibble generated by `get_dailytempcop` or `get_dailytemp`
#' @param print (boolean) Display daily temperature ggplot2 chart, defaults to TRUE. Use FALSE to not display chart.
#' @param anomaly (boolean) Display current anomaly versus historic mean, defaults to TRUE.
#' @param maxtemp (boolean) Display current deviation versus historic max, defaults to FALSE.
#' @param current_year (numeric) Year to highlight in alternate color, defaults to current year.
#' @param title_lab (string) Title to override default chart title. Default title pulls region name from dataset attributes.
#' @param cop (boolean) Flag for chart caption, TRUE = Copernicus, FALSE =. ClimateReanalyzer.org
#'
#' @return Invisibly returns a ggplot2 object with daily temperature anomaly chart
#'
#' @details `plot_temp` invisibly returns a ggplot2 object with a pre-defined daily temperature anomaly chart using data from `get_dailytemp`.
#' By default the chart is also displayed. Plots one line per year, as well as mean and anomaly (which may be disabled). Users may further modify the output ggplot2 chart.
#'
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#'
#' @examples
#' \donttest{
#' # Fetch temperature anomaly:
#' dailydata <- get_dailytempcop()
#' #
#' # Plot output using package's built-in ggplot2 defaults
#' plot_dailytemp(dailydata)
#'
#' # Don't plot anomaly shading and highight specific year
#' plot_dailytemp(anomaly = FALSE, current_year = 2012)
#'
#' # Or just call plot_temp(), which defaults to get_dailytempcop() dataset
#' plot_dailytemp()
#'
#' p <- plot_dailytemp(dailydata, print = FALSE)
#' # Modify plot such as: p + ggplot2::labs(title='Record Temperatures in 2023') }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#'
#' @export

plot_dailytemp <- function(dataset = get_dailytempcop(), print = TRUE, anomaly = FALSE, maxtemp = FALSE,
                           current_year = as.numeric(substr(Sys.Date(), 1, 4)),
                           title_lab = 'Daily Average Air Temperature', cop=TRUE) {

  if (is.null(dataset)) return(invisible(NULL))

  if (anomaly && maxtemp) stop("Only one of anomaly or maxtemp parameters can be TRUE")

latest <- pull(tail(dataset, 1)['date'])
meanperiod <- colnames(dataset)[5]
colnames(dataset)[5] <- 'mean_temp'

region <- attr(dataset, "hs_daily_region")

subtitle_lab <- '2-meter temperature since 1940 and mean'

if (region == 'WS' ||region == 'NS') subtitle_lab <- 'Sea surface temperature since 1982 and mean'

if (title_lab == "Daily Average Air Temperature") {

    if (region == 'WS' | region == 'NS') title_lab <- 'Daily Average Sea Surface Temperature'

    region <- switch(region,
                      W = 'World',
                     NW = 'Northern Hemisphere',
                     SW = 'Southern Hemisphere',
                     AR = 'Arctic',
                     AN = 'Antarctic',
                     TR = 'Tropics',
                     WS = 'World (60S-60N)',
                     NS = 'North Atlantic')

    title_lab <- paste(region, title_lab)}

# Color code notes: 'A': all years, 'M': mean, 'L': latest.

captionsource <- if (cop) paste0('Source: EU Copernicus Climate Service\ncds.climate.copernicus.eu as of ', latest)
                    else
                          paste0('Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ', latest)

plot <- ggplot(dataset) +
    geom_line(aes(x = dummy_date, y = temp, group = year, color = 'A'), alpha = 0.7) +
    geom_line(aes(dummy_date, mean_temp, color = 'M'), linetype = 'dashed', linewidth = 1.1) +
    scale_y_continuous(n.breaks = 9) +
    theme_bw(base_size = 12) +
    scale_x_date(name = 'Day of year', breaks = c(as.Date('1925-01-01'), as.Date('1925-04-01'),
                                                  as.Date('1925-07-01'), as.Date('1925-10-01'), as.Date('1925-12-31')),
                 date_labels = '%b%est', date_minor_breaks = '1 month') +
    labs(title = title_lab, subtitle = subtitle_lab,
         y = 'Temperature (C\U00B0)',
         caption = captionsource,
         color = NULL) +
    scale_color_manual(values = c('firebrick', 'black', 'grey'), labels = c(current_year, meanperiod, 'All years'), breaks = c('L', 'M', 'A')) +
    geom_line(data = filter(dataset, year == current_year),
              aes(dummy_date, temp, color = 'L'), linewidth = 1.3) +
    theme(legend.position = "top") + theme(legend.key.size = unit(0.5, 'cm'),
                                         legend.margin = margin(5, 0, 0, 0))

if (anomaly) {
  subtitle_lab <- '2-meter temperature since 1940, mean, and current anomaly'

  if (region == 'World (60S-60N)' | region == 'North Atlantic' | region == 'WS' | region == 'NS')
      subtitle_lab <- 'Sea surface temperature since 1982, mean, and current anomaly'

  plot <- plot + scale_fill_gradientn(name = 'Anomaly (C\U00B0)', colors = RColorBrewer::brewer.pal(9, 'YlOrRd'),
                                      labels = scales::label_number(accuracy = 0.1)) +
          geom_rect(data = filter(dataset, year == current_year),
          aes(xmin = dummy_date - 1,
              xmax = dummy_date,
              ymin = mean_temp,
              ymax = mean_temp + temp_anom,
              fill = temp_anom)) +
          geom_line(data = filter(dataset, year == current_year),
            aes(dummy_date, temp, color='L'), linewidth = 1.3) +
    geom_line(aes(dummy_date, mean_temp, color = 'M'), linetype = 'dashed', linewidth = 1.2) +
  labs(subtitle = subtitle_lab) }

if (maxtemp) {
  subtitle_lab <- '2-meter temperature since 1940, mean, and deviation from historic max'

  if (region == 'World (60S-60N)' | region == 'North Atlantic' | region == 'WS' | region == 'AS')
    subtitle_lab <- 'Sea surface temperature since 1982, mean, and current anomaly'

  max_temp <- dataset |> filter(year!=current_year) |> group_by(day_of_year) |> summarize(max_temp=max(temp))
  dataset <- left_join(dataset, max_temp, by = "day_of_year")

  plot <- plot +
    scale_fill_gradientn(name = 'Deviation (C\U00B0)',
                         colors = rev(RColorBrewer::brewer.pal(9, 'RdYlGn')), labels = scales::label_number(accuracy = 0.1)) +
    geom_rect(data = filter(dataset, year == current_year),
              aes(xmin = dummy_date - 1,
                  xmax = dummy_date,
                  ymin = max_temp,
                  ymax = temp,
                  fill = temp - max_temp)) +
    geom_line(data = filter(dataset, year == current_year),
              aes(dummy_date, temp, color='L'), linewidth = 1.3) +
    labs(subtitle = subtitle_lab) }

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}


#' Download and plot essential climate data
#'
#' Retrieves the daily air temperature since 1940 from the EU Copernicus Service
#' \url{https://cds.climate.copernicus.eu/#!/home}
#'
#' @name get_dailytempcop
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)
#' @param region (string) Region selection, defaults to world air temperature. Options are: World Air "W".
#'
#' @return Invisibly returns a tibble with the daily 2-meter air temperatures since 1940 as well as historic mean by day-of-year and current anomaly versus mean.
#'
#' `get_dailytempcop` invisibly returns a tibble with the daily temperatures since 1940 as well as mean by day-of-year and anomaly.
#'
#' Region options include world air (default).
#' The historic daily mean-by-day period defaults to 1991-2020.
#'
#' Data are updated daily.
#'
#' @importFrom utils download.file head tail
#' @importFrom readr parse_number
#' @import tidyr
#' @import dplyr
#'
#' @examples
#' \donttest{
#' # Fetch temp anomaly from cache if available:
#' dailytemps <- get_dailytempcop()
#' #
#' # Force cache refresh:
#' dailytemps <- get_dailytempcop(use_cache=FALSE)
#' #
#' # Review cache contents and last update dates:
#' hockeystick_cache_details()
#' #
#' # Plot output using package's built-in ggplot2 settings
#' plot_dailytemp(dailytemps) }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' \itemize{
#' \item Copernicus: \url{https://cds.climate.copernicus.eu/#!/home}
#'
#' Notes: daily mean surface air temperature (2-meter height) estimates from the ECMWF Reanalysis version 5 (ERA5) for the period January 1940 to present. ERA5 is a state-of-the-art numerical climate/weather modeling framework that ingests surface, radiosonde, and satellite observations to estimate the state of the atmosphere through time.
#' ERA5 files have a horizontal grid resolution of 0.25° x 0.25° (about 31km x 31km at 45°N). Each daily temperature represents an average across all model gridcells within the defined latitude/longitude bounds for the selected domain. The means are area-weighted to account for the convergence of longitude lines at the poles
#'
#' }
#'
#' @export


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
  temp_csv <- read.csv(dl, skip = 18)

  colnames(temp_csv) <- c('date', 'temp', '1991-2020 mean', 'temp_anom', 'status' )
  temp_csv$date <- as.Date(temp_csv$date)
  temp_csv$year <- lubridate::year(temp_csv$date)
  temp_csv$dummy_date <- as.Date(paste("1925", lubridate::month(temp_csv$date), lubridate::day(temp_csv$date),sep = '-'))

  temp_csv <- temp_csv |> filter(!is.na(dummy_date))
  temp_csv <- temp_csv |> group_by(year) |> mutate(day_of_year = row_number()) |> ungroup()

  daily_temperature <- temp_csv |> select(year, day_of_year, date, temp, `1991-2020 mean`, temp_anom, dummy_date) |> as_tibble()

  attr(daily_temperature, "hs_daily_region") <- region

  if (write_cache) saveRDS(daily_temperature, file.path(hs_path, 'dailytempcop.rds'))

  invisible(daily_temperature) }
