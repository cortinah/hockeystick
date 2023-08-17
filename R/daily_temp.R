#' Download and plot essential climate data
#'
#' Retrieves the daily air or sea-surface temperature data since 1979 from ClimateReanalyzer.org
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
#' @return Invisibly returns a tibble with the daily 2-meter air or sea surface temperatures since 1979 as well as historic mean by day-of-year and current anomaly versus mean.
#'
#' `get_dailytemp` invisibly returns a tibble with the daily temperatures since 1979 as well as mean by day-of-year and anomaly. Default to world data, but region can be selected among six options.
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
#' Notes: The "Daily 2-meter Air Temperature" page shows area-weighted daily means calculated from the 2-meter air temperature variable from the Climate Forecast System version 2 and Climate Forecast System Reanalysis, which are publicly available products of the NOAA National Centers for Environmental Prediction.
#' Sea surface is from NOAA Optimum Interpolation SST (OISST) version 2.1. OISST is a 0.25°x0.25° gridded dataset that provides estimates of temperature based on a blend of satellite, ship, and buoy observations. The dataset spans 1 January 1982 to present with a 1 to 2-day lag from the current day.
#'
#' }
#'
#' @export

get_dailytemp <- function(use_cache = TRUE, write_cache = getOption("hs_write_cache"),
                          region = 'W',
                          mean_start = if(region == 'WS' | region == 'AS') 1982 else 1979,
                          mean_end = 2000) {

  hs_path <- tools::R_user_dir("hockeystick", "cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path, 'dailytemp.rds')))

    {cached_temp <- readRDS((file.path(hs_path, 'dailytemp.rds')))
     cached_region <- attr(cached_temp, "hs_daily_region")

      if (region==cached_region) return(invisible(cached_temp)) }
  }

  file_url <- switch(region,
                     W='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_world_t2_day.json',          # World Air
                     NH='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_nh_t2_day.json',            # Northern Hemi Air
                     SH='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_sh_t2_day.json',            # Southern Hemi Air
                     AR='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_arctic_t2_day.json',        # Arctic Air
                     AN='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_antarctic_t2_day.json',     # #Antarctic Air
                     TR='https://climatereanalyzer.org/clim/t2_daily/json/cfsr_tropics_t2_day.json',       # Tropics Air
                     WS='https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_world2_sst_day.json',  # World Sea
                     AS='https://climatereanalyzer.org/clim/sst_daily/json/oisst2.1_natlan1_sst_day.json') # North Atlantic Sea

                     connected <- .isConnected(file_url)
  if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

  dl <- tempfile()
  download.file(file_url, dl)
  temp_json <- jsonlite::fromJSON(dl)

leap_years <- seq.int(1980, 2032, 4)


suppressWarnings(
daily_temperature <- as_tibble(temp_json) |>
  separate_wider_delim('data', delim=',', names_sep = '') |>
  mutate(data1 = parse_number(data1),
         data366 = parse_number(data366, na = 'NA)')) |>
  head(-3) |> mutate_all(as.numeric) |>
  pivot_longer(2:367, names_to = 'day_of_year') |>
  rename(year = name,
         temp = value) |>
  mutate(day_of_year = parse_number(day_of_year)) |>
  filter(!(day_of_year==60 & year %in% leap_years)) |>
  drop_na()
)


daily_temperature <- daily_temperature |> group_by(year) |>
  mutate(day_of_year = row_number()) |> ungroup() |>
  group_by(day_of_year) |>
  mutate(mean_temp = mean(temp[year >= mean_start & year <= mean_end]),
         temp_anom = temp - mean_temp) |>
  ungroup()

dates <- data.frame(day_of_year=1:365,
                    dummy_date=seq(as.Date('1975-01-01'), as.Date('1975-12-31'), by = '1 days'))

daily_temperature <- left_join(daily_temperature, dates, by='day_of_year')

colnames(daily_temperature)[4] <- paste0(mean_start,'-',mean_end,' mean')

if (region == 'WS' | region == 'AS') daily_temperature <- daily_temperature |> filter(year!=1981)

attr(daily_temperature, "hs_daily_region") <- region

if (write_cache) saveRDS(daily_temperature, file.path(hs_path, 'dailytemp.rds'))

invisible(daily_temperature)
}


#' Download and plot essential climate data
#'
#' Plots the daily temperatures and anomaly since 1979 retrieved using `get_dailytemp()` with ggplot2. The output ggplot2 object may be further modified.
#'
#'
#' @name plot_dailytemp
#' @param dataset Name of the tibble generated by `get_dailytemp`
#' @param print (boolean) Display daily temperature ggplot2 chart, defaults to TRUE. Use FALSE to not display chart.
#' @param anomaly (boolean) Display current anomaly versus historic mean, defaults to TRUE.
#' @param current_year (numeric) Year to highlight in alternate color, defaults to current year.
#' @param title_lab (string) Title to override default chart title. Default title pulls region name from dataset attributes.
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
#' dailydata <- get_dailytemp()
#' #
#' # Plot output using package's built-in ggplot2 defaults
#' plot_dailytemp(dailydata)
#'
#' # Don't plot anomaly shading and highight specific year
#' plot_dailytemp(anomaly = FALSE, current_year = 2012)
#'
#' # Or just call plot_temp(), which defaults to get_temp() dataset
#' plot_dailytemp()
#'
#' p <- plot_dailytemp(dailydata, print = FALSE)
#' # Modify plot such as: p + ggplot2::labs(title='Record Temperatures in 2023') }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#'
#' @export

plot_dailytemp <- function(dataset = get_dailytemp(), print = TRUE, anomaly = TRUE,
                           current_year = as.numeric(substr(Sys.Date(), 1, 4)),
                           title_lab = 'Daily Average Air Temperature') {

  if (is.null(dataset)) return(invisible(NULL))

latest <- paste(pull(tail(dataset,1)[1]), substr(pull(tail(dataset,1)[6]),6,7), substr(pull(tail(dataset,1)[6]),9,10), sep = '-')
meanperiod <- colnames(dataset)[4]
colnames(dataset)[4] <- 'mean_temp'

region <- attr(dataset, "hs_daily_region")

subtitle_lab <- '2-meter temperature since 1979 and mean'

if (region == 'WS' ||region == 'AS') subtitle_lab <- 'Sea surface temperature since 1982 and mean'

if (title_lab == "Daily Average Air Temperature") {

    if (region == 'WS' | region == 'AS') title_lab <- 'Daily Average Sea Surface Temperature'

    region <- switch(region,
                     W = 'World',
                     NH = 'Northern Hemisphere',
                     SH = 'Southern Hemisphere',
                     AR = 'Arctic',
                     AN = 'Antarctic',
                     TR = 'Tropics',
                     WS = 'World (60S-60N)',
                     AS = 'North Atlantic')

    title_lab <- paste(region, title_lab)}

plot <- ggplot(dataset) +
    geom_line(aes(x = dummy_date, y = temp, group = year), alpha = 0.7, color = 'grey') +
    scale_fill_gradientn(name = 'Anomaly (C\U00B0)', colors = RColorBrewer::brewer.pal(9, 'YlOrRd'), labels = scales::label_number(accuracy = 0.1)) +
    geom_line(aes(dummy_date, mean_temp, color = 'M'), linetype = 'dashed', linewidth = 1.1) +
    scale_y_continuous(n.breaks = 9) +
    theme_bw(base_size = 12) +
    scale_x_date(name = 'Day of year', breaks = c(as.Date('1975-01-01'), as.Date('1975-04-01'),
                                                  as.Date('1975-07-01'), as.Date('1975-10-01'), as.Date('1975-12-31')),
                 date_labels = '%b%est', date_minor_breaks = '1 month') +
    labs(title = title_lab, subtitle = subtitle_lab,
         y = 'Temperature (C\U00B0)',
         caption = paste0('Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ', latest),
         color = NULL) +
    scale_color_manual(values = c('firebrick', 'black', 'grey'), labels = c(current_year, meanperiod), breaks = c('L', 'M')) +
    geom_line(data = filter(dataset, year == current_year),
              aes(dummy_date, temp, color = 'L'), linewidth = 1.3) +
    theme(legend.position = "top") + theme(legend.key.size = unit(0.5, 'cm'),
                                         legend.margin = margin(5, 0, 0, 0))

if (anomaly) {
  subtitle_lab <- '2-meter temperature since 1979, mean, and current anomaly'

  if (region == 'World (60S-60N)' | region == 'North Atlantic' | region == 'WS' | region == 'AS')
      subtitle_lab <- 'Sea surface temperature since 1982, mean, and current anomaly'

  plot <- plot +
          geom_rect(data = filter(dataset, year == current_year),
          aes(xmin = dummy_date - 1,
              xmax = dummy_date,
              ymin = mean_temp,
              ymax = mean_temp + temp_anom,
              fill = temp_anom)) +
          geom_line(data = filter(dataset, year == current_year),
            aes(dummy_date, temp, color='L'), linewidth = 1.3) +
  labs(subtitle = subtitle_lab) }

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}
