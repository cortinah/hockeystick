library(tidyverse)
library(jsonlite)
library(RColorBrewer)


get_dailytemp <- function(use_cache = TRUE, write_cache = getOption("hs_write_cache"),
                          mean_start = 1979, mean_end = 2020) {

  hs_path <- tools::R_user_dir("hockeystick","cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path,'dailytemp.rds'))) return(invisible(readRDS((file.path(hs_path,'dailytemp.rds')))))
  }

  file_url <- 'https://climatereanalyzer.org/clim/t2_daily/json/cfsr_world_t2_day.json'
  connected <- .isConnected(file_url)
  if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

  dl <- tempfile()
  download.file(file_url, dl)
  temp_json <- fromJSON(dl)

leap_years <- seq.int(1980, 2032, 4)


suppressWarnings(
daily_temperature <- as.data.frame(temp_json) |>
  separate_wider_delim(data, delim=',', names_sep='') |>
  mutate(data1 = parse_number(data1),
         data366 = parse_number(data366, na='NA)')) |>
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

if (write_cache) saveRDS(daily_temperature, file.path(hs_path, 'dailytemp.rds'))

invisible(daily_temperature)
}


# draw plot

plot_dailytemp <- function(dataset = get_dailytemp(), print=TRUE, anomaly = TRUE, current_year = as.numeric(substr(Sys.Date(), 1, 4))) {

  if (is.null(dataset)) return(invisible(NULL))

latest <- paste(pull(tail(dataset,1)[1]), substr(pull(tail(dataset,1)[6]),6,7), substr(pull(tail(dataset,1)[6]),9,10), sep = '-')

plot <- ggplot(dataset) +
    geom_line(aes(dummy_date, temp, group = year), alpha = 0.7, color='grey') +
    scale_fill_gradientn(name='Anomaly (C\U00B0)', colors = brewer.pal(9, 'YlOrRd')) +
    geom_line(aes(dummy_date, mean_temp, color = 'M'), linetype = 'dashed', linewidth = 1.1) +
    scale_y_continuous(n.breaks = 9) +
    theme_bw(base_size = 12) +
    scale_x_date(name=element_blank(), breaks = c(as.Date('1975-01-01'), as.Date('1975-04-01'),
                                                  as.Date('1975-07-01'), as.Date('1975-10-01'), as.Date('1975-12-31')),
                 date_labels = '%b-%d', date_minor_breaks = '1 month') +
    labs(title = 'Daily Global Average Air Temperature', subtitle = '2-meter temperature since 1979 and mean',
         y = 'Temperature (C\U00B0)',
         caption = paste0('Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ', latest),
         color = NULL) +
    scale_color_manual(values=c('firebrick', 'black', 'grey'), labels = c(current_year, '1979-2000 Mean'), breaks=c('L', 'M')) +
    geom_line(data = filter(dataset, year == current_year),
              aes(dummy_date, temp, color='L'), linewidth = 1.3) +
    theme(legend.position="top") + theme(legend.key.size = unit(0.5, 'cm'),
                                         legend.margin = margin(5, 0, 0, 0))

if (anomaly) plot <- plot +
          geom_rect(data = filter(dataset, year == current_year),
          aes(xmin = dummy_date - 1,
              xmax = dummy_date,
              ymin = mean_temp,
              ymax = mean_temp + temp_anom,
              fill = temp_anom)) +
          geom_line(data = filter(dataset, year == current_year),
            aes(dummy_date, temp, color='L'), linewidth = 1.3) +
  labs(subtitle = '2-meter temperature since 1979, mean, and current anomaly')

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}
