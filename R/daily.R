library(tidyverse)
library(jsonlite)
#library(splitstackshape)
library(RColorBrewer)

# pull daily temperature from https://climatereanalyzer.org/clim/t2_daily/
temp_json <- fromJSON('https://climatereanalyzer.org/clim/t2_daily/json/cfsr_world_t2_day.json')

leap_years <- seq.int(1980, 2028, 4)


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
  mutate(mean_temp = mean(temp[year >= 1979 & year <= 2000]),
         temp_anom = temp - mean_temp) |>
  ungroup()


dates <- data.frame(day_of_year=1:365,
                    date=seq(as.Date('1975-01-01'), as.Date('1975-12-31'), by = '1 days'))


daily_temperature <- left_join(daily_temperature, dates)

# draw plot
ggplot(daily_temperature) +
  geom_line(aes(date, temp, group = year), color = 'grey', alpha=0.7) +
  scale_fill_gradientn(name='Anomaly (C\U00B0)', colors = brewer.pal(9, 'YlOrRd')) +
  geom_line(aes(date, mean_temp), color = 'black', linetype = 'dashed', linewidth=1.1) +
  geom_line(data = filter(daily_temperature, year == as.numeric(substr(Sys.Date(), 1, 4))), aes(date, temp), colour = "firebrick", linewidth=2) +
  scale_y_continuous(limits=c(11,18), breaks=11:18) +
  theme_bw(base_size = 12) +
  scale_x_date(name=element_blank(), breaks=c(as.Date('1975-01-01'), as.Date('1975-04-01'),
                                            as.Date('1975-07-01'),as.Date('1975-10-01'),
                                            as.Date('1975-12-31')),
               date_labels = '%b-%d',
               date_minor_breaks = '1 month') +

  geom_rect(data = filter(daily_temperature, year == as.numeric(substr(Sys.Date(),1,4))),
            aes(xmin = date-1,
                xmax = date,
                ymin = mean_temp,
                ymax = mean_temp + temp_anom,
                fill = temp_anom)) +
  theme(legend.position="top") + theme(legend.key.size = unit(0.5, 'cm'),
                                       legend.margin = margin(5, 0, 0, 0)) +
  labs(title='Global Daily Average 2-Meter Air Temperature', subtitle='Daily since 1979, 1979-2000 mean, and current anomaly',
       y='Temperature (C\U00B0)', caption='Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org')
