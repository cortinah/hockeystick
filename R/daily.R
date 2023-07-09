library(tidyverse)
library(jsonlite)
library(splitstackshape)
library(RColorBrewer)

# pull daily temperature from https://climatereanalyzer.org/clim/t2_daily/
temp_json <- fromJSON("https://climatereanalyzer.org/clim/t2_daily/json/cfsr_world_t2_day.json")


# can also use unnest() and seq(as.Date())
# create dataframe
temp <- as.data.frame(temp_json) %>%
  cSplit('data', ',') %>%
  mutate(data_001 = parse_number(data_001),
         data_366 = parse_number(data_366)) %>%
  head(-3) %>%
  pivot_longer(2:ncol(.), names_to = "day") %>%
  rename(year = name,
         temp = value) %>%
  mutate(year = parse_number(year),
         day = parse_number(day)) %>%
  filter(complete.cases(.)) %>%
  group_by(day) %>%
  mutate(baseline_1991_2020 = mean(temp[year >= 1991 & year <= 2020]),
         temp_anom = temp-baseline_1991_2020) %>%
  ungroup()


# draw plot
color_scale <- brewer.pal(9, "YlOrRd")

ggplot(temp) +
  geom_line(aes(day, temp, group = year), colour = "grey") +
  geom_rect(data = . %>% filter(year == 2023),
            aes(xmin = lag(day, default = 1),
                xmax = day,
                ymin = baseline_1991_2020,
                ymax = baseline_1991_2020 + temp_anom,
                fill = temp_anom)) +
  scale_fill_gradientn(colors = color_scale) +
  geom_line(aes(day, baseline_1991_2020, group = year), colour = "black", linetype = "dashed") +
  geom_line(data = . %>% filter(year == 2023), aes(day, temp, group = year), colour = "black") +
  theme_minimal()
