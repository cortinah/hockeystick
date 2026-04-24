library(fredr)
library(tidyverse)

prod <- fredr(
  series_id = "OPHNFB",
  observation_start = as.Date("1950-01-01"),
  observation_end = as.Date("2026-01-01"),
  frequency = "q"
)

prod |> mutate(month = month(date)) -> prod
tail(prod)

prod |>
  mutate(prev1 = lag(value, 1)) |>
  mutate(change1 = ((value / prev1)^4) - 1) -> prod

prod |>
  mutate(prev4 = lag(value, 4)) |>
  mutate(change4 = (value / prev4) - 1) -> prod
tail(prod)

prod |>
  mutate(prev12 = lag(value, 12)) |>
  mutate(change12 = (value / prev12)^(1 / 3) - 1) -> prod
tail(prod)

prod |>
  mutate(prev20 = lag(value, 20)) |>
  mutate(change20 = (value / prev20)^(1 / 5) - 1) -> prod
tail(prod)

plot(prod$change4, type = 'l')
library(ggplot2)

prod |> drop_na(change20) |> ggplot(aes(x = date, y = change20)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("1950-01-01"), as.Date("2026-01-01"))
  ) +
  scale_y_continuous(labels = scales::percent, n.breaks = 8) +
  labs(y = '5-year rate (annualized)', title = 'US Productivity Growth', x=NULL) +
  geom_smooth()


ggplot(prod, aes(x = date, y = change4)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("1955-01-01"), as.Date("2026-01-01"))
  ) +
  scale_y_continuous(labels = scales::percent, n.breaks = 8) +
  labs(y = 'Annual rate', title = 'US Productivity Growth', x=NULL) +
  geom_smooth()

