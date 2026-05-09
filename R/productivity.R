library(fredr)
library(tidyverse)

prod <- fredr(
  series_id = "OPHNFB",
  observation_start = as.Date("1950-01-01"),
  observation_end = as.Date("2026-04-01"),
  frequency = "q"
)

prod |> mutate(date=rollforward(prod$date+65))-> prod
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

prod |>
  mutate(prev28 = lag(value, 28)) |>
  mutate(change28 = (value / prev28)^(1 / 7) - 1) -> prod
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


ggplot(prod, aes(x = date, y = change28)) +
  geom_line() +
  theme_minimal() +
  scale_x_date(
    date_breaks = "5 years",
    date_labels = "%Y",
    limits = c(as.Date("1955-01-01"), as.Date("2026-01-01"))
  ) +
  scale_y_continuous(labels = scales::percent, n.breaks = 8) +
  labs(y = 'Seven-year Annual rate', title = 'US Productivity Growth', x=NULL) +
  geom_smooth()

ggplot(prod, aes(x=date, y=log(value))) +geom_line() + geom_smooth(method ='lm') +
  theme_minimal(base_size = 14) +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y",
    limits = c(as.Date("1950-12-31"), as.Date("2026-12-31"))
  ) +
  scale_y_continuous(n.breaks = 4) +
  labs(y = 'log productivity', title = 'US Labor Productivity', x=NULL)


ggplot(prod, aes(x=date, y=value)) +geom_line() + geom_smooth() +
  theme_minimal(base_size = 14) +
  scale_x_date(
    date_breaks = "10 years",
    date_labels = "%Y",
    limits = c(as.Date("1950-12-31"), as.Date("2026-12-31"))
  ) +
  scale_y_continuous(n.breaks = 4) +
  labs(y = 'Productivity', title = 'US Labor Productivity', x=NULL)
