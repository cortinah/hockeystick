library(hockeystick)
library(dplyr)
old <- options(pillar.sigfig = 4)
options(digits=4)
### Global heat

t <- get_temp(use_cache = T, write_cache = F)

t |> rowwise() |> mutate(ytd=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct))) |> ungroup() -> t

#Jan-Nov 2023 actual: 1.123

t |> mutate(diff = `J-D`-ytd) -> t


tail(head(t,-1), 7) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff
slice_tail(t, n=1) |> select(ytd) |> as.numeric() + temp_diff


# Implies annual avg: 1.102 20-yr
# Implies annual avg: 1.10 10-yr
# Implies annual avg: 1.09 7-yr
# Implies annual avg: 1.098 5-yr


## daily temps

d <- get_dailytemp(use_cache = F, write_cache = T)

d |> filter(year!=2023) |> group_by(year) |> filter(dummy_date>=as.Date('1975-11-01')) |>
  filter(dummy_date<as.Date('1975-12-01')) |>
  summarize(dailyanom=mean(temp_anom)) -> dailyanom

colnames(dailyanom)[1]='Year'; dailyanom$Year <- as.Date(paste0(as.character(dailyanom$Year),'-12-31'))

t <- left_join(t, dailyanom)

# update month
t |> mutate(dailydiff = Nov - dailyanom) -> t


tail(head(t,-1), 7) |> summarize(anom=mean(dailydiff)) |> as.numeric() -> daily_diff

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-11-01')) |>
  filter(dummy_date<as.Date('1975-12-01')) |>
  summarize(anom=mean(temp_anom)) |> as.numeric() -> mtddavg
#Nov to date: 0.984


### force Aug

t <- get_temp()

t |> rowwise() |> mutate(ytd=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct))) |> ungroup() -> t

t |> mutate(diff = `J-D`- ytd) -> t

tail(head(t, -1), 7) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff_new

# August
mean(c(as.numeric(tail(t,1)[2:8]), 0.77049 + 0.4999)) + -0.007857 ##1.051


# September
mean(c(as.numeric(tail(t,1)[2:9]), 0.9803 + 0.4511)) + -0.0050 ##1.086

# October
mean(c(as.numeric(tail(t,1)[2:10]), 0.9211 + 0.4261)) + -0.0050 ##1.116


# November
mean(c(as.numeric(tail(t,1)[2:11]), mtddavg + daily_diff)) + temp_diff_new

# ALL --> 1.135
mean(c(as.numeric(tail(t,1)[2:11]), mtddavg + daily_diff, 0.69 + daily_diff))

## results data

yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')

yearly<-yearly[-1, 1:2]

#highest ever has been 1.02

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Sep-Dec < 1.11

((1.1049 * 12) - sum(t[144,2:9]))/4 # 1.195  1.195 - 0.45 = 0.745


#####################

l <- data.frame(Name=c('H','M','N','A'), Amount=c(1,1,5,1),col=c('blue','yellow','red','orange'))

library(ggplot2)

ggplot(l,aes(x=Name,y=Amount,fill=col)) +geom_col() + theme_minimal() + guides(fill="none") +

  labs(title='Weekly Laundry Quantity',subtitle='Relative Amounts', caption='Source: 171 W. 76th Laundry Services',

       y='Amount by Weight', x='Individual')


#### Month ####
library(tidyverse)
library(hockeystick)
options(pillar.sigfig = 3)

d <- get_dailytempcop(use_cache = FALSE, write_cache = TRUE)
tail(d, 5)

d |> filter(year==2016 | year==2023 | year==2024) |> filter(dummy_date > as.Date("1925-01-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_smooth(se = F) + scale_y_continuous(n.breaks=6) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Temp Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%b") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue"))


d |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-12-01")) |> group_by(year) |> summarize(mtd=mean(temp_anom)) |> slice_max(n=10, order_by=mtd)
d |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-12-01")) |> group_by(year) |> summarize(mtd=mean(temp_anom)) |> slice_max(n=10, order_by=mtd) |> pull(mtd, name=year) |> rev() |> diff()


d |> filter(year==2023 | year==2024) |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-11-01")) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> print(n=31)


d |> filter(year==2023 | year==2023 | year==2024) |> filter(dummy_date >= as.Date("1925-12-01") & dummy_date <= as.Date("1925-12-31")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + theme(legend.position = 'top')

# FORECAST REST OF MONTH
extra <- tail(d, 1)

# Get latest
fcst <- pull(extra[1, "temp_anom"])

daysfc <- as.numeric(as.Date("2024-12-31")-as.Date("2024-01-01"))

extra <- data.frame(year=rep(2024, daysfc-pull(extra[1,2])), day_of_year=(pull(extra[1,2])+1):daysfc, date=NA,temp=NA,`1979-2000 mean`=NA, temp_anom=fcst,dummy_date=pull(extra[1,7])+1:(daysfc-pull(extra[1,2])))

colnames(extra) <- colnames(d)
mutate(extra, date = as.Date(paste0(year, '-', substr(dummy_date,6,7), '-', substr(dummy_date, 9, 10)))) -> extra

f <- bind_rows(d,extra)

f |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-11-01")) |> group_by(year) |> summarize(mtd=round(mean(temp_anom),digits = 2)) |> slice_max(n=10, order_by=mtd)
f |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-11-01")) |> group_by(year) |> summarize(mtd=mean(temp_anom)) |> slice_max(n=10, order_by=mtd) |> pull(mtd, name=year) |> rev() |> diff()


f |> filter(year==2022 | year==2024 | year==2023) |> filter(dummy_date >= as.Date("1925-12-01") & dummy_date <= as.Date("1925-12-31")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + theme(legend.position = 'top')

f |> filter(year==2023 | year==2024) |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-10-01")) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> tail(31)

# Fable
library(fable)
library(fable.prophet)

fcst <- d |> select(date, y=temp_anom) |> tail(365*5)

train <- as_tsibble(fcst,index=date) |> tsibble::fill_gaps() |>
  tidyr::fill(y, .direction = "down")

fit <- train |>
  model(
   arima = ARIMA(y),
    ets = ETS(y),
   prophet = prophet(y)
)

fc <- fit |> forecast(h='1 month')
fc |> autoplot()
accuracy(fit)

f <- left_join(f, fc |> filter(.model=='arima')) |> rename(arima=.mean) |> select(-y,-.model)
f <- left_join(f, fc |> filter(.model=='ets')) |> rename(ets=.mean) |> select(-y,-.model)
f <- left_join(f, fc |> filter(.model=='prophet')) |> rename(prophet=.mean) |> select(-y,-.model)


f |> filter(year==2023 | year==2024) |> filter(dummy_date >= as.Date("1925-08-01") & dummy_date < as.Date("1925-09-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='Red: extend, Blue: ARIMA, Black: ETS, Orange: Prophet', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + geom_point(aes(y=arima), color='dodgerblue', size=1) + geom_point(aes(y=ets), color='black', size=1) +
  geom_point(aes(y=prophet), color='orange', size=1) + theme(legend.position='top')

# substitute projection into temp_anom

f[(nrow(f)-nrow(extra)+1):nrow(f),'temp_anom'] <- f[(nrow(f)-nrow(extra)+1):nrow(f),'arima']

f |> filter(year==2023 | year==2024) |> filter(dummy_date >= as.Date("1925-08-01") & dummy_date < as.Date("1925-09-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='Red: extend, Blue: ARIMA, Orange: ETS, Purple: Prophet', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + geom_point(aes(y=arima), color='blue', size=1) + geom_point(aes(y=ets), color='orange', size=1) +
  geom_point(aes(y=prophet), color='purple', size=1) + theme(legend.position='top')

f |> filter(dummy_date < as.Date("1925-09-01"), dummy_date >= as.Date("1925-08-01")) |> group_by(year) |> summarize(month=round(mean(temp_anom),digits = 2)) |> slice_max(n=10, order_by=month)

f |> filter(year==2023 | year==2024) |> filter(dummy_date < as.Date("1925-09-01"), dummy_date >= as.Date("1925-08-01")) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> print(n=31)

## Prophet
#library(prophet)
# fcst <- d |> select(ds=date, y=temp_anom) |> tail(365*5)
# m <- prophet::prophet(fcst)
# future <- make_future_dataframe(m, periods = 23)
# forecast <- predict(m, future)
# plot(m, forecast)
#
# forecast |> select(ds, yhat) |> tail(31) -> forecast
# f |> tail(31) -> f
# f <- cbind(f, forecast)
#
#
# f |> filter(year==2016 | year==2024) |> filter(dummy_date >= as.Date("1925-03-01") & dummy_date < as.Date("1925-04-01")) |>
#   ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
#   theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
#   scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + geom_point(aes(y=yhat), color='red')


### checks
d |> filter(dummy_date < as.Date("1925-06-01"), dummy_date >= as.Date("1925-05-01")) |> group_by(year) |> summarize(ytd=mean(temp_anom)) -> may
may |> filter(year!=2024) |> ggplot(aes(x=year, y=ytd)) + geom_col() + theme_bw()
may |> filter(year!=2024) |> arrange(-ytd)
#nasa march 2016: 1.34C, copernicus: 0.932

1.34-0.932 +1.08
1.34-0.932 +1.1

#feb gap: 0.29
1.4-1.11

0.29 +1.08

#
d |> filter(year==2023) |> mutate(month=substr(dummy_date,6,7)) |> group_by(month) |> summarise(avg=mean(temp_anom))

d |> mutate(month=substr(dummy_date,6,7)) |> filter(month=='05') |> group_by(year) |> summarise(avg=mean(temp_anom)) |> filter(year>=2013)



#### Arctic Sea ice ####
# https://nsidc.org/arcticseaicenews/sea-ice-tools/
# https://nsidc.org/arcticseaicenews/
# https://nsidc.org/sea-ice-today/sea-ice-tools/charctic-interactive-sea-ice-graph

library(tidyverse)

i <- get_icecurves(use_cache=F, write_cache = T)
i |> filter(year==2024) |> tail(1)
i |> filter(mo==3) |> arrange(-extent)
<<<<<<< HEAD
i |> filter(mo==3) |> filter(year %in% c(2024, 2023, 2022, 2021))

# to adjust for daily variation
#i |> mutate(extmin=extent*0.94) -> i

i |> mutate(extmin=extent*1) -> i

=======
i |> filter(mo==3) |> filter(year %in% c(2024, 2023, 2022, 2021)) |> arrange(extent)

# to adjust for daily variation
#i |> mutate(extmin=extent*0.94) -> i
i |> mutate(extmin=extent*1) -> i

>>>>>>> 98368c5e8c28ef052853f8a2c5b1959519130d99
i |> plot_icecurves() + geom_hline(yintercept = 14.0) + geom_hline(yintercept = 14.2)

fcst <- i |> mutate(date=tsibble::make_yearmonth(year=year, month=mo)) |> arrange(date) |> select(date, extmin)

# Fable
library(tsibble)
library(fable)
library(fable.prophet)

fcst <- fcst |> select(date, y=extmin) |> tail(12*8)

train <- as_tsibble(fcst, index=date)

fit <- train |>
  model(
    arima = ARIMA(y),
    ets = ETS(y),
    prophet = prophet(y)
  )

accuracy(fit)


fc <- fit |> forecast(h='5 month')
fc |> autoplot(level = 75) + geom_hline(yintercept = 14.0) + scale_y_continuous(n.breaks = 10)
fc |> filter(.model=='ets') |> autoplot(level = 50) + geom_hline(yintercept = 14.2) + scale_y_continuous(n.breaks = 10) + geom_hline(yintercept = 14)
fc |> filter(date==tsibble::make_yearmonth(2025,03)) |> hilo(level = 66)

fcst <- fc |> filter(.model=='arima') |> rename(arima=.mean) |> select(-y,-.model) |> full_join(fcst)
fcst <- fc |> filter(.model=='ets') |> rename(ets=.mean) |> select(-y,-.model) |> full_join(fcst)
fcst <- fc |> filter(.model=='prophet') |> rename(prophet=.mean) |> select(-y,-.model) |> full_join(fcst)

fcst |> filter(date==yearmonth('2025 Mar')) |> as_tibble() |> select(prophet,ets,arima) |> as.matrix() |> min()


fcst |> ggplot(aes(x=as.Date(date), y=y)) + geom_point(size=0) + geom_line(linewidth=1,color='darkgreen') + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='Arctic Sea Ice', subtitle='Green: actual, Blue: ARIMA, Black: ETS, Orange: Prophet', x='Date',color ='Year',y='MM sqkm') +
  scale_x_date(date_labels="%m/%y") + geom_line(aes(y=arima), color='dodgerblue', size=1) + geom_line(aes(y=ets), color='black', size=1) +
  geom_line(aes(y=prophet), color='orange', size=1) + theme(legend.position='top')



#### Year 2024 ###########
library(dplyr)
library(ggplot2)
library(hockeystick)

d <- get_dailytempcop(use_cache = FALSE, write_cache = TRUE)
tail(d,5)

d |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd)
d |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd) |> pull(ytd, name=year) |> rev() |> diff()


d |> filter(year==2023 | year==2024) |>   ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) +theme(legend.position = 'top')

# FORECAST REST OF YEAR
avgdays <- 5
extra <- tail(d, avgdays)


# Get latest
fcst <- mean(pull(extra[,"temp_anom"]))
#fcst <- fcst*.75
#fcst <- 0.44

daysfc <- as.numeric(as.Date("2024-12-31")-as.Date("2024-01-01"))

extra <- data.frame(year=rep(2024, daysfc-pull(extra[avgdays,2])), day_of_year=(pull(extra[avgdays,2])+1):daysfc, date=NA,temp=NA,`1979-2000 mean`=NA, temp_anom=fcst,dummy_date=pull(extra[avgdays,7])+1:(daysfc-pull(extra[avgdays,2])))

colnames(extra) <- colnames(d)
mutate(extra, date = as.Date(paste0(year, '-', substr(dummy_date,6,7), '-', substr(dummy_date, 9, 10)))) -> extra

f <- bind_rows(d,extra)

f |> group_by(year) |> summarize(ytd=round(mean(temp_anom),digits = 2)) |> slice_max(n=10, order_by=ytd)
f |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd) |> pull(ytd, name=year) |> rev() |> diff()


f |> filter(year==2023 | year==2024 | year==2022) |> ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + theme(legend.position = 'top')

f |> filter(year==2023 | year==2024) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> print(n=31)


# Fable
library(fable)
library(fable.prophet)

fcst <- d |> select(date, y=temp_anom) |> tail(365*7)

train <- as_tsibble(fcst,index=date) |> tsibble::fill_gaps() |>
  tidyr::fill(y, .direction = "down")

fit <- train |>
  model(
    arima = ARIMA(y),
    ets = ETS(y),
    prophet = prophet(y)
  )

fc <- fit |> forecast(h='8 month')
fc |> autoplot()
accuracy(fit)

f <- left_join(f, fc |> filter(.model=='arima')) |> rename(arima=.mean) |> select(-y,-.model)
f <- left_join(f, fc |> filter(.model=='ets')) |> rename(ets=.mean) |> select(-y,-.model)
f <- left_join(f, fc |> filter(.model=='prophet')) |> rename(prophet=.mean) |> select(-y,-.model)


f |> filter(year==2020 | year==2024) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='Red: extend, Blue: ARIMA, Black: ETS, Orange: Prophet', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + geom_point(aes(y=arima), color='dodgerblue', size=1) + geom_point(aes(y=ets), color='black', size=1) +
  geom_point(aes(y=prophet), color='orange', size=1) + theme(legend.position='top')

# substitute projection into temp_anom

f[(nrow(f)-nrow(extra)+1):nrow(f),'temp_anom'] <- f[(nrow(f)-nrow(extra)+1):nrow(f),'ets']

f |> filter(year==2023 | year==2024) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 12) +labs(title='World Daily Average Air Temperature', subtitle='Red: extend, Blue: ARIMA, Orange: ETS, Purple: Prophet', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue")) + geom_point(aes(y=arima), color='blue', size=1) + geom_point(aes(y=ets), color='orange', size=1) +
  geom_point(aes(y=prophet), color='purple', size=1) + theme(legend.position='top')

f |> group_by(year) |> summarize(month=round(mean(temp_anom),digits = 2)) |> slice_max(n=10, order_by=month)



#### hottest day ever ####
options(pillar.sigfig = 4)
maxtemp <- d |> group_by(year) |> top_n(n = 1, wt = temp)

maxtemp |> ggplot(aes(x=year, y=temp)) +geom_point(color='red', size=2.5) + geom_line(color='dodgerblue', linewidth=1) + theme_bw() + scale_y_continuous(n.breaks = 8) +
  scale_x_continuous(n.breaks = 20) +labs(x='Year', y='Highest Annual Temperature (C)', title='Highest-Ever Recorded Global Temperature on July 22, 2024',
                                          caption='Source: EU Copernicus Climate Service\n cds.climate.copernicus.eu as of 2024-07-22')



maxtemp |> ggplot(aes(x=year, y=temp)) + geom_segment( aes(x=year, xend=year, y=15, yend=temp), linetype = 2, linewidth = 0.1) +
  geom_point( size=3, color="red")  + theme_bw(base_size = 11) + scale_y_continuous(n.breaks = 10, limits = c(15,17.25)) +
  scale_x_continuous(n.breaks = 10) +labs(x=element_blank(), y='Highest Annual Temperature (C)', title='Highest-Ever Recorded Global Average Temperature on July 22, 2024',
                                          caption='Source: EU Copernicus Climate Service\n cds.climate.copernicus.eu as of 2024-07-22') +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()) + geom_smooth()

d |> top_n(wt = temp, n = 5) |> arrange(-temp)


#### Forecast Monthly NCEI temp ####
library(tidyverse)
library(hockeystick)
ncei <- read_csv("R/ncei_december.csv")
cop <- get_dailytempcop(use_cache = F)
monthforecast <- 'Dec'
months = 1:12; names(months) = month.abb
start <- paste("1925",months[monthforecast],"01",sep='-')
end <- ceiling_date(as.Date(start),"month")-1



nceihistory <- ncei |> filter(Year>=2022) |> select(Anomaly) |> pull() |> mean()
cophistory <- cop |> filter(year>=2022, year<2024) |> filter(dummy_date >=as.Date(start), dummy_date <=as.Date(end)) |> summarize(mean(temp_anom)) |> pull()
gap <- nceihistory - cophistory

start <- paste("2024",months[monthforecast],"01",sep='-')
end <- ceiling_date(as.Date(start),"month")-1

fcstcop <- cop |> filter(date>=as.Date(start), date <=as.Date(end)) |> select(temp_anom) |> summarize(mean(temp_anom)) |> pull()
fcstloti <- round(fcstcop + gap, 2)

f |> filter(dummy_date <= as.Date("1925-12-31"), dummy_date >= as.Date("1925-11-01")) |> group_by(year) |> summarize(mtd=round(mean(temp_anom),digits = 2)) |> slice_max(n=2, order_by=mtd) |> filter(year==2024) |> pull(mtd) -> fcstcop

# FRED
library(fredr)
fredr_set_key("48da82a29bba2cf57b2cd0be421f9d47")

employees <- fredr(series_id = "CES9091000001",
  observation_start = as.Date("1970-01-01"),
  observation_end = as.Date("2024-01-01"), frequency = "m")

ggplot(employees, aes(x=date, y=value)) + geom_line() + scale_x_date() + theme_bw()

employees |> mutate(month=month(date)) -> employees

employees |> filter(month==1) -> employees
employees |> mutate(prev=lag(value)) |> mutate(change=value-prev) -> employees

ggplot(drop_na(employees), aes(x=date, y=change)) +geom_col(color='black',fill='darkorange') +scale_x_date(date_labels = "'%y", date_breaks = "2 year") +
  scale_y_continuous(n.breaks = 8) +labs(title='Federal Employment', x='Year', y='Annual Change in Fed Employment',caption='Source: FRED') +theme_bw()
