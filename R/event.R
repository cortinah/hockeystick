library(hockeystick)
library(dplyr)
old <- options(pillar.sigfig = 4)
options(digits=4)
### Global heat

t <- get_temp(use_cache = T, write_cache = F)

t |> rowwise() |> mutate(ytd=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct))) |> ungroup() -> t

#Jan-Oct actual: 1.121

t |> mutate(diff=`J-D`-ytd) -> t


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

# plot
library(ggplot2)
d |> filter(year==2022 | year==2023 | year==2021) |> filter(dummy_date > as.Date("1975-09-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=1) + geom_smooth() + theme_minimal() +labs(color='Year')

plot_dailytemp()

## results data
yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')
yearly<-yearly[-1, 1:2]

#highest ever has been 1.02

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Sep-Dec < 1.11
((1.1049 * 12) - sum(t[144,2:9]))/4 # 1.195  1.195 - 0.45 = 0.745
