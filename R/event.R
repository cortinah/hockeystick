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


##########################

# plot Feb kalshi
library(dplyr)
library(ggplot2)
library(hockeystick)

d <- get_dailytemp(use_cache = F, write_cache = T)

d |> filter(year==2016 | year==2023 | year==2024) |> filter(dummy_date > as.Date("1925-01-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_smooth(se = F) + scale_y_continuous(n.breaks=6) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Temp Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%b") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue"))


d |> filter(dummy_date < as.Date("1925-03-01"), dummy_date > as.Date("1925-01-31")) |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd)
d |> filter(dummy_date < as.Date("1925-03-01"), dummy_date > as.Date("1925-01-31")) |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd) |> pull(ytd, name=year) |> rev() |> diff()


d |> filter(year==2016 | year==2024) |> filter(dummy_date < as.Date("1925-03-01"), dummy_date > as.Date("1925-01-31")) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> print(n=29)


d |> filter(year==2016 | year==2023 | year==2024) |> filter(dummy_date >= as.Date("1925-02-01") & dummy_date < as.Date("1925-03-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue"))

# FORECAST REST OF MONTH
extra <- tail(d,1)

# 0.28 is breakeven as of Feb 2
fcst <- pull(extra[1,6])

extra <- data.frame(year=rep(2024, 60-pull(extra[1,2])), day_of_year=(pull(extra[1,2])+1):60, date=NA,temp=NA,`1979-2000 mean`=NA, temp_anom=fcst,dummy_date=pull(extra[1,7])+1:(60-pull(extra[1,2])))
f <- bind_rows(d,extra)

f |> filter(dummy_date < as.Date("1925-03-01"), dummy_date >= as.Date("1925-02-01")) |> group_by(year) |> summarize(ytd=round(mean(temp_anom),digits = 2)) |> slice_max(n=10, order_by=ytd)
f |> filter(dummy_date < as.Date("1925-03-01"), dummy_date >= as.Date("1925-02-01")) |> group_by(year) |> summarize(ytd=mean(temp_anom)) |> slice_max(n=10, order_by=ytd) |> pull(ytd, name=year) |> rev() |> diff()


f |> filter(year==2016 | year==2024) |> filter(dummy_date >= as.Date("1925-02-01") & dummy_date < as.Date("1925-03-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=0) + geom_line(linewidth=1) + scale_y_continuous(n.breaks=12) +
  theme_bw(base_size = 13) +labs(title='World Daily Average Air Temperature', subtitle='2-meter air temperature', x='Date',color ='Year',y='Anomaly (C)', caption = paste0("Source: Climate Change Institute, University of Maine\nClimateReanalyzer.org as of ", pull(tail(d,1)["date"]))) +
  scale_x_date(date_labels="%m/%d") + scale_color_manual(values = c("darkgreen", "red", "dodgerblue"))


f |> filter(year==2016 | year==2024) |> filter(dummy_date < as.Date("1925-03-01"), dummy_date > as.Date("1925-01-31")) |> select(year, dummy_date, temp_anom) |> tidyr::pivot_wider(names_from = year, values_from = temp_anom) |> print(n=29)

