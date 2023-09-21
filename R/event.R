library(hockeystick)
library(dplyr)
old <- options(pillar.sigfig = 4)
options(digits=4)
### Global heat

t <- get_temp(use_cache = F, write_cache = T)

t |> rowwise() |> mutate(ytd=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug))) |> ungroup() -> t

#Jan-Aug actual: 1.06

t |> mutate(diff=`J-D`-ytd) -> t


tail(head(t,-1), 5) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff
slice_tail(t,n=1) |> select(ytd) |> as.numeric() + temp_diff

# Implies annual avg: 1.069 20-yr
# Implies annual avg: 1.067 10-yr
# Implies annual avg: 1.051 7-yr
# Implies annual avg: 1.064 5-yr


## daily temps

d <- get_dailytemp(use_cache = F, write_cache = T)

d |> filter(year!=2023) |> group_by(year) |> filter(dummy_date>=as.Date('1975-09-01')) |>
  filter(dummy_date<as.Date('1975-10-01')) |>
  summarize(dailyanom=mean(temp_anom)) -> dailyanom

colnames(dailyanom)[1]='Year'; dailyanom$Year <- as.Date(paste0(as.character(dailyanom$Year),'-12-31'))

t <- left_join(t, dailyanom)
t |> mutate(dailydiff = Sep - dailyanom) -> t


tail(head(t,-1), 7) |> summarize(anom=mean(dailydiff)) |> as.numeric() -> daily_diff

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-09-01')) |>
  filter(dummy_date<as.Date('1975-10-01')) |>
  summarize(anom=mean(temp_anom)) |> as.numeric() -> mtddavg
#Sep to date: 0.710


### force Aug

t <- get_temp()

t |> rowwise() |> mutate(ytd=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep))) |> ungroup() -> t

t |> mutate(diff = `J-D`- ytd) -> t

tail(head(t, -1), 7) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff_new

# August
mean(c(as.numeric(tail(t,1)[2:8]), 0.77049 + 0.4999)) + -0.007857 ##1.056

# September
mean(c(as.numeric(tail(t,1)[2:9]), mtddavg + daily_diff)) + temp_diff_new ##1.084 Sep 14

# ALL --> 1.103
mean(c(as.numeric(tail(t,1)[2:9]), mtddavg + daily_diff, 0.69 + daily_diff,0.69 + daily_diff,0.69 + daily_diff))

# plot
library(ggplot2)
d |> filter(year==2023 | year==2022) |> filter(dummy_date > as.Date("1975-09-01")) |>
  ggplot(aes(x=dummy_date, y=temp_anom, color=as.factor(year))) + geom_point(size=1) + geom_smooth() + theme_minimal()

plot_dailytemp()

## results data
yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')
yearly<-yearly[-1, 1:2]

#highest ever has been 1.02

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Sep-Dec < 1.11
((1.1049 * 12) - sum(t[144,2:9]))/4 # 1.195  1.195 - 0.45 = 0.745
