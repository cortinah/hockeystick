library(hockeystick)
library(dplyr)
old <- options(pillar.sigfig = 4)
options(digits=4)
### Global heat

t <- get_temp()

t |> rowwise() |> mutate(h1=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul))) |> ungroup() -> t

#Jan-Jul actual: 1.034

t |> mutate(diff=`J-D`-h1) -> t


tail(head(t,-1), 10) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff
slice_tail(t,n=1) |> select(h1) |> as.numeric() + temp_diff

# Implies annual avg: 1.043 20-yr
# Implies annual avg: 1.039 10-yr
# Implies annual avg: 1.019 7-yr
# Implies annual avg: 1.034 5-yr


## daily temps

d <- get_dailytemp(use_cache = F)

d |> filter(year!=2023) |> group_by(year) |> filter(dummy_date>=as.Date('1975-08-01')) |> filter(dummy_date<as.Date('1975-09-01')) |>
  summarize(dailyanom=mean(temp_anom)) -> dailyanom; colnames(dailyanom)[1]='Year'
dailyanom$Year <- as.Date(paste0(as.character(dailyanom$Year),'-12-31'))

t <- left_join(t, dailyanom)
t |> mutate(dailydiff= Aug - dailyanom) -> t


tail(head(t,-1), 10) |> summarize(anom=mean(dailydiff)) |> as.numeric() -> daily_diff

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-08-01')) |> filter(dummy_date<as.Date('1975-09-01')) |>
  summarize(anom=mean(temp_anom)) |> as.numeric() -> mtddavg
#Aug to date: 0.753


### force Aug

t <- get_temp()

t |> rowwise() |> mutate(h1=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug))) |> ungroup() -> t

t |> mutate(diff=`J-D`-h1) -> t

tail(head(t,-1), 10) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff_new

mean(c(as.numeric(tail(t,1)[2:8]), mtddavg + daily_diff)) + temp_diff_new #1.0035


## results data
yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')
yearly<-yearly[-1, 1:2]

#highest ever has been 1.02

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Aug-Dec
((1.0449 *12) - sum(t[144,2:8]))/5 #1.0597

