library(hockeystick)
library(dplyr)
# burricanes
h |> filter(Year>=2000) |> summarise(across(everything(), list(median,mean)))

# median 7, 3 (major)

#sea ice
a <- get_icecurves()
plot_icecurves(a)

a |> filter(mo==7 & year >=2015 & year <=2022) |> summarise(julyavg=mean(extent)) |> as.numeric() -> july

a |> filter(mo==9 & year >=2015 & year <=2022) |> summarise(julyavg=mean(extent)) |> as.numeric() -> sep

a |> filter(mo==7 & year==2023) |> select(extent) |> as.numeric() -> current

sep/july * current
# 4.79

js <- a |> filter(mo ==7 | mo==9)

js <- pivot_wider(js, names_from=mo, values_from = extent)
js |> mutate(ratio=`9` / `7`) |> filter(year>=2000 & year <2023) -> js
js |> summarize(mean=mean(ratio), sd=sd(ratio)) -> js

# ratio = 0.607 +- 0.0592  0.4888 to 0.7257
plot_icecurves(a) +geom_hline(yintercept = 4.8)
# buy 4.98??

### Global heat

t <- get_temp()

t |> rowwise() |> mutate(h1=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul))) |> ungroup() -> t
#h1 1.0342
t |> mutate(ratio=`J-D`/h1, diff=`J-D`-h1) -> t


head(t,-1) |> summarize(ratio=mean(ratio)) |> as.numeric() -> temp_ratio #1.0476
tail(head(t,-1),10) |> summarize(ratio=mean(ratio)) |> as.numeric() -> temp_ratio #1.0014
tail(head(t,-1),10) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff


tail(t,1) |> select(h1) |> as.numeric() * temp_ratio
#1.0528 buy 1.05 - 1.07
slice_tail(t,n=1) |> select(h1) |> as.numeric() + temp_diff

tail(t,20) |> ggplot(aes(x=1, y=diff)) +geom_violin() +geom_jitter()

## daily temps


d <- get_dailytemp(use_cache = F)

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-08-01')) |> filter(dummy_date<as.Date('1975-09-01')) |> summarize(anom=mean(temp_anom))

d |> filter(year==2023) |> filter(dummy_date<as.Date('1975-08-01')) |> summarize(anom=mean(temp_anom))
# gap: 1.034 -0.617 = 0.417

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-07-01')) |> filter(dummy_date<as.Date('1975-08-01')) |> summarize(anom=mean(temp_anom))

### force Aug


t <- get_temp()

t |> rowwise() |> mutate(h1=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug))) |> ungroup() -> t

t |> mutate(ratio=`J-D`/h1, diff=`J-D`-h1) -> t

#t |> filter(ratio > 0) -> t

tail(head(t,-1),7) |> summarize(ratio=mean(ratio)) |> as.numeric() -> temp_ratio_new #1.030
tail(head(t,-1),10) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff_new

t <- get_temp()
mean(c(as.numeric(tail(t,1)[2:8]),0.743+0.417)) * temp_ratio_new #1.066

mean(c(as.numeric(tail(t,1)[2:8]),0.743+0.417)) + temp_diff_new #1.044

#highest ever has been 1.01

yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')
yearly<-yearly[-1,1:2]

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Aug-Dec
((1.0449 *12) - sum(t[144,2:8]))/5 #1.0597

#####################################################
#####################################################
#####################################################

### Global heat
old <- options(pillar.sigfig = 4)
t <- get_temp(use_cache = F)

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
#
# d |> filter(year==2023) |> filter(dummy_date<as.Date('1975-08-01')) |> summarize(anom=mean(temp_anom)) -> ytdavg
# ytdgap <- tail(t,1)['h1'] - ytdavg |> as.numeric()
#
# d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-07-01')) |> filter(dummy_date<as.Date('1975-08-01')) |> summarize(anom=mean(temp_anom))
# # gap: 1.18-0.83 = 0.35


d |> filter(year!=2023) |> group_by(year) |> filter(dummy_date>=as.Date('1975-08-01')) |> filter(dummy_date<as.Date('1975-09-01')) |>
  summarize(anom=mean(temp_anom)) -> auganom; colnames(auganom)[1]='Year'
auganom$Year <- as.Date(paste0(as.character(auganom$Year),'-12-31'))

t <- left_join(t, auganom)
t |> mutate(augdiff=Aug - anom) -> t


tail(head(t,-1), 10) |> summarize(anom=mean(anom, na.rm = T)) |> as.numeric() -> aug_diff

d |> filter(year==2023) |> filter(dummy_date>=as.Date('1975-08-01')) |> filter(dummy_date<as.Date('1975-09-01')) |>
  summarize(anom=mean(temp_anom)) |> as.numeric() -> augtdavg
#Aug to date: 0.753


### force Aug

t <- get_temp()

t |> rowwise() |> mutate(h1=mean(c(Jan,Feb,Mar,Apr,May,Jun,Jul,Aug))) |> ungroup() -> t

t |> mutate(diff=`J-D`-h1) -> t

tail(head(t,-1), 7) |> summarize(diff=mean(diff)) |> as.numeric() -> temp_diff_new

mean(c(as.numeric(tail(t,1)[2:8]), augtdavg + aug_diff)) + temp_diff_new #1.0035

#highest ever has been 1.01

yearly<-read.csv("https://data.giss.nasa.gov/gistemp/graphs/graph_data/Global_Mean_Estimates_based_on_Land_and_Ocean_Data/graph.txt", skip = 2,sep = '')
yearly<-yearly[-1,1:2]

mean(as.numeric(tail(t,4)[1,2:13]))

## remaining Aug-Dec
((1.0449 *12) - sum(t[144,2:8]))/5 #1.0597

