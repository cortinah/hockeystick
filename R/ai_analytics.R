Sys.setenv(TZ='America/New_York')
locale(tz='America/New_York')

library(riingo)
library(gt)
library(xts)
library(tidyverse)
#riingo_set_token=.renviron

symbols <- c(`MSCI`='MSCI',`Morningstar`='MORN',`S&P Global`='SPGI', `Moody\'s Corp`='MCO', `FactSet Research Systems`='FDS', `Broadridge Financial Solutions`='BR',
             'Thomson Reuters'='TRI')
             #, 'LSEG'='LSEGY')


download <- riingo_prices(symbols, start_date = '2025-05-07', end_date = '2026-05-06')
download |> select(ticker, date, adjClose) -> prices
prices$date <- force_tz(prices$date, "America/New_York")

prices |> pivot_wider(names_from = ticker, values_from = adjClose, id_cols = date) -> prices
prices$date <- as.POSIXct(prices$date, tz='America/New_York')


as.xts(prices[,-1], order.by = prices$date) -> prices


library(quantmod)
library(PerformanceAnalytics)

returns <- Return.calculate(prices)

day <- Return.cumulative(returns['2026-05-06'])
ytd <- Return.cumulative(returns['2026-01-01/']) # ytd
year <- Return.cumulative(returns['/1 year']) # one year


table <- as.data.frame(rbind(day,ytd,year))
rownames(table) <- NULL

table <- cbind(as.factor(c("Today", 'YTD', "1 Year")),table)
colnames(table)[1] <- 'Period'

gt(table) -> rettable

pivot_longer(table, cols = 2:8,names_to = "ticker",values_to = 'return') -> table

library(ggplot2)
library(forcats)
table$Period <- as.factor(table$Period)
table$ticker <- as.factor(table$ticker)

table$Period<-fct_relevel(table$Period, "YTD", after=1)
table$Period<-fct_rev(table$Period)


table$ticker <- fct_relevel(table$ticker, colnames(ytd)[order(ytd)], after=0L)

#table$ticker<-fct_rev(table$ticker)

library(RColorBrewer)
colors <- brewer.pal(n = 3, name="Set1")

ggplot(table, aes(y=return, x=ticker, fill=Period)) + geom_bar(position = 'dodge', stat='identity') +
  theme_bw(base_size = 13) +scale_y_continuous(labels = scales::percent_format()) +
  labs(y='Return', x= NULL, title='Analytics stocks market performance', subtitle='As of May 5, 2026') +
  scale_x_discrete() +theme(legend.position = 'top') +
  scale_fill_manual(breaks = c('Today', 'YTD', '1 Year'), values = c("Today"=colors[1], "YTD"='orange', "1 Year"='brown')) +
  theme(axis.text.x = element_text(face="bold"))
###
as.data.frame(rettable) -> rettable
rettable |> select(Period,FDS, TRI, MORN, BR, SPGI, MCO, MSCI) -> rettable
rettable |>mutate(across(2:8, as.numeric)) -> rettable

gt(rettable) |> fmt_percent(columns = 2:8,decimals = 1) |> tab_header(title = "Data analytics equity market performance",
                                                    subtitle = "As of May 6, 2026") |>
  opt_stylize(style = 6, color='blue') |> tab_footnote("Symbols - FDS: FactSet, TRI: Thomson Reuters, MORN: Morningstar, BR: Broadridge, SPGI: S&P Global, MCO: Moody's, MSCI: MSCI.")

