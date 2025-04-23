library(quantmod)
library(tidyverse)

getSymbols('^SPX', from='1970-01-01')
ma50 <- rollmean(SPX,k=50, fill='NA', align = 'right')
ma200 <- rollmean(SPX,k=200, fill='NA', align = 'right')

# 1,3,6,9,12 month
SPX <- merge.xts(SPX$SPX.Close, ma50$SPX.Close, ma200$SPX.Close)
rm(list=ls(pattern = 'ma'))

calc <- SPX
names(calc)<-c('SPX', 'ma50', 'ma200')
plot(calc['2024::'], legend.loc = 'top', grid.ticks.lty = 3)
fwd1 <- lag.xts(SPX, k=-21*1)
fwd2 <- lag.xts(SPX, k=-21*2)
fwd3 <- lag.xts(SPX, k=-21*3)
fwd6 <- lag.xts(SPX, k=-21*6)
fwd9 <- lag.xts(SPX, k=-21*9)
calc <- merge.xts(calc, fwd1$SPX.Close,fwd2$SPX.Close, fwd3$SPX.Close, fwd6$SPX.Close, fwd9$SPX.Close)
names(calc)<-c('SPX', 'ma50', 'ma200', 'fwd1', 'fwd2', 'fwd3', 'fwd6', 'fwd9')
rm(list=ls(pattern = 'fwd'))

calc <- data.frame(date=index(calc), spx=calc$SPX, ma50=calc$ma50, ma200=calc$ma200, mo1=calc$fwd1,
                   mo2=calc$fwd2, mo3=calc$fwd3, mo6=calc$fwd6, mo9=calc$fwd9)
rownames(calc) <- NULL
calc <- calc |> mutate(ind=sign(ma50-ma200))
calc <- calc |> mutate(swap=ind==lag(ind))
calc <- calc |> mutate(dc=(!swap & (ind==-1)))
calc |> filter(dc==T) -> dc

dc |> select(date, SPX, starts_with('fwd')) -> dc
dc |> mutate(ret1=fwd1/SPX-1, ret2=fwd2/SPX-1, ret3=fwd3/SPX-1, ret6=fwd6/SPX-1,ret9=fwd9/SPX-1) -> dc
dc |> drop_na() -> dc

dc |> summarize(across(starts_with('ret'), mean))
dc |> summarize(across(starts_with('ret'), fivenum))
dc |> select(starts_with('ret')) |> sign() |> summarize_all(\(x) (x>0)) |> summarize_all(sum)/nrow(dc)

ggplot(dc,aes(x=ret1)) + geom_bar(fill='darkgrey') + scale_y_continuous(breaks = seq(0,10,2)) +
  scale_x_binned(n.breaks = 6) +theme_bw() +labs(x='return')

