library(quantmod)
library(PerformanceAnalytics)

getSymbols('^SPX', from = '2017-01-01')
getSymbols('SPY', from = '2017-01-01')
getSymbols('RSP', from = '2017-01-01')

sp <- merge(SPX$SPX.Adjusted, SPY$SPY.Adjusted, RSP$RSP.Adjusted)

spret <- Return.calculate(sp)
spret <- spret['2021/']
Return.annualized(spret)

TrackingError(spret$SPX.Adjusted, spret$SPY.Adjusted)

chart.RelativePerformance(spret$SPX.Adjusted, spret$SPY.Adjusted)


