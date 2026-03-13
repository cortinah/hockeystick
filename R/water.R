library(quantmod)

library(readxl)
water <- read_excel("water.xlsx", sheet = "EUR Timeseries")
water <- water[, c(1, 2, 11)]

water$Date <- as.Date(water$Date, format = "%Y-%m-%d")
water$Date <- as.POSIXct(as.character(water$Date), tz = 'America/New_York')

w <- xts(water$`Broad UCITS PROT1 EUR GR`, order.by = water$Date)
b <- xts(water$`STOXX World AC All Cap EUR GR`, order.by = water$Date)

rets <- merge(w, b)
rets <- Return.calculate(rets)

Return.cumulative(rets)

TrackingError(rets$w, rets$b)

chart.RelativePerformance(spret$SPX.Adjusted, spret$SPY.Adjusted)

TrackingError(spret$SPY.Adjusted, spret$RSP.Adjusted)
