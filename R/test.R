library(quantmod)
library(PerformanceAnalytics)

getSymbols('BRKU', from = '2024-12-11')
getSymbols('BRKW', from = '2024-12-11')
getSymbols('BRK-B', from = '2022-12-11')

chartSeries(`BRK-B`, type='line',TA=NULL,theme = chartTheme('white',up.col='orange',dn.col='red'))

sp <- merge(BRKU$BRKU.Adjusted, BRKW$BRKW.Adjusted, `BRK-B`$'BRK-B.Adjusted')

spret <- Return.calculate(sp)
spret <- spret['2024/']
Return.annualized(spret)

TrackingError(spret$BRKU.Adjusted, spret$BRK.B.Adjusted)

chart.RelativePerformance(spret$BRKU.Adjusted, spret$BRKW.Adjusted)



######## Normalized plots
normalize_to_100 <- function(x) {
  if (length(x) == 0 || is.na(x[1])) return(x) # handle empty or NA
  return((x / as.numeric(x[1])) * 100)
}

ts1_norm <- normalize_to_100(BRKU$BRKU.Adjusted)
ts2_norm <- normalize_to_100(`BRK-B`$'BRK-B.Adjusted')
ts3_norm <- normalize_to_100(`BRKW`$'BRKW.Adjusted')

# Combine into one xts object
combined <- merge(ts1_norm, ts2_norm, ts3_norm)
colnames(combined) <- c("BRKU", "BRK", "BRKW")

# Plot both series in one chart
plot(combined,
     multi.panel = FALSE,
     col = c("blue", "red","orange"),
     lwd = 2,
     main = "Multiple xts Series Normalized to 100",
     ylab = "Index (Start = 100)",
     legend.loc = "top")
