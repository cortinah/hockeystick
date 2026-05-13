library(httr2)
library(jsonlite)
library(dplyr)
# https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend

# ── Parameters ────────────────────────────────────────────────────────────────
aoi  <- "UN_EUR"   # Area of interest (e.g. UN_EUR, UN_AFR, UN_ASI, UN_AME, UN_OCE)
aoi <- 'ALB'
year <- 2026       # Year to retrieve

# ── Fetch ─────────────────────────────────────────────────────────────────────
url <- paste0(
  "https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/weeklyaoi",
  "?aoi=", aoi, "&year=", year
)

url <- paste0(
  "https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/weekly",
  "?country=", aoi, "&year=", year
)


https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/weekly?country=ALB&year=2026

# https://api2.effis.emergency.copernicus.eu/statistics/v2/emissions/weeklyaoi?aoi=UN_EUR&year=2026
# EMISSIONS
# statistics/v2/ta/weeklyaoi THERMAL ANOMALIES
# /statistics/v2/dsr/weeklyaoi SEVERE EVENTS

resp <- request(url) |> req_perform(verbosity = 1)

raw <- resp |>
  resp_body_string() |>
  fromJSON(flatten = TRUE)

# ── Parse & compute cumulative burnt area ─────────────────────────────────────
df <- raw$banfweekly |>
  as_tibble() |>
  mutate(
    date            = as.Date(mddate, format = "%Y%m%d"),
    cum_area_ha     = cumsum(area_ha),        # cumulative: current year
    cum_area_ha_avg = cumsum(area_ha_avg),    # cumulative: historical average
    cum_area_ha_min = cumsum(area_ha_min),    # cumulative: historical min
    cum_area_ha_max = cumsum(area_ha_max)     # cumulative: historical max
  ) |>
  select(aoi, year = week, date, week,
         area_ha, cum_area_ha,
         area_ha_avg, cum_area_ha_avg,
         area_ha_min, cum_area_ha_min,
         area_ha_max, cum_area_ha_max,
         events, events_avg)


#=======
ggplot(df, aes(x = date)) +theme_minimal() +geom_ribbon(data=df,aes(ymin=cum_area_ha_min, ymax=cum_area_ha_max), fill='gray90') + geom_line(aes(y=cum_area_ha),col='red') + geom_line(data=df, aes(y=cum_area_ha_avg),col="dodgerblue") +
  labs(y='Cumulative Burnt Area (thousanfs of ha)', x=NULL, title='Wildfire Burnt Area') + scale_y_continuous(labels = scales::label_comma(scale = .001))



ggplot(df, aes(x = date)) +theme_minimal() +geom_ribbon(data=df,aes(ymin=area_ha_min, ymax=area_ha_max),fill='gray90') + geom_line(aes(y=area_ha),col='red') + geom_line(data=df, aes(y=area_ha_avg),col="blue")




ggplot(df, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_area_ha_min, ymax=cum_area_ha_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_area_ha, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_area_ha_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Burnt Area (thousands of ha)', x=NULL, title='2026 Wildfire Area Burnt', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")

# num fires
ggplot(df, aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_events_min, ymax=cum_events_max, fill='Min - Max Range\n(since 2012)')) +
  geom_line(aes(y=cum_events, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_events_avg, col="Average\n(since 2012)")) +
  labs(y='Cumulative Number', x=NULL, title='2026 Number of Wildfires', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma()) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2012)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")


####### emissions

e |> filter(plt=='CO2') |> ggplot(aes(x = date)) +theme_bw(base_size = 13) +geom_ribbon(aes(ymin=cum_minv, ymax=cum_maxv, fill='Min - Max Range\n(since 2003)')) +
  geom_line(aes(y=cum_curvs, col='Year-to-date'), linewidth=1.1) + geom_line(aes(y=cum_avvg, col="Average\n(since 2003)")) + labs(y='Cumulative Emissions (millions of tons)', x=NULL, title='2026 Emissions from Wildfires', subtitle='World',caption='Source: GWIS') + scale_y_continuous(n.breaks = 8, labels = scales::label_comma(scale = .000001)) +
  scale_colour_manual("",values=c("red", "dodgerblue"), breaks=c("Year-to-date","Average\n(since 2003)"))+
  scale_fill_manual('', values="grey90") +theme(legend.position = "top")



