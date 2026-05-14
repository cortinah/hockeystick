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



