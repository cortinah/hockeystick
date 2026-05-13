library(httr2)
library(jsonlite)
library(dplyr)
library(purrr)

# ── Configuration ─────────────────────────────────────────────────────────────

BASE_URL <- "https://api2.effis.emergency.copernicus.eu/statistics/v2/gwis/weeklyaoi"

# Candidate AOI codes based on UN geoscheme used by GWIS.
# The script will probe each one and keep only the valid ones.
candidate_aois <- c(
  "UN_EUR",   # Europe
  "UN_AFR",   # Africa
  "UN_ASI",   # Asia
  "UN_AME",   # Americas (try both)
  "UN_NAM",   # North America
  "UN_SAM",   # South America
  "UN_OCE",   # Oceania
  "UN_GLOB",  # Global
  "GLOB",
  "WORLD",
  "UN_BORA",  # Boreal Asia
  "UN_TPAS",  # Tropical Asia
  "UN_MIDE",  # Middle East
  "UN_CAMS",  # Central America
  "UN_NHSA",  # Northern Hemisphere South America
  "UN_SHSA",  # Southern Hemisphere South America
  "UN_NAFE",  # North Africa
  "UN_SSAF",  # Sub-Saharan Africa
  "UN_AUST"   # Australia
)

# GWIS data starts from 2012 (VIIRS sensor); go up to current year
years <- 2012:2026

# ── Helper: fetch one AOI + year ──────────────────────────────────────────────

fetch_gwis <- function(aoi, year) {
  resp <- tryCatch(
    request(BASE_URL) |>
      req_url_query(aoi = aoi, year = year) |>
      req_error(is_error = \(r) FALSE) |>   # don't throw on HTTP errors
      req_perform(),
    error = function(e) NULL
  )

  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  raw <- tryCatch(
    resp |> resp_body_string() |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )

  if (is.null(raw)) return(NULL)
  df_raw <- raw$banfweekly
  if (is.null(df_raw) || !is.data.frame(df_raw) || nrow(df_raw) == 0) return(NULL)

  df_raw |>
    as_tibble() |>
    mutate(
      year            = as.integer(year),
      date            = as.Date(mddate, format = "%Y%m%d"),
      cum_area_ha     = cumsum(area_ha),
      cum_area_ha_avg = cumsum(area_ha_avg),
      cum_area_ha_min = cumsum(area_ha_min),
      cum_area_ha_max = cumsum(area_ha_max)
    ) |>
    select(aoi, year, week, date,
           area_ha, cum_area_ha,
           area_ha_avg, cum_area_ha_avg,
           area_ha_min, cum_area_ha_min,
           area_ha_max, cum_area_ha_max,
           events, events_avg)
}

# ── Step 1: discover valid AOI codes using 2026 as probe year ─────────────────

message("Probing AOI codes...")

valid_aois <- keep(candidate_aois, function(aoi) {
  result <- fetch_gwis(aoi, 2026)
  valid  <- !is.null(result)
  message(sprintf("  %-12s %s", aoi, if (valid) "✓" else "✗"))
  valid
})

message(sprintf("\nValid AOIs found: %s\n", paste(valid_aois, collapse = ", ")))

# ── Step 2: fetch all valid AOIs × all years ──────────────────────────────────

message("Fetching all regions × years...")

all_data <- map_dfr(valid_aois, function(aoi) {
  map_dfr(years, function(yr) {
    message(sprintf("  %s  %d", aoi, yr))
    result <- fetch_gwis(aoi, yr)
    Sys.sleep(0.2)   # be polite to the API
    result
  })
})

message(sprintf("\nDone. %d rows fetched across %d regions and %d years.",
                nrow(all_data), n_distinct(all_data$aoi), n_distinct(all_data$year)))

# ── Step 3: save ──────────────────────────────────────────────────────────────

out_file <- "gwis_cumulative_burnt_area_all.csv"
write.csv(all_data, out_file, row.names = FALSE)
message(sprintf("Saved to %s", out_file))

# ── Quick preview ─────────────────────────────────────────────────────────────

all_data |>
  group_by(aoi, year) |>
  summarise(total_ha = max(cum_area_ha), .groups = "drop") |>
  tidyr::pivot_wider(names_from = year, values_from = total_ha) |>
  print(n = Inf)
