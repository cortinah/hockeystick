library(mapgl)
library(httr2)
library(sf)
library(dplyr)
library(glue)

my_key <- Sys.getenv("MAP_KEY")

area <- c("-72,-46,-63,-42")

get_nasa_data <- function(key, region) {
  request("https://firms.modaps.eosdis.nasa.gov/api/area/csv") %>%
    req_url_path_append(key, "VIIRS_SNPP_NRT", region, 2,format(Sys.time(), "%Y-%m-%d")) %>%
    req_perform() %>%
    resp_body_string() %>%
    read.csv(text = .) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
}

fires <- get_nasa_data(my_key, area)

fires <- fires %>%
  mutate(
    tooltip = glue(
      "<b>Date:</b> {acq_date}<br>",
      "<b>Time:</b> {acq_time} UTC<br>",
      "<b>Condidence:</b> {toupper(confidence)}<br>",
      "<b>Fire Radiative Power(FRP):</b> {round(frp, 2)} MW"
    )
  )

mapboxgl_view(fires)

mapboxgl() |>
  fit_bounds(fires) |>
  add_navigation_control(
    position = "top-left"
  ) |>
  add_circle_layer(
    id = "fires",
    source = fires,
    circle_color = match_expr(
      column = "confidence",
      values = c("l", "n", "h"),
      stops = c("#f1c40f", "#e67e22", "#e74c3c")
    ),
    circle_stroke_color = "white",
    circle_stroke_width = 1,
    circle_radius = interpolate('frp',values=c(0.7,3.5,11,30,157),
                                stops=c(1,3,6,15,30)),
    circle_opacity = 0.5,
    tooltip = "tooltip") |>
  add_legend(
    legend_title = "NASA FIRMS Confidence",
    values = c("Low", "Nominal", "High"),
    layer_id = "fires",
    type = "categorical",
    position = "top-right",
    interactive = TRUE,
    filter_column = "confidence",
    filter_values = c("l","n","h"),
    colors = c("#f1c40f", "#e67e22", "#e74c3c")
  ) |>
  add_screenshot_control(
    filename = "Fire_data",
    button_title = "Download",
    hide_controls = "TRUE",
    include_legend = "TRUE",
    position = "bottom-right"
  )


# other stuff: https://mhinfographics.com/tag/animation/

## old
fires <- fire_nrt_M_C61_649107 |>
  st_as_sf(coords = c("longitude", "latitude"), crs=4326)

library(geodata)
mexico <- elevation_30s(country = "Mexico", path=tempdir())
small <- aggregate(mexico, fact=4)
plot(mexico)
plot(fires['brightness'], add=T, pal=heat.colors(10), pch=20)

