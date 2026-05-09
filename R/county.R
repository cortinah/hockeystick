library(tidycensus)
library(mapgl)
library(viridisLite)
get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "NY",
  county = c("New York", "Kings"),
  geometry = TRUE) -> ny

ny |>
  maplibre_view(
    column = "estimate",
    palette = viridisLite::magma,

    interactive_legend = TRUE
  )


ggplot(ny) +geom_sf(aes(fill=estimate)) +theme_void() +
  scale_fill_viridis_c(option = "turbo",direction = -1)
