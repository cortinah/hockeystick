# ---------------------------------------------------------------------------
# Fixture data — mirrors the structure returned by each get_* function.
# All tests use these fixtures so no network access is required.
# ---------------------------------------------------------------------------

carbon_fixture <- tibble::tibble(
  year    = c(2020L, 2020L, 2020L),
  month   = c(1L, 2L, 3L),
  date    = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
  average = c(412.5, 413.4, 414.1),
  trend   = c(411.8, 412.7, 413.4),
  ndays   = c(28L, 27L, 30L),
  stdev   = c(0.5, 0.4, 0.5),
  unc     = c(0.1, 0.1, 0.1)
)

temp_fixture <- tibble::tibble(
  Year = as.Date(c("1980-12-31", "1981-12-31", "1982-12-31")),
  Jan  = c(0.20, 0.30, 0.10),
  Feb  = c(0.10, 0.20, 0.15),
  Mar  = c(0.30, 0.40, 0.20),
  Apr  = c(0.20, 0.30, 0.10),
  May  = c(0.10, 0.20, 0.00),
  Jun  = c(0.00, 0.10, -0.10),
  Jul  = c(0.10, 0.20, 0.00),
  Aug  = c(0.20, 0.30, 0.10),
  Sep  = c(0.30, 0.40, 0.20),
  Oct  = c(0.40, 0.50, 0.30),
  Nov  = c(0.30, 0.40, 0.20),
  Dec  = c(0.20, 0.30, 0.10),
  `J-D` = c(0.20, 0.30, 0.10),
  `D-N` = c(0.20, 0.30, 0.10),
  DJF   = c(0.15, 0.25, 0.12),
  MAM   = c(0.20, 0.30, 0.10),
  JJA   = c(0.10, 0.20, 0.00),
  SON   = c(0.33, 0.43, 0.23)
)

seaice_fixture <- tibble::tibble(
  date   = as.Date(c("1979-07-31", "1980-07-31", "1981-07-31")),
  extent = c(8.5, 8.2, 8.0)
)

icecurves_fixture <- {
  df <- tibble::tibble(
    year   = rep(c(2022L, 2023L), each = 12L),
    month  = rep(1:12, 2L),
    extent = c(14.0, 15.0, 14.0, 13.0, 11.0, 10.0,  7.0,  5.0,  6.0,  8.0, 11.0, 13.0,
               13.0, 14.0, 13.0, 12.0, 10.0,  9.0,  7.0,  5.5,  6.5,  8.5, 10.5, 12.5)
  )
  attr(df, "hs_ice_type") <- c("N", "extent")
  df
}

hurricanes_fixture <- data.frame(
  Year                   = 1990:1995,
  RevisedNamedStorms     = c(12, 8, 6, 8, 7, 19),
  RevisedHurricanes      = c( 8, 4, 4, 4, 3, 11),
  RevisedMajorHurricanes = c( 1, 2, 1, 1, 0,  3),
  RevisedACE             = c(91, 34, 75, 38, 32, 228),
  RevisedUShurricanes    = c( 1, 0, 1, 1, 0,  2)
)

emissions_fixture <- tibble::tibble(
  country             = rep("World", 5L),
  year                = 2000:2004,
  co2                 = c(25000, 26000, 27000, 28000, 29000),
  land_use_change_co2 = c(3000, 3100, 3200, 3300, 3400)
)

merged_fixture <- tibble::tibble(
  Date    = as.Date(c("2020-01-31", "2020-02-29", "2020-03-31")),
  Year    = c(2020, 2020, 2020),
  Carbon  = c(412.5, 413.4, 414.1),
  Anomaly = c(0.80, 0.90, 1.00)
)

# ---------------------------------------------------------------------------
# Input validation — no network access required
# ---------------------------------------------------------------------------

test_that("get_seaice rejects invalid pole", {
  expect_error(get_seaice(pole = "X"), "pole must be")
})

test_that("get_seaice rejects invalid month", {
  expect_error(get_seaice(month = 13), "Month must be")
})

test_that("get_seaice rejects invalid measure", {
  expect_error(get_seaice(measure = "volume"), "measure must be")
})

test_that("get_icecurves rejects invalid pole", {
  expect_error(get_icecurves(pole = "E"), "pole must be")
})

test_that("get_icecurves rejects invalid measure", {
  expect_error(get_icecurves(measure = "volume"), "measure must be")
})

# ---------------------------------------------------------------------------
# Plot functions return ggplot objects given fixture data
# ---------------------------------------------------------------------------

test_that("plot_carbon returns a ggplot", {
  p <- plot_carbon(carbon_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_temp returns a ggplot", {
  p <- plot_temp(temp_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_temp_monthly returns a ggplot", {
  p <- plot_temp_monthly(temp_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_temp_scatter returns a ggplot", {
  p <- plot_temp_scatter(temp_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("warming_stripes returns a ggplot", {
  p <- warming_stripes(temp_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("warming_stripes stripe_only = TRUE returns a ggplot", {
  p <- warming_stripes(temp_fixture, stripe_only = TRUE, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_seaice returns a ggplot", {
  p <- plot_seaice(seaice_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_icecurves returns a ggplot", {
  p <- plot_icecurves(icecurves_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_hurricanes returns a ggplot", {
  p <- plot_hurricanes(hurricanes_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_hurricane_nrg returns a ggplot", {
  p <- plot_hurricane_nrg(hurricanes_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_emissions returns a ggplot", {
  p <- plot_emissions(emissions_fixture, print = FALSE, annot = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_emissions_with_land returns a ggplot", {
  p <- plot_emissions_with_land(emissions_fixture, print = FALSE, annot = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_carbontemp returns a ggplot", {
  p <- plot_carbontemp(merged_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

# ---------------------------------------------------------------------------
# NULL propagation — every plot function should return NULL silently
# ---------------------------------------------------------------------------

test_that("plot functions return NULL invisibly when passed NULL", {
  expect_null(plot_carbon(NULL,               print = FALSE))
  expect_null(plot_temp(NULL,                 print = FALSE))
  expect_null(plot_temp_monthly(NULL,         print = FALSE))
  expect_null(plot_temp_scatter(NULL,         print = FALSE))
  expect_null(warming_stripes(NULL,           print = FALSE))
  expect_null(plot_seaice(NULL,               print = FALSE))
  expect_null(plot_icecurves(NULL,            print = FALSE))
  expect_null(plot_hurricanes(NULL,           print = FALSE))
  expect_null(plot_hurricane_nrg(NULL,        print = FALSE))
  expect_null(plot_emissions(NULL,            print = FALSE))
  expect_null(plot_emissions_with_land(NULL,  print = FALSE))
  expect_null(plot_carbontemp(NULL,           print = FALSE))
})

# ---------------------------------------------------------------------------
# Fixtures for fires functions
# ---------------------------------------------------------------------------

fires_area_fixture <- tibble::tibble(
  place        = rep("WORLD", 4L),
  week         = 1:4,
  date         = as.Date(c("2024-01-07", "2024-01-14", "2024-01-21", "2024-01-28")),
  events       = c(100L, 120L, 110L, 130L),
  events_min   = c( 80L,  90L,  85L, 100L),
  events_max   = c(150L, 160L, 155L, 170L),
  events_avg   = c(110L, 115L, 112L, 120L),
  area_ha      = c(5000L, 6000L, 5500L, 7000L),
  area_ha_min  = c(3000L, 3500L, 3200L, 4000L),
  area_ha_avg  = c(4500L, 5000L, 4800L, 5500L),
  area_ha_max  = c(8000L, 9000L, 8500L, 9500L),
  cum_events      = c(100L,  220L,  330L,  460L),
  cum_events_min  = c( 80L,  170L,  255L,  355L),
  cum_events_max  = c(150L,  310L,  465L,  635L),
  cum_events_avg  = c(110L,  225L,  337L,  457L),
  cum_area_ha     = c(5000L, 11000L, 16500L, 23500L),
  cum_area_ha_min = c(3000L,  6500L,  9700L, 13700L),
  cum_area_ha_avg = c(4500L,  9500L, 14300L, 19800L),
  cum_area_ha_max = c(8000L, 17000L, 25500L, 35000L)
)

fires_emissions_fixture <- tibble::tibble(
  place      = rep("WORLD", 6L),
  date       = rep(as.Date(c("2024-01-07", "2024-01-14", "2024-01-21")), 2L),
  plt        = c(rep("CO2", 3L), rep("PM2.5", 3L)),
  curv       = c(100, 120, 110, 10, 12, 11),
  minv       = c( 80,  90,  85,  8,  9,  8),
  maxv       = c(150, 160, 155, 15, 16, 15),
  avgv       = c(110, 115, 112, 11, 11, 11),
  cum_curv   = c(100, 220, 330, 10, 22, 33),
  cum_minv   = c( 80, 170, 255,  8, 17, 25),
  cum_maxv   = c(150, 310, 465, 15, 31, 46),
  cum_avgv   = c(110, 225, 337, 11, 22, 33)
)

# ---------------------------------------------------------------------------
# Input validation — fires functions
# ---------------------------------------------------------------------------

test_that("get_fires_area rejects years before 2012", {
  expect_error(get_fires_area(year = 2011), "year must be greater than 2011")
})

test_that("get_fires_emissions rejects years before 2012", {
  expect_error(get_fires_emissions(year = 2011), "year must be greater than 2011")
})

# ---------------------------------------------------------------------------
# plot_fires_area — all var/style combinations return ggplot
# ---------------------------------------------------------------------------

test_that("plot_fires_area var=area style=cumulative returns a ggplot", {
  p <- plot_fires_area(fires_area_fixture, var = "area", style = "cumulative", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_fires_area var=area style=weekly returns a ggplot", {
  p <- plot_fires_area(fires_area_fixture, var = "area", style = "weekly", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_fires_area var=count style=cumulative returns a ggplot", {
  p <- plot_fires_area(fires_area_fixture, var = "count", style = "cumulative", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_fires_area var=count style=weekly returns a ggplot", {
  p <- plot_fires_area(fires_area_fixture, var = "count", style = "weekly", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

# ---------------------------------------------------------------------------
# plot_fires_emissions — pollutant/style combinations return ggplot
# ---------------------------------------------------------------------------

test_that("plot_fires_emissions pollutant=CO2 style=cumulative returns a ggplot", {
  p <- plot_fires_emissions(fires_emissions_fixture, pollutant = "CO2", style = "cumulative", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_fires_emissions pollutant=CO2 style=weekly returns a ggplot", {
  p <- plot_fires_emissions(fires_emissions_fixture, pollutant = "CO2", style = "weekly", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_fires_emissions pollutant=PM2.5 style=cumulative returns a ggplot", {
  p <- plot_fires_emissions(fires_emissions_fixture, pollutant = "PM2.5", style = "cumulative", print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

# ---------------------------------------------------------------------------
# NULL propagation — fires plot functions
# ---------------------------------------------------------------------------

test_that("plot_fires_area returns NULL invisibly when passed NULL", {
  expect_null(plot_fires_area(NULL, print = FALSE))
})

test_that("plot_fires_emissions returns NULL invisibly when passed NULL", {
  expect_null(plot_fires_emissions(NULL, print = FALSE))
})

# ---------------------------------------------------------------------------
# merge_carbontemp output structure
# ---------------------------------------------------------------------------

test_that("merge_carbontemp returns expected columns", {
  result <- merge_carbontemp(carbon = carbon_fixture, temp = temp_fixture)
  expect_true(is.data.frame(result))
  expect_true(all(c("Date", "Year", "Carbon", "Anomaly") %in% colnames(result)))
})

test_that("merge_carbontemp returns NULL when either input is NULL", {
  expect_null(merge_carbontemp(carbon = NULL,          temp = temp_fixture))
  expect_null(merge_carbontemp(carbon = carbon_fixture, temp = NULL))
})

# ---------------------------------------------------------------------------
# plot_hockeystick — spliced PAGES2k + NASA record
# ---------------------------------------------------------------------------

# Long format returned by get_temp2k: one row per year per measure.
temp2k_fixture <- tibble::tibble(
  year    = c(1L, 1000L, 1849L, 1850L, 1900L, 2017L),
  measure = c(rep("ensemble_median", 3L), rep("instrumental", 3L)),
  value   = c(-0.30, -0.40, -0.20, 0.10, 0.15, 0.30)
)

# get_temp format. Years must span the baseline window and extend past 2017 so
# the NASA branch of the splice is non-empty.
temp_hockey_fixture <- tibble::tibble(
  Year = as.Date(c("1965-12-31", "1975-12-31", "1985-12-31", "2020-12-31", "2021-12-31")),
  `J-D` = c(0.10, 0.15, 0.20, 1.10, 1.15),
  `D-N` = c(0.10, 0.15, 0.20, 1.10, 1.15)
)

test_that("plot_hockeystick returns a ggplot", {
  p <- plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_hockeystick no_legend defaults to FALSE", {
  expect_false(formals(plot_hockeystick)$no_legend)
})

test_that("plot_hockeystick color-codes sources by default", {
  p <- plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture, print = FALSE)

  expect_true("colour" %in% names(p$mapping))
  expect_false(is.null(p$scales$get_scales("colour")))
  expect_equal(p$theme$legend.position, "top")

  d <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(length(unique(d$group)), 3)
})

test_that("plot_hockeystick no_legend draws a single black line with no legend", {
  p <- plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture,
                        no_legend = TRUE, print = FALSE)

  expect_false("colour" %in% names(p$mapping))
  expect_null(p$scales$get_scales("colour"))
  expect_equal(p$theme$legend.position, "none")

  d <- ggplot2::ggplot_build(p)$data[[1]]
  expect_length(unique(d$group), 1)
  expect_true(all(d$colour == "black"))

  # No key is laid out in the rendered figure
  g <- ggplot2::ggplotGrob(p)
  expect_false(any(grepl("key", g$layout$name)))
})

test_that("plot_hockeystick no_legend plots the same data as the default", {
  pd <- plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture, print = FALSE)
  pn <- plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture,
                         no_legend = TRUE, print = FALSE)

  dd <- ggplot2::ggplot_build(pd)$data[[1]]
  dn <- ggplot2::ggplot_build(pn)$data[[1]]

  # Grouped geoms emit rows group-by-group, so compare after sorting by year
  expect_equal(dd$x[order(dd$x)], dn$x[order(dn$x)])
  expect_equal(dd$y[order(dd$x)], dn$y[order(dn$x)])
})

test_that("plot_hockeystick returns NULL invisibly when passed NULL", {
  expect_null(plot_hockeystick(dataset = NULL, temp = temp_hockey_fixture, print = FALSE))
  expect_null(plot_hockeystick(dataset = temp2k_fixture, temp = NULL, print = FALSE))
})

test_that("plot_hockeystick returns NULL when the baseline period has no data", {
  expect_message(
    expect_null(plot_hockeystick(dataset = temp2k_fixture, temp = temp_hockey_fixture,
                                 baseline = c(1850, 1860), print = FALSE)),
    "baseline offset"
  )
})

# ---------------------------------------------------------------------------
# plot_dailyanom — daily temperature anomaly
# ---------------------------------------------------------------------------

# Mirrors the layout returned by get_dailytempcop(): the 5th column holds the
# historic day-of-year mean, which plot_dailyanom renames to mean_temp.
# Temps are built as mean + offset so temp_anom is exactly the per-year offset.
daily_fixture <- {
  days  <- 1:10L
  years <- c(2018L, 2019L, 2020L)
  mean_daily <- seq(10, 19, length.out = length(days))
  offsets <- c(-0.2, 0.1, 0.4)

  df <- expand.grid(day_of_year = days, year = years)
  df <- df[order(df$year, df$day_of_year), ]
  m <- mean_daily[match(df$day_of_year, days)]
  o <- offsets[match(df$year, years)]

  out <- tibble::tibble(
    year            = df$year,
    day_of_year     = df$day_of_year,
    date            = as.Date(sprintf('%d-01-%02d', df$year, df$day_of_year)),
    temp            = m + o,
    `1991-2020 mean` = m,
    temp_anom       = o,
    dummy_date      = as.Date(sprintf('1925-01-%02d', df$day_of_year))
  )
  attr(out, "hs_daily_region") <- 'W'
  out
}

test_that("plot_dailyanom returns a ggplot", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})

test_that("plot_dailyanom has no anomaly argument", {
  expect_false("anomaly" %in% names(formals(plot_dailyanom)))
})

test_that("plot_dailyanom plots the anomaly rather than the temperature", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020, print = FALSE)
  d <- ggplot2::ggplot_build(p)$data[[1]]

  expect_equal(sort(d$y), sort(daily_fixture$temp_anom))
  # Absolute temperatures are an order of magnitude larger and must not appear
  expect_false(isTRUE(all.equal(sort(d$y), sort(daily_fixture$temp))))
})

test_that("plot_dailyanom draws the historic mean as a flat line at zero", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020, print = FALSE)
  b <- ggplot2::ggplot_build(p)

  expect_true(inherits(p$layers[[2]]$geom, "GeomLine"))
  expect_true(all(b$data[[2]]$y == 0))
  # The mean stays in the legend, so its period is still named
  expect_true("1991-2020 mean" %in% p$scales$get_scales("colour")$labels)
})

test_that("plot_dailyanom maxtemp shades deviation from historic max on the anomaly scale", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020, maxtemp = TRUE, print = FALSE)

  expect_true(inherits(p$layers[[4]]$geom, "GeomRect"))
  expect_match(p$labels$subtitle, "deviation from historic max")

  r <- ggplot2::ggplot_build(p)$data[[4]]
  expect_equal(nrow(r), sum(daily_fixture$year == 2020))
  # Bounds are anomalies, not absolute temperatures: ymin is max offset over
  # the other years (0.1) and ymax this year's offset (0.4). Derived by
  # subtracting absolute temps, so allow for floating point drift.
  expect_true(all(abs(r$ymin - 0.1) < 1e-6))
  expect_true(all(abs(r$ymax - 0.4) < 1e-6))
})

test_that("plot_dailyanom labels use a plotmath degree sign", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020, print = FALSE)

  # Labels are plotmath expressions rather than literal strings, keeping the
  # source ASCII-only. Assert the symbol is `degree`, which distinguishes a
  # correct label from one whose \U escape decoded to some other codepoint.
  expect_true(inherits(p$labels$y, "expression"))
  expect_true("degree" %in% all.names(p$labels$y))

  pm <- plot_dailyanom(daily_fixture, current_year = 2020, maxtemp = TRUE, print = FALSE)
  fill_name <- pm$scales$get_scales("fill")$name
  expect_true(inherits(fill_name, "expression"))
  expect_true("degree" %in% all.names(fill_name))
})

test_that("plot_dailyanom titles follow the dataset region", {
  air <- plot_dailyanom(daily_fixture, current_year = 2020, print = FALSE)
  expect_equal(air$labels$title, "World Daily Air Temperature Anomaly")
  expect_match(air$labels$subtitle, "since 1940")

  sea <- daily_fixture
  attr(sea, "hs_daily_region") <- 'WS'
  p <- plot_dailyanom(sea, current_year = 2020, print = FALSE)
  expect_equal(p$labels$title, "World (60S-60N) Daily Sea Surface Temperature Anomaly")
  expect_match(p$labels$subtitle, "since 1982")
})

test_that("plot_dailyanom honours a custom title", {
  p <- plot_dailyanom(daily_fixture, current_year = 2020,
                      title_lab = "Record Warmth", print = FALSE)
  expect_equal(p$labels$title, "Record Warmth")
})

test_that("plot_dailyanom returns NULL invisibly when passed NULL", {
  expect_null(plot_dailyanom(NULL, print = FALSE))
})

test_that("plot_dailyanom builds when current_year is absent from the data", {
  p <- plot_dailyanom(daily_fixture, current_year = 1900, print = FALSE)
  expect_true(inherits(p, "ggplot"))
})
