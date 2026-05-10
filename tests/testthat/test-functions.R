context("Behavioral tests using fixture data")

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
