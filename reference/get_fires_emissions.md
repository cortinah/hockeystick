# Download and plot essential climate data

Retrieves global wildfire greenhouse gas emissions data from the Global
Wildfire Information System (GWIS), operated by the Copernicus Emergency
Management Service.
<https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend>

## Usage

``` r
get_fires_emissions(
  place = "WORLD",
  year = as.numeric(format(Sys.Date(), "%Y")),
  use_cache = TRUE,
  write_cache = getOption("hs_write_cache")
)
```

## Arguments

- place:

  (string) Region or country for which to retrieve data. May be one of
  the following regional aggregates: `"WORLD"` (default), `"UN_EUR"`
  (Europe), `"UN_AFR"` (Africa), `"UN_ASI"` (Asia), `"UN_AME"`
  (Americas), or `"UN_OCE"` (Oceania). Alternatively, supply an ISO
  3166-1 alpha-3 country code such as `"USA"`, `"CAN"`, or `"BRA"`.

- year:

  (numeric) Four-digit year for which to retrieve data. Must be 2012 or
  later. Defaults to the current year.

- use_cache:

  (boolean) Return cached data if available, defaults to TRUE. Use FALSE
  to fetch updated data.

- write_cache:

  (boolean) Write data to cache, defaults to FALSE. Use TRUE to write
  data to cache for later use. Can also be set using
  options(hs_write_cache=TRUE)

## Value

Invisibly returns a tibble with weekly and cumulative wildfire emissions
data for the requested place and year. The `plt` column identifies the
pollutant (e.g. `"CO2"`, `"CO"`, `"PM2.5"`). Weekly value columns
include `curv` (current), `minv`, `maxv`, and `avgv`. Cumulative columns
are prefixed with `cum_`.

## Details

`get_fires_emissions` invisibly returns a tibble with weekly and
year-to-date cumulative wildfire emissions by pollutant, sourced from
the GWIS emissions API. Historical minimum, maximum, and average
baselines are included for comparison. Data coverage begins in 2012.
Filter on the `plt` column to select a specific pollutant (e.g.
`filter(plt == "CO2")`). Cache filenames include the place and year so
that multiple queries can be cached independently.

## References

Global Wildfire Information System (GWIS), Copernicus Emergency
Management Service / European Commission Joint Research Centre.
<https://gwis.jrc.ec.europa.eu/apps/gwis.statistics/seasonaltrend>

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
# Fetch global emissions data for the current year from cache if available:
emissions <- get_fires_emissions()
#
# Fetch CO2 emissions for the USA in 2023:
emissions_usa <- get_fires_emissions(place = 'USA', year = 2023)
#
# Force cache refresh:
emissions <- get_fires_emissions(use_cache = FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details() # }
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
```
