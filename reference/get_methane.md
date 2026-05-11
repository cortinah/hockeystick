# Download and plot essential climate data

Retrieves globally averaged marine surface methane monthly mean data
from National Oceanic and Atmospheric Administration. The Global
Monitoring Division of NOAA’s Earth System Research Laboratory has
measured methane since 1983 at a globally distributed network of air
sampling sites. A global average is constructed by first smoothing the
data for each site as a function of time, and then smoothed values for
each site are plotted as a function of latitude. Global means are
calculated from the latitude plot at each time step.
<https://gml.noaa.gov/ccgg/trends_ch4/>
<https://gml.noaa.gov/ccgg/about/global_means.html>

## Usage

``` r
get_methane(use_cache = TRUE, write_cache = getOption("hs_write_cache"))
```

## Arguments

- use_cache:

  (boolean) Return cached data if available, defaults to TRUE. Use FALSE
  to fetch updated data.

- write_cache:

  (boolean) Write data to cache, defaults to FALSE. Use TRUE to write
  data to cache for later use. Can also be set using
  options(hs_write_cache=TRUE)

## Value

Invisibly returns a tibble with the monthly methane series

## Details

`get_methane` invisibly returns a tibble with NOAA's monthly globally
averaged methane measurement. The returned object includes year, month,
date, average, average uncertainty, trend, and trend uncertainty
columns. Trend is NOAA's published trend. Please refer to above website
for details. CH4 expressed as a mole fraction in dry air, nanomol/mol,
abbreviated as ppb.

## References

Lan, X., K.W. Thoning, and E.J. Dlugokencky: Trends in globally-averaged
CH4, N2O, and SF6 determined from NOAA Global Monitoring Laboratory
measurements. Version 2022-11,
[doi:10.15138/P8XG-AA10](https://doi.org/10.15138/P8XG-AA10)

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
# Fetch from cache if available, otherwise download:
ch4 <- get_methane()
#
# Force fetch from source:
ch4 <- get_methane(use_cache=FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details()
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#
# Plot output using package's built-in ggplot2 settings
plot_methane(ch4) # }

```
