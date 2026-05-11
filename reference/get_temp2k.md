# Download and plot essential climate data

Retrieves the Common Era Global Surface Temperature Reconstructions.
Source is PAGES2k Consortium and NOAA National Centers for Environmental
Information. <https://www.ncei.noaa.gov/access/paleo-search/study/26872>

## Usage

``` r
get_temp2k(use_cache = TRUE, write_cache = getOption("hs_write_cache"))
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

Invisibly returns a tibble with filtered and unfiltered temperature
reconstructions and Cowtan & Way instrumental temperatures.

`get_temp2k` invisibly returns a tibble with the PAGES2k Consortium
temperature reconstruction (years 1-2000 CE) and instrumental record
(years 1850-2017 CE). Temperatures represent deviations from the
1961-1990 mean.

The returned object includes annual average temperature anomalies as
well as filtered anomalies using a 31-year Butterworth filter.
Reconstructions use seven different statistical methods that draw from a
global collection of temperature-sensitive palaeoclimate records.

Methodology described in PAGES2k (2019)
<https://www.nature.com/articles/s41561-019-0400-0>

## References

- PAGES2k Common Era Surface Temperature Reconstructions.
  <https://www.ncei.noaa.gov/access/paleo-search/study/26872>

- PAGES 2k Consortium., Neukom, R., Barboza, L.A. et al. Consistent
  multidecadal variability in global temperature reconstructions and
  simulations over the Common Era. *Nat. Geosci.* 12, 643–649 (2019).
  [doi:10.1038/s41561-019-0400-0](https://doi.org/10.1038/s41561-019-0400-0)

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
# Fetch temp anomaly from cache if available:
anomaly <- get_temp2k()
#
# Force cache refresh:
anomaly <- get_temp2k(use_cache=FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details()
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#
# Plot output using package's built-in ggplot2 settings
plot_temp2k(anomaly) # }

```
