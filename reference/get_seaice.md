# Download and plot essential climate data

Retrieves Arctic or Antarctic annual Sea Ice Index (in million square
km). Source is the National Snow and Ice Data Center, defaults to Arctic
(Northern Hemisphere) July sea ice extent.
<https://nsidc.org/data/explore-data>  
  

## Usage

``` r
get_seaice(
  pole = "N",
  month = 7,
  measure = "extent",
  use_cache = TRUE,
  write_cache = getOption("hs_write_cache")
)
```

## Arguments

- pole:

  'N' for Arctic or 'S' for Antarctic

- month:

  2-digit month to retrieve sea ice for, defaults to 7 (July)

- measure:

  Must be 'extent' or 'area', defaults to 'extent'. Please see
  terminology link in references for details.

- use_cache:

  (boolean) Return cached data if available, defaults to TRUE. Use FALSE
  to fetch updated data, or to change pole or month in cache.

- write_cache:

  (boolean) Write data to cache, defaults to FALSE. Use TRUE to write
  data to cache for later use. Can also be set using
  options(hs_write_cache=TRUE)

## Value

Invisibly returns a tibble with the annual series of monthly Sea Ice
Index since 1979 (in million square km).

`get_seaice` invisibly returns a tibble with annual series of monthly
Sea Ice Index since 1979 (in million square km).

User may select Arctic or Antarctic sea ice extent or area (in millions
of square kilometers) by year for a given month, specified by argument
`month`. Defaults to Arctic July sea ice extent.
<https://nsidc.org/sea-ice-today/about-data#area_extent>

## References

- NSIDC Data Archive: <https://nsidc.org/data/explore-data>

- All About Sea Ice: <https://nsidc.org/learn/parts-cryosphere/sea-ice>

- Sea Ice terminology: <https://nsidc.org/learn>

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
# Fetch sea ice from cache if available:
seaice <- get_seaice()
#> Please set use_cache=FALSE if you are changing pole, month, or measure from last cached data.
#
# Force cache refresh:
seaice <- get_seaice(use_cache = FALSE)
# change region and month
seaice <- get_seaice(pole='S', month=9, use_cache = FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details()
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#
# Plot output using package's built-in ggplot2 settings
plot_seaice(seaice) # }

```
