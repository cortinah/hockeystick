# Download and plot essential climate data

Retrieves global mean sea level (GMSL) data from historic tide gauge and
recent satellite altimeter observations (in mm). Source for tide gauge
data is Commonwealth Scientific and Industrial Research Organisation
(CSIRO) as described in Church and White (2011).
<https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/>  
  
Source for satellite data is NOAA Laboratory for Satellite Altimetry:
<https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/>

## Usage

``` r
get_sealevel(use_cache = TRUE, write_cache = getOption("hs_write_cache"))
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

Invisibly returns a tibble with the global mean sea level time series
(in mm) over 1880-2013 using tide gauges and since 1993 for satellite
measurements.

`get_sealevel` invisibly returns a tibble with mean sea level in mm time
series from tide gauges and satellite observations.

The satellite observations have been releveled so that their mean level
in 1993 matches that of the tide gauges.

The tide gauge data are no longer updated and cover the period from 1880
to 2013, per Church, J. A. and N.J. White (2011)
<https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/>

Satellite data are updated monthly or more frequently from the NOAA
Laboratory for Satellite Altimetry. TOPEX and Jason-1,-2,-3 satellites
dataset, with seasonal signals removed.
<https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/>

## References

- CSIRO reconstructed tide gauge GMSL for 1880 to 2009:
  <https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/>

- Church, J. A. and N.J. White (2011), Sea-level rise from the late 19th
  to the early 21st Century. *Surveys in Geophysics*,
  doi:10.1007/s10712-011-9119-1.
  <https://link.springer.com/article/10.1007/s10712-011-9119-1>

- NOAA Laboratory for Satellite Altimetry
  <https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/>

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
# Fetch sea level from cache if available:
gmsl <- get_sealevel()
#
# Force cache refresh:
gmsl <- get_sealevel(use_cache=FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details()
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#
# Plot output using package's built-in ggplot2 settings
plot_sealevel(gmsl) # }

```
