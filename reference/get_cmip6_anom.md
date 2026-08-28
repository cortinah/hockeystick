# Download and plot essential climate data

Computes expected climate change as the difference between a CMIP6
projection retrieved from WorldClim and its 1970-2000 reference-period
baseline, on a common one-degree grid.

## Usage

``` r
get_cmip6_anom(
  var = "bioc",
  bio = 1,
  ssp = "585",
  period = "2081-2100",
  model = "MPI-ESM1-2-HR",
  res = "10m",
  use_cache = TRUE,
  write_cache = getOption("hs_write_cache")
)
```

## Arguments

- var, bio, ssp, period, model, res:

  Passed through to [`get_cmip6`](get_cmip6.md).

- use_cache:

  (boolean) Return cached anomaly data if available, defaults to TRUE.

- write_cache:

  (boolean) Write data to cache, defaults to FALSE.

## Value

Invisibly returns a tibble with columns `lon`, `lat`, and `delta`: the
projected change versus the 1970-2000 baseline for each cell. For
temperature variables `delta` is the absolute change in degrees C; for
precipitation variables (var "prec", or bioc 12-19) it is the percent
change, capped at +/-100\\ baseline is near zero.

## Details

`get_cmip6_anom` downloads the projection and the corresponding
WorldClim v2.1 baseline, aggregates both to a one-degree grid, and
subtracts cell by cell. Differencing removes most of the model's
systematic bias, leaving the expected change. Polar regions can show
spurious values where the downscaled grids differ in land-sea masking.

## References

See [`get_cmip6`](get_cmip6.md).

## Author

Hernando Cortina, <hch@alum.mit.edu>

## Examples

``` r
# \donttest{
anom <- get_cmip6_anom()
plot_cmip6_anom(anom)

# }
```
