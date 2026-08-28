# Manage cached datasets

Manage cached datasets

## Usage

``` r
hockeystick_cache_list()

hockeystick_cache_delete(files, force = TRUE)

hockeystick_cache_delete_all(force = TRUE)

hockeystick_cache_details(files = NULL)

hockeystick_update_all()
```

## Arguments

- files:

  (character) one or more complete file names

- force:

  (logical) Should files be force deleted? Default: `TRUE`

## Value

`hockeystick_cache_list()` returns a character vector of full path
filenames in cache.

`hockeystick_cache_delete()` no return value, called for side effect.

`hockeystick_cache_delete_all()` no return value, called for side
effect.

`hockeystick_cache_details()` returns list of filenames and sizes of
cached files.

`hockeystick_update_all()` updates all datasets and caches them. No
return value, called for side effect.

## Details

`cache_delete` only accepts 1 file name, while `cache_delete_all`
doesn't accept any names, but deletes all files. For deleting many
specific files, use `cache_delete` in a
[`lapply()`](https://rdrr.io/r/base/lapply.html) type call

We cache using
[`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html), find your
cache folder by executing `tools::R_user_dir("hockeystick","cache")`

## Functions

- `hockeystick_cache_list()` returns a character vector of full path
  file names in cache

- `hockeystick_cache_delete()` deletes one or more files, returns
  nothing

- `hockeystick_cache_delete_all()` delete all files, returns nothing

- `hockeystick_cache_details()` prints file name and file size of each
  file, supply with one or more files, or no files (and get details for
  all available)

- `hockeystick_update_all()` updates the cache with the latest co2,
  temperature, sea level, and sea ice data.

## References

Caching data sets: ROpenSci guide to persistent config and data for R
packages: https://blog.r-hub.io/2020/03/12/user-preferences/

## Examples

``` r
# \donttest{
# list files in cache
hockeystick_cache_list()
#> [1] "/home/runner/.cache/R/hockeystick/cmip6_bioc1_MPI-ESM1-2-HR_ssp585_2081-2100.rds"

# List info for single files
hockeystick_cache_details(files = hockeystick_cache_list()[1])
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#>   file: /cmip6_bioc1_MPI-ESM1-2-HR_ssp585_2081-2100.rds
#>   size: 105.8 kB
#>   date: 2026-08-28 11:57:51.051818
#> 
hockeystick_cache_details(files = hockeystick_cache_list()[2])
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#>   file: NA
#>   size: NA kB
#>   date: NA
#> 

# List info for all files
hockeystick_cache_details()
#> <hockeystick cached files>
#>   directory: /home/runner/.cache/R/hockeystick
#> 
#>   file: /cmip6_bioc1_MPI-ESM1-2-HR_ssp585_2081-2100.rds
#>   size: 105.8 kB
#>   date: 2026-08-28 11:57:51.051818
#> 

# Delete cached files by name
hockeystick_cache_delete(files = hockeystick_cache_list()[1])

# Update all hockeystick data and place in cache
hockeystick_update_all()
#> Retrieving remote data requires connectivity to source.
#> Retrieving remote data requires connectivity to source.
#> Unable to access remote resource.
#> Retrieving remote data requires connectivity to source.

# Delete all cached data
hockeystick_cache_delete_all()
# }
```
