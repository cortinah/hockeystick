#' Download and plot essential climate data
#'
#' Retrieves downscaled CMIP6 climate projection data from WorldClim
#' (\url{https://www.worldclim.org/data/cmip6/}). Data are monthly or bioclimatic
#' variables projected by a selected global climate model (GCM) under an SSP
#' scenario for one of four 20-year periods (2021-2100), at 10 arc-minute resolution.
#'
#' @name get_cmip6
#' @param var (string) Climate variable. One of \code{"bioc"} (bioclimatic variables,
#'   the default), \code{"tmin"}, \code{"tmax"}, or \code{"prec"} (precipitation).
#' @param bio (numeric) If \code{var = "bioc"}, which bioclimatic variable to return
#'   (1-19). Defaults to 1 (Annual Mean Temperature). The variables are:
#'   1 = Annual Mean Temperature; 2 = Mean Diurnal Range; 3 = Isothermality (BIO2/BIO7);
#'   4 = Temperature Seasonality (standard deviation); 5 = Max Temperature of Warmest Month;
#'   6 = Min Temperature of Coldest Month; 7 = Temperature Annual Range (BIO5-BIO6);
#'   8 = Mean Temperature of Wettest Quarter; 9 = Mean Temperature of Driest Quarter;
#'   10 = Mean Temperature of Warmest Quarter; 11 = Mean Temperature of Coldest Quarter;
#'   12 = Annual Precipitation; 13 = Precipitation of Wettest Month; 14 = Precipitation of Driest Month;
#'   15 = Precipitation Seasonality (coefficient of variation); 16 = Precipitation of Wettest Quarter;
#'   17 = Precipitation of Driest Quarter; 18 = Precipitation of Warmest Quarter;
#'   19 = Precipitation of Coldest Quarter. Variables 1-11 are in degrees C
#'   (seasonality variables 4 and 7 as applicable), 12-19 in mm.
#' @param ssp (string) Shared Socioeconomic Pathway scenario. One of "126", "245",
#'   "370", or "585" (the default). SSP5-8.5 is the high-emissions upper bound and
#'   shows the strongest projected change; SSP2-4.5 is a middle-of-the-road
#'   scenario roughly consistent with current policies ("current trends continue").
#'   SSP1-2.6 assumes strong mitigation, and SSP3-7.0 is a high fragmented-world path.
#' @param period (string) Projection period. One of "2021-2040", "2041-2060",
#'   "2061-2080", or "2081-2100" (the default).
#' @param model (string) CMIP6 global climate model. Defaults to "MPI-ESM1-2-HR".
#'   Availability of variable/scenario combinations varies by model; the function
#'   checks the WorldClim index and reports what is available.
#' @param res (string) Resolution: "10m" (~18 km, default), "5m" (~9 km) or "2.5m"
#'   (~5 km). Finer resolutions are considerably larger downloads.
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE.
#'
#' @return Invisibly returns a tibble with columns \code{lon}, \code{lat}, and
#'   \code{value}: the projected values aggregated to a one-degree grid.
#'
#' @details `get_cmip6` downloads a WorldClim CMIP6 GeoTIFF, aggregates it to a
#'   one-degree grid (averaging), and returns a tibble suitable for mapping.
#'   Requires the `terra` package (installed automatically when needed).
#'
#' @examples
#' \donttest{
#' # Projected annual mean temperature under SSP585, 2081-2100:
#' proj <- get_cmip6(var='bioc', bio=1)
#' plot_cmip6(proj)
#' #
#' # Projected precipitation change under a moderate scenario:
#' prec <- get_cmip6(var='prec', ssp=245, period='2041-2060', use_cache=FALSE)
#' }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' \itemize{
#' \item WorldClim CMIP6 future climate data: \url{https://www.worldclim.org/data/cmip6/}
#' \item Fick, S.E. & Hijmans, R.J. (2017) WorldClim 2: new 1km spatial resolution climate
#'  surfaces for global land areas. \emph{Int. J. Climatol.} 37: 4302-4315.
#' }
#'
#' @importFrom tibble as_tibble
#' @importFrom utils download.file
#'
#' @export

get_cmip6 <- function(var = 'bioc', bio = 1,
                      ssp = '585', period = '2081-2100',
                      model = 'MPI-ESM1-2-HR', res = '10m',
                      use_cache = TRUE, write_cache = getOption("hs_write_cache")) {

  var <- match.arg(var, c('bioc', 'tmin', 'tmax', 'prec'))
  stopifnot(ssp %in% c('126', '245', '370', '585'))
  stopifnot(period %in% c('2021-2040', '2041-2060', '2061-2080', '2081-2100'))
  stopifnot(res %in% c('2.5m', '5m', '10m'))
  if (var == 'bioc') stopifnot(bio %in% 1:19)

  hs_path <- tools::R_user_dir("hockeystick", "cache")
  cachename <- paste0('cmip6_', var, if (var=='bioc') bio else '', '_',
                      model, '_ssp', ssp, '_', period, '.rds')

  if (use_cache && file.exists(file.path(hs_path, cachename)))
    return(invisible(readRDS(file.path(hs_path, cachename))))

  if (!requireNamespace("terra", quietly = TRUE))
    stop("get_cmip6 requires the 'terra' package. Install with install.packages('terra')")

  connected <- .isConnected('https://geodata.ucdavis.edu/cmip6/')
  if (!connected) {message("Retrieving remote data requires connectivity to source."); return(invisible(NULL))}

  fname <- paste0('wc2.1_', res, '_', var, '_', model, '_ssp', ssp, '_', period, '.tif')
  url <- paste0('https://geodata.ucdavis.edu/cmip6/', res, '/', model, '/ssp', ssp, '/', fname)

  # Verify combination exists using WorldClim's availability index
  idx <- tempfile()
  status <- tryCatch(suppressWarnings(download.file('https://geodata.ucdavis.edu/cmip6/files.txt',
                                                    idx, quiet = TRUE)), error = function(e) 1L)
  avail <- NULL
  if (identical(status, 0L)) {
    ff <- read.table(idx, sep = '_', stringsAsFactors = FALSE)
    avail <- unique(ff[[2]][ff[[1]] == var & ff[[3]] == paste0('ssp', ssp) & ff[[4]] == paste0(period, '.tif')])
  }
  if (!is.null(avail) && !(model %in% avail)) {
    message("Combination not available on WorldClim. Models with ", var,
            "/ssp", ssp, "/", period, ":")
    message(paste(sort(avail), collapse=', '))
    return(invisible(NULL))
  }

  dl <- tempfile(fileext = '.tif')
  status <- tryCatch(suppressWarnings(download.file(url, dl, mode = 'wb')), error = function(e) 1L)
  if (!identical(status, 0L)) {message("Unable to access remote resource."); return(invisible(NULL))}

  r <- terra::rast(dl)
  if (terra::nlyr(r) > 1) {
    layer_idx <- if (var == 'bioc') bio else bio # monthly vars: layers are months 1-12
    layer_name <- if (var == 'bioc') paste0('Bio', bio) else paste0('month', bio)
    r <- r[[layer_idx]]
  } else layer_name <- var

  # Aggregate 10 arc-min cells (~18km) up to ~1 degree to keep output manageable
  fact <- round(60 / as.numeric(gsub('[a-z]', '', res)))
  agg <- terra::aggregate(r, fact = max(fact, 1), fun = mean, na.rm = TRUE)
  df <- terra::as.data.frame(agg, xy = TRUE, na.rm = TRUE)
  colnames(df)[1:3] <- c('lon', 'lat', 'value')

  out <- as_tibble(df)
  attr(out, "hs_cmip6_meta") <- c(var = layer_name, var0 = var, model = model,
                                  ssp = paste0('SSP', ssp), period = period)

  if (write_cache) saveRDS(out, file.path(hs_path, cachename))
  invisible(out)
}


# Fetch and aggregate the matching WorldClim v2.1 baseline (1970-2000),
# returning a one-degree tibble identical in structure to get_cmip6 output
.get_cmip6_baseline <- function(var, bio, res = '10m') {

  if (!requireNamespace("terra", quietly = TRUE))
    stop("get_cmip6 requires the 'terra' package. Install with install.packages('terra')")

  hs_path <- tools::R_user_dir("hockeystick", "cache")
  dir.create(hs_path, showWarnings = FALSE, recursive = TRUE)

  stem <- switch(var, bioc = 'bio', var)
  zip_name <- paste0('wc2.1_', res, '_', stem, '.zip')
  zip_path <- file.path(hs_path, zip_name)

  layer_file <- if (var == 'bioc') {
    paste0('wc2.1_', res, '_bio_', bio, '.tif')
  } else {
    paste0('wc2.1_', res, '_', var, '_', sprintf('%02d', bio), '.tif')
  }
  layer_path <- file.path(hs_path, layer_file)

  if (!file.exists(layer_path)) {
    status <- tryCatch(suppressWarnings(download.file(
      paste0('https://geodata.ucdavis.edu/climate/worldclim/2_1/base/', zip_name),
      zip_path, mode = 'wb')), error = function(e) 1L)
    if (!identical(status, 0L)) {message("Unable to access remote resource."); return(NULL)}
    try(utils::unzip(zip_path, files = layer_file, exdir = hs_path), silent = TRUE)
    try(file.remove(zip_path), silent = TRUE)
    if (!file.exists(layer_path)) {message('Unexpected baseline archive contents.'); return(NULL)}
  }

  fact <- max(round(60 / as.numeric(gsub('[a-z]', '', res))), 1L)
  agg <- terra::aggregate(terra::rast(layer_path), fact = fact, fun = mean, na.rm = TRUE)
  df <- terra::as.data.frame(agg, xy = TRUE, na.rm = TRUE)
  colnames(df)[1:3] <- c('lon', 'lat', 'value')
  as_tibble(df)
}

#' Download and plot essential climate data
#'
#' Computes expected climate change as the difference between a CMIP6 projection
#' retrieved from WorldClim and its 1970-2000 reference-period baseline, on a
#' common one-degree grid.
#'
#' @name get_cmip6_anom
#' @param var,bio,ssp,period,model,res Passed through to \code{\link{get_cmip6}}.
#' @param use_cache (boolean) Return cached anomaly data if available, defaults to TRUE.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE.
#'
#' @return Invisibly returns a tibble with columns \code{lon}, \code{lat}, and
#'   \code{delta}: the projected change versus the 1970-2000 baseline for each cell.
#'
#' @details `get_cmip6_anom` downloads the projection and the corresponding
#'   WorldClim v2.1 baseline, aggregates both to a one-degree grid, and subtracts
#'   cell by cell. Differencing removes most of the model's systematic bias,
#'   leaving the expected change. Polar regions can show spurious values where
#'   the downscaled grids differ in land-sea masking.
#'
#' @examples
#' \donttest{
#' anom <- get_cmip6_anom()
#' plot_cmip6_anom(anom)
#' }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references See \code{\link{get_cmip6}}.
#'
#' @importFrom tibble as_tibble
#' @export

get_cmip6_anom <- function(var = 'bioc', bio = 1,
                           ssp = '585', period = '2081-2100',
                           model = 'MPI-ESM1-2-HR', res = '10m',
                           use_cache = TRUE, write_cache = getOption("hs_write_cache")) {

  cachename <- paste0('cmip6anom_', var, if (var == 'bioc') bio else '', '_',
                      model, '_ssp', ssp, '_', period, '.rds')
  hs_path <- tools::R_user_dir("hockeystick", "cache")

  if (use_cache && file.exists(file.path(hs_path, cachename)))
    return(invisible(readRDS(file.path(hs_path, cachename))))

  fut <- get_cmip6(var = var, bio = bio, ssp = ssp, period = period,
                   model = model, res = res, use_cache = use_cache, write_cache = TRUE)
  if (is.null(fut)) return(invisible(NULL))

  base <- .get_cmip6_baseline(var, bio, res = res)
  if (is.null(base)) return(invisible(NULL))

  m <- merge(fut, base, by = c('lon', 'lat'), suffixes = c('_fut', '_base'))
  out <- as_tibble(m[c('lon', 'lat', 'value_fut', 'value_base')])
  out$delta <- out$value_fut - out$value_base

  layer_name <- if (var == 'bioc') paste0('Bio', bio) else paste0('month', bio)
  attr(out, "hs_cmip6_meta") <- c(var = layer_name, var0 = var, model = model,
                                  ssp = paste0('SSP', ssp), period = period)

  if (write_cache) saveRDS(out, file.path(hs_path, cachename))
  invisible(out)
}


#' Download and plot essential climate data
#'
#' Plots gridded CMIP6 climate projections retrieved using `get_cmip6()` with ggplot2.
#'
#' @name plot_cmip6
#' @param dataset Name of the tibble generated by `get_cmip6`
#' @param palette (string) Color palette name passed to RColorBrewer. Defaults to
#'   "Spectral" reversed for temperature-type variables.
#' @param print (boolean) Display map, defaults to TRUE.
#'
#' @return Invisibly returns a ggplot2 object with the projection map.
#'
#' @details `plot_cmip6` returns a pre-defined ggplot2 world map of projected
#'   values. Users may further modify the output chart.
#'
#' @import ggplot2
#'
#' @export

plot_cmip6 <- function(dataset = get_cmip6(), palette = 'Spectral', print = TRUE) {

  if (is.null(dataset)) return(invisible(NULL))

  meta <- attr(dataset, "hs_cmip6_meta")

  # Human-readable variable names and units for WorldClim codes
  bio_names <- c('Annual Mean Temperature', 'Mean Diurnal Range', 'Isothermality',
                 'Temperature Seasonality', 'Max Temperature of Warmest Month',
                 'Min Temperature of Coldest Month', 'Temperature Annual Range',
                 'Mean Temperature of Wettest Quarter', 'Mean Temperature of Driest Quarter',
                 'Mean Temperature of Warmest Quarter', 'Mean Temperature of Coldest Quarter',
                 'Annual Precipitation', 'Precipitation of Wettest Month',
                 'Precipitation of Driest Month', 'Precipitation Seasonality',
                 'Precipitation of Wettest Quarter', 'Precipitation of Driest Quarter',
                 'Precipitation of Warmest Quarter', 'Precipitation of Coldest Quarter')

  v <- meta[['var']]
  if (grepl('^Bio[0-9]+$', v)) {
    bio_n <- as.integer(sub('Bio', '', v))
    var_lab <- bio_names[bio_n]
    fill_lab <- if (bio_n %in% c(3,4,15)) 'Percent' else
                if (bio_n >= 12) expression("Precipitation (mm)") else
                if (bio_n %in% c(2,7)) expression("Range (" * degree * "C)") else
                expression("Temperature (" * degree * "C)")
  } else if (grepl('^month[0-9]+$', v)) {
    month_n <- as.integer(sub('month', '', v))
    var_lab <- paste(month.name[month_n], if (meta[['var0']] == 'prec') 'Precipitation' else 'Temperature')
    fill_lab <- if (meta[['var0']] == 'prec') expression("Precipitation (mm)") else expression("Temperature (" * degree * "C)")
  } else {
    var_lab <- switch(v, prec = 'Precipitation', tmin = 'Minimum Temperature', tmax = 'Maximum Temperature', v)
    fill_lab <- if (v == 'prec') expression("Precipitation (mm)") else expression("Temperature (" * degree * "C)")
  }

  title_lab <- var_lab
  n_colors <- min(9, length(unique(dataset$value)))

  plot <- ggplot(dataset, aes(x = lon, y = lat, fill = value)) +
    geom_tile(width = 1, height = 1) +
    scale_fill_gradientn(colors = rev(RColorBrewer::brewer.pal(n_colors, palette)),
                         na.value = 'transparent', n.breaks = 8) +
    scale_x_continuous(name = NULL, breaks = seq(-180, 180, 60)) +
    scale_y_continuous(name = NULL, breaks = seq(-60, 80, 20)) +
    coord_fixed(ratio = 1, expand = FALSE) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    labs(title = title_lab,
         subtitle = 'Downscaled CMIP6 projection, WorldClim v2.1',
         fill = fill_lab,
         caption = paste0('Model: ', meta[['model']], ' | Source: WorldClim.org, CC-BY 4.0'))

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}


#' Download and plot essential climate data
#'
#' Plots expected climate change (projection minus 1970-2000 baseline) computed by
#' `get_cmip6_anom()` with a diverging color scale centered on zero.
#'
#' @name plot_cmip6_anom
#' @param dataset Name of the tibble generated by `get_cmip6_anom`
#' @param print (boolean) Display map, defaults to TRUE.
#'
#' @return Invisibly returns a ggplot2 object with the change map.
#'
#' @details `plot_cmip6_anom` returns a pre-defined ggplot2 world map of expected
#'   change in degrees C, with blue for cooling and red for warming. Users may
#'   further modify the output chart.
#'
#' @import ggplot2
#' @importFrom scales label_number
#'
#' @examples
#' \donttest{
#' plot_cmip6_anom()
#' }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references See \code{\link{get_cmip6}}.
#'
#' @export

plot_cmip6_anom <- function(dataset = get_cmip6_anom(), zero_centered = FALSE, print = TRUE) {

  if (is.null(dataset)) return(invisible(NULL))

  meta <- attr(dataset, "hs_cmip6_meta")
  var_lab <- switch(meta[['var0']],
                    prec = 'Precipitation',
                    tmin = 'Minimum Temperature',
                    tmax = 'Maximum Temperature',
                    if (grepl('^Bio[0-9]+$', meta[['var']]))
                      c('Annual Mean Temperature', 'Mean Diurnal Range', 'Isothermality',
                        'Temperature Seasonality', 'Max Temperature of Warmest Month',
                        'Min Temperature of Coldest Month', 'Temperature Annual Range',
                        'Mean Temperature of Wettest Quarter', 'Mean Temperature of Driest Quarter',
                        'Mean Temperature of Warmest Quarter', 'Mean Temperature of Coldest Quarter',
                        'Annual Precipitation', 'Precipitation of Wettest Month',
                        'Precipitation of Driest Month', 'Precipitation Seasonality',
                        'Precipitation of Wettest Quarter', 'Precipitation of Driest Quarter',
                        'Precipitation of Warmest Quarter', 'Precipitation of Coldest Quarter')[
                          as.integer(sub('Bio', '', meta[['var']]))] else meta[['var']])

  is_prec <- meta[['var0']] == 'prec' ||
    (grepl('^Bio[0-9]+$', meta[['var']]) && as.integer(sub('Bio', '', meta[['var']])) >= 12)

  # Cap the scale at the 99th percentile so extreme outlier cells
  # don't compress the color range; outliers are squashed to end colors
  hi <- unname(stats::quantile(dataset$delta, 0.99, na.rm = TRUE))
  if (zero_centered) {
    fill_scale <- scale_fill_gradient2(low = '#2166ac', mid = 'white', high = '#b2182b',
                                       midpoint = 0, limits = c(-hi, hi), n.breaks = 8,
                                       na.value = 'transparent', oob = scales::squish,
                                       labels = if (is_prec) scales::label_percent(scale = 100, accuracy = 1)
                                                else scales::label_number(accuracy = 1))
  } else {
    fill_scale <- scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, 'YlOrRd'),
                                       limits = c(0, hi), n.breaks = 8,
                                       na.value = 'transparent', oob = scales::squish,
                                       labels = if (is_prec) scales::label_percent(scale = 100, accuracy = 1)
                                                else scales::label_number(accuracy = 1))
  }

  plot <- ggplot(dataset, aes(x = lon, y = lat, fill = delta)) +
    geom_tile(width = 1, height = 1) +
    fill_scale +
    scale_x_continuous(name = NULL, breaks = seq(-180, 180, 60)) +
    scale_y_continuous(name = NULL, breaks = seq(-60, 80, 20)) +
    coord_fixed(ratio = 1, expand = FALSE) +
    theme_bw(base_size = 12) +
    theme(panel.grid = element_blank()) +
    labs(title = paste('Expected Change in', var_lab, 'by', meta[['period']]),
         subtitle = expression("Versus 1970-2000 baseline, WorldClim CMIP6 downscaled"),
         fill = if (is_prec) expression(Delta * " (%)") else expression(Delta * "T (" * degree * "C)"),
         caption = paste0(meta[['model']], ' ', meta[['ssp']],
                          ' | Source: WorldClim.org, CC-BY 4.0'))

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}
