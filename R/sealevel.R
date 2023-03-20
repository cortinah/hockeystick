#' Download and plot essential climate data
#'
#' Retrieves global mean sea level (GMSL) data from historic tide gauge and recent satellite altimeter observations (in mm).
#' Source for tide gauge data is Commonwealth Scientific and Industrial Research Organisation (CSIRO) as described in Church and White (2011).
#' \url{https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/} \cr \cr
#' Source for satellite data is NOAA Laboratory for Satellite Altimetry:
#' \url{https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/}
#'
#' @name get_sealevel
#' @param use_cache (boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.
#' @param write_cache (boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)
#'
#' @return Invisibly returns a tibble with the global mean sea level time series (in mm) over 1880-2013 using tide gauges and since 1993 for satellite measurements.
#'
#' `get_sealevel` invisibly returns a tibble with mean sea level in mm time series from tide gauges and satellite observations.
#'
#' The satellite observations have been releveled so that their mean level in 1993 matches that of the tide gauges.
#'
#' The tide gauge data are no longer updated and cover the period from 1880 to 2013, per Church, J. A. and N.J. White (2011) \url{https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/}
#'
#' Satellite data are updated monthly or more frequently from the NOAA Laboratory for Satellite Altimetry. TOPEX and Jason-1,-2,-3 satellites dataset, with seasonal signals removed. \url{https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/}
#'
#' @importFrom readr read_csv
#' @importFrom lubridate ymd date_decimal round_date
#' @importFrom utils download.file read.csv unzip
#' @importFrom dplyr full_join summarize_all filter select
#' @importFrom tidyr pivot_longer
#'
#' @examples
#' \donttest{
#' # Fetch sea level from cache if available:
#' gmsl <- get_sealevel()
#' #
#' # Force cache refresh:
#' gmsl <- get_sealevel(use_cache=FALSE)
#' #
#' # Review cache contents and last update dates:
#' hockeystick_cache_details()
#' #
#' # Plot output using package's built-in ggplot2 settings
#' plot_sealevel(gmsl) }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#' @references
#' \itemize{
#' \item CSIRO reconstructed tide gauge GMSL for 1880 to 2009: \url{https://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/}
#' \item Church, J. A. and N.J. White (2011), Sea-level rise from the late 19th to the early 21st Century. \emph{Surveys in Geophysics}, doi:10.1007/s10712-011-9119-1. \url{https://link.springer.com/article/10.1007/s10712-011-9119-1}
#' \item NOAA Laboratory for Satellite Altimetry \url{https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/}
#'  }
#'
#' @export

get_sealevel <- function(use_cache = TRUE, write_cache = getOption("hs_write_cache")) {

  hs_path <- tools::R_user_dir("hockeystick","cache")

  if (use_cache) {
    if (file.exists(file.path(hs_path,'sealevel.rds'))) return(invisible(readRDS((file.path(hs_path,'sealevel.rds')))))
  }

file_url <- 'https://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise/slr/slr_sla_gbl_free_txj1j2_90.csv'
connected <- .isConnected(file_url)
if (!connected) {message("Retrieving remote data requires internet connectivity."); return(invisible(NULL))}

dl <- tempfile()
download.file(file_url, dl)

gmsl_sat <- utils::read.csv(dl, header = FALSE, skip = 6)
gmsl_sat$gmsl_sat <- rowMeans(gmsl_sat[,2:5], na.rm = TRUE)
colnames(gmsl_sat)[1] <- 'date'
gmsl_sat <- gmsl_sat[,c('date', 'gmsl_sat')]
gmsl_sat$date <- lubridate::ymd(lubridate::round_date(lubridate::date_decimal(gmsl_sat$date), 'day'))


file_url <- 'https://research.csiro.au/slrwavescoast/?ddownload=327'
td <- tempdir()
dl <- tempfile(tmpdir=td)
download.file(file_url, dl, mode='wb')

unzip(dl, 'church_white_gmsl_2011_up/CSIRO_Recons_gmsl_mo_2015.csv', exdir = td, overwrite = TRUE)
gmsl_tide <- readr::read_csv(file.path(td,'church_white_gmsl_2011_up/CSIRO_Recons_gmsl_mo_2015.csv'), col_types = readr::cols(`GMSL uncertainty (mm)` = readr::col_skip()))
colnames(gmsl_tide) <- c('date', 'gmsl_tide')
gmsl_tide$date <- lubridate::ymd(lubridate::round_date(lubridate::date_decimal(gmsl_tide$date),'day'))

gmsl <- suppressMessages( dplyr::full_join(gmsl_tide, gmsl_sat) )

diff <- dplyr::filter(gmsl, date >= as.Date('1993-01-01') & date < as.Date('1994-01-01'))
diff <- dplyr::summarize_all(diff, list(mean=mean), na.rm = TRUE)
diff <- diff$gmsl_tide_mean - diff$gmsl_sat_mean
gmsl$gmsl_sat <- gmsl$gmsl_sat + diff

gmsl <- tidyr::pivot_longer(gmsl, -date, values_drop_na = TRUE, names_to = 'method', values_to = 'gmsl')

if (write_cache) saveRDS(gmsl, file.path(hs_path, 'sealevel.rds'))

invisible(gmsl) }


#' Download and plot essential climate data
#'
#' Plots the global mean sea level data retrieved using `get_sealevel()` with ggplot2. The output ggplot2 object may be further modified.
#'
#'
#' @name plot_sealevel
#' @param dataset Name of the tibble generated by `get_sealevel`, defaults to calling `get_sealevel`
#' @param print (boolean) Display sealevel ggplot2 chart, defaults to TRUE. Use FALSE to not display chart.
#'
#' @return Invisibly returns a ggplot2 object with sealevel chart
#'
#' @details `plot_sealevel` invisibly returns a ggplot2 object with a pre-defined sealevel change chart using data from `get_sealevel`.
#' By default the chart is also displayed. Users may further modify the output ggplot2 chart.
#'
#' @import ggplot2
#' @importFrom lubridate ymd ceiling_date
#'
#' @examples
#' \donttest{
#' # Fetch sealevel data:
#' gmsl <- get_sealevel()
#' #
#' # Plot output using package's built-in ggplot2 defaults
#' plot_sealevel(gmsl)
#'
#' # Or just call plot_sealevel(), which defaults to get_sealevel() dataset
#' plot_sealevel()
#'
#' p <- plot_sealevel(gmsl, print = FALSE)
#' # Modify plot such as: p + ggplot2::labs(title='Rising Waters') }
#'
#' @author Hernando Cortina, \email{hch@@alum.mit.edu}
#'
#' @export

plot_sealevel <- function(dataset = get_sealevel(), print=TRUE) {

if (is.null(dataset)) return(invisible(NULL))

plot <-  ggplot(dataset, aes(x=date, color=method, y=gmsl)) +geom_line(size=1) + theme_bw(base_size = 12) +
         scale_x_date(name=NULL, breaks='15 years', limits = c(ymd('1878-01-01'), ceiling_date(max(dataset$date), 'years')), date_labels ='%Y') +
         scale_color_manual(values=c('dodgerblue2','firebrick1'), labels=c('Satellite observations','Coastal tide gauge records')) +theme(legend.position = c(0.30,0.84), legend.background=element_blank(), legend.title = element_blank()) +
         scale_y_continuous(n.breaks = 8) +
         labs(title='Sea Level Rise', subtitle='Tide gauges: 1880-2013; Satellite: 1992-present, calibrated to 1993 mean tide gauge.', y= 'Variation (mm)',
         caption='Sources: NOAA Laboratory for Satellite Altimetry (sat)\nhttps://www.star.nesdis.noaa.gov/socd/lsa/SeaLevelRise\nCommonwealth Scientific and Industrial Research Organisation (tide gauge)\nhttps://research.csiro.au/slrwavescoast/sea-level/measurements-and-data/sea-level-data/')

  if (print) suppressMessages( print(plot) )
  invisible(plot)
}
