#' Manage cached datasets
#'
#' @name hockeystick_cache
#' @param files (character) one or more complete file names
#' @param force (logical) Should files be force deleted? Default: `TRUE`
#'
#' @details `cache_delete` only accepts 1 file name, while `cache_delete_all`
#' doesn't accept any names, but deletes all files. For deleting many
#' specific files, use `cache_delete` in a [lapply()] type call
#'
#' @details We cache using [tools::R_user_dir()], find your cache
#' folder by executing `tools::R_user_dir("hockeystick","cache")`
#'
#' @section Functions:
#' \itemize{
#'  \item `hockeystick_cache_list()` returns a character vector of full path
#'  file names in cache
#'  \item `hockeystick_cache_delete()` deletes one or more files, returns nothing
#'  \item `hockeystick_cache_delete_all()` delete all files, returns nothing
#'  \item `hockeystick_cache_details()` prints file name and file size of each file, supply with one or more files, or no files (and get details for all available)
#'  \item `hockeystick_update_all()` updates the cache with the latest co2, temperature, sea level, and sea ice data.
#' }
#'
#' @examples \donttest{
#' # list files in cache
#' hockeystick_cache_list()
#'
#' # List info for single files
#' hockeystick_cache_details(files = hockeystick_cache_list()[1])
#' hockeystick_cache_details(files = hockeystick_cache_list()[2])
#'
#' # List info for all files
#' hockeystick_cache_details()
#'
#' # Delete cached files by name
#' hockeystick_cache_delete(files = hockeystick_cache_list()[1])
#'
#' # Update all hockeystick data and place in cache
#' hockeystick_update_all()
#'
#' # Delete all cached data
#' hockeystick_cache_delete_all()
#' }
#' @references
#' Caching data sets: ROpenSci guide to persistent config and data for R packages: https://blog.r-hub.io/2020/03/12/user-preferences/

#' @export
#' @return `hockeystick_cache_list()` returns a character vector of full path filenames in cache.
#' @rdname hockeystick_cache
hockeystick_cache_list <- function() {
  list.files(hscache_path(), pattern = ".rds", ignore.case = TRUE,
             recursive = TRUE, full.names = TRUE)
}

#' @export
#' @return `hockeystick_cache_delete()` no return value, called for side effect.
#' @rdname hockeystick_cache
hockeystick_cache_delete <- function(files, force = TRUE) {
  if (!all(file.exists(files))) {
    message("These files don't exist or can't be found: \n",
         strwrap(files[!file.exists(files)], indent = 5), call. = FALSE)
  }
  invisible(file.remove(files))
}

#' @export
#' @return `hockeystick_cache_delete_all()` no return value, called for side effect.
#' @rdname hockeystick_cache
hockeystick_cache_delete_all <- function(force = TRUE) {
  files <- list.files(hscache_path(), pattern = ".rds", ignore.case = TRUE,
                      full.names = TRUE, recursive = TRUE)
  invisible(file.remove(files))
}

#' @export
#' @return `hockeystick_cache_details()` returns list of filenames and sizes of cached files.
#' @rdname hockeystick_cache
hockeystick_cache_details <- function(files = NULL) {
  if (is.null(files)) {
    files <- list.files(hscache_path(), pattern = ".rds", ignore.case = TRUE,
                        full.names = TRUE, recursive = TRUE)
    structure(lapply(files, file_info_), class = "hockeystick_cache_info")
  } else {
    structure(lapply(files, file_info_), class = "hockeystick_cache_info")
  }
}

#' Display cached file info
#'
#' Internal function
#' @param x filenames
file_info_ <- function(x) {
  fs <- file.size(x)
  list(file = x,
       type = "rds",
       size = if (!is.na(fs)) getsize(fs) else NA,
       date = if (!is.na(fs)) file.mtime(x) else NA
  )
}

#' Get rounded size of file in kB
#'
#' Internal function
#' @param x filenames
getsize <- function(x) {
  round(x/10^3, 1)
}

#' Display data cache info
#' Shows filenames and cache file sizes
#' @param x filenames
#' @param ... Additional parameters
#' @return Display data cache info. No return value, called for side effect.
#' @export
#' @method print hockeystick_cache_info
print.hockeystick_cache_info <- function(x, ...) {
  cat("<hockeystick cached files>", sep = "\n")
  cat(sprintf("  directory: %s\n", hscache_path()), sep = "\n")
  for (i in seq_along(x)) {
    cat(paste0("  file: ", sub(hscache_path(), "", x[[i]]$file)), sep = "\n")
    cat(paste0("  size: ", x[[i]]$size, " kB"), sep = "\n")
    cat(paste0("  date: ", x[[i]]$date), sep='\n')
    cat("\n")
  }
}

#' Return path of data cache directory
#'
#' Internal Function
#' @return Return path of data cache directory
hscache_path <- function() tools::R_user_dir("hockeystick","cache")


#' @export
#' @return `hockeystick_update_all()` updates all datasets and caches them. No return value, called for side effect.
#' @rdname hockeystick_cache
hockeystick_update_all <- function() {
  get_carbon(use_cache = FALSE, write_cache = TRUE)
  get_temp(use_cache = FALSE, write_cache = TRUE)
  get_seaice(use_cache = FALSE, write_cache = TRUE)
  get_sealevel(use_cache = FALSE, write_cache = TRUE)
  get_icecurves(use_cache = FALSE, write_cache = TRUE)
  get_temp2k(use_cache = FALSE, write_cache = TRUE)
  get_hurricanes(use_cache = FALSE, write_cache = TRUE)
  get_emissions(use_cache = FALSE, write_cache = TRUE)
  get_methane(use_cache = FALSE, write_cache = TRUE)
  get_dailytemp(use_cache = FALSE, write_cache = TRUE)
  invisible(0)
}
