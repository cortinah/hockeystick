.onAttach <- function(libname, pkgname) {
  options(hs_write_cache=getOption("hs_write_cache", FALSE))
  packageStartupMessage("hockeystick: Use hockeystick_cache_details() to view cached climate data.\nhockeystick: Use hockeystick_update_all() to update and cache all climate data.")
 }
