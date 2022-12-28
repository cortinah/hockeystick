.onAttach <- function(libname, pkgname) {
  options(hs_write_cache=getOption("hs_write_cache", FALSE))
  hs_logo <- "                                                                 _>>>>_-
                                                               -****<*<__-
                                                              >)****<*<___
                                                             <])****<*<__
                                                           -/)])****<*<_
                                                          >]])])****<<-
                                                        _/]]])])****_
                                                      _)]]]]])])**_
                                                    _*/]]]]]])])_
                                                  _/]]/]]]]]]*_
                                               _>*/]]]/]]]]*_
                                         --_>**)**/]]]/]/>-
                       - ---_______>>><**)><*)*)**/]](>_
-_______-_--__>__>__>>_>_>><****>>_>>_>>>>_______----"

  hs_cols <- c("#0066FF","#FFFF00","#FF6000","#FF0000","#FF0000")
  if (!isTRUE(getOption("knitr.in.progress"))) multicolor::multi_color(hs_logo, colors = hs_cols)
  packageStartupMessage(paste0("hockeystick ", utils::packageVersion("hockeystick"),": Use hockeystick_cache_details() to view cached climate data.\n", "hockeystick ",
                               utils::packageVersion("hockeystick"), ": Use hockeystick_update_all() to update and cache all climate data."))
}
