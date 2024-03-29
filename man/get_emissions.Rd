% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emissions.R
\name{get_emissions}
\alias{get_emissions}
\title{Download and plot essential climate data}
\usage{
get_emissions(use_cache = TRUE, write_cache = getOption("hs_write_cache"))
}
\arguments{
\item{use_cache}{(boolean) Return cached data if available, defaults to TRUE. Use FALSE to fetch updated data.}

\item{write_cache}{(boolean) Write data to cache, defaults to FALSE. Use TRUE to write data to cache for later use. Can also be set using options(hs_write_cache=TRUE)}
}
\value{
Invisibly returns a tibble with annual carbon dioxide emissions
}
\description{
Retrieves Global Carbon Project (GCP) annual global carbon dioxide emissions since 1750 from Our World In Data repository
\url{https://github.com/owid/co2-data}
}
\details{
\code{get_emissions} invisibly returns a tibble with GCP's annual carbon dioxide emissions from fossil fuels in aggregate and for every nation.
The returned object includes ISO code, country, year, co2 emissions, growth rates, per capita, and decompositions by industry and gas type.
Please refer to above website for details.
}
\examples{
\donttest{
# Fetch from cache if available:
emissions <- get_emissions()
#
# Force cache refresh:
emissions <- get_emissions(use_cache=FALSE)
#
# Review cache contents and last update dates:
hockeystick_cache_details()
#
# Plot output using package's built-in ggplot2 settings
plot_emissions(emissions) }

}
\references{
\url{https://www.globalcarbonproject.org/carbonbudget/}

Friedlingstein, P. et al (2020), Global Carbon Budget 2020, \emph{Earth System Science Data}, vol. 12, 3269-3340 \doi{10.5194/essd-12-3269-2020}
}
\author{
Hernando Cortina, \email{hch@alum.mit.edu}
}
