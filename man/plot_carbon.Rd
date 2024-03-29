% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/carbon.R
\name{plot_carbon}
\alias{plot_carbon}
\title{Download and plot essential climate data}
\usage{
plot_carbon(dataset = get_carbon(), print = TRUE, annot = TRUE)
}
\arguments{
\item{dataset}{Name of the tibble generated by \code{get_carbon}}

\item{print}{(boolean) Display carbon dioxide ggplot2 chart, defaults to TRUE. Use FALSE to not display chart.}

\item{annot}{(boolean) Include chart annotation with latest date and value, defaults to TRUE.}
}
\value{
Invisibly returns a ggplot2 object with carbon dioxide chart
}
\description{
Plots carbon dioxide data retrieved using \code{get_carbon()} with ggplot2. The output ggplot2 object may be modified.
}
\details{
\code{plot_carbon} invisibly returns a ggplot2 object with a pre-defined carbon dioxide chart using data from \code{get_carbon}.
By default the chart is also displayed. Users may further modify the output ggplot2 chart.
}
\examples{
\donttest{
# Fetch carbon dioxide data:
maunaloa <- get_carbon()
#
# Plot output using package's built-in ggplot2 defaults
plot_carbon(maunaloa)

# Or just call plot_carbon(), which defaults to get_carbon() dataset
plot_carbon()

p <- plot_carbon(maunaloa, print = FALSE)
# Modify plot such as: p + ggplot2::labs(title='The Keeling Curve') }

}
\author{
Hernando Cortina, \email{hch@alum.mit.edu}
}
