% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_colours.R
\name{plot_colours}
\alias{plot_colours}
\title{Plot a vector of hex colours.}
\usage{
plot_colours(colour_vector, names = T, background_colour = "white")
}
\arguments{
\item{colour_vector}{vector of colors to plot.}
}
\description{
This function is from some other package, I don't know where it's from. But it's super helpful.
}
\examples{
m <- lori_pal()
plot_colours(m)

##
plot_colours(lori_pal(2),F)
}
