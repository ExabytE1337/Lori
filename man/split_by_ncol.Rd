% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_by_ncol.R
\name{split_by_ncol}
\alias{split_by_ncol}
\title{Function to split a dataframe into a list}
\usage{
split_by_ncol(x, n = 25)
}
\arguments{
\item{x}{should be a dataframe. Right now I'm not checking it, but will be fixed in the future.}

\item{n}{maximum number of columns in the smaller dataframes.}
}
\description{
This function splits a dataframe into a list of dataframes with at most
n columns. Especially useful when working with large dataframes.
}
