% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make-newdata.R
\name{sample_info}
\alias{sample_info}
\alias{sample_info.data.frame}
\title{Extract information of the sample contained in a data set}
\usage{
sample_info(x)

\method{sample_info}{data.frame}(x)
}
\arguments{
\item{x}{A data frame (or object that inherits from \code{data.frame}).}
}
\value{
A data frame containing sample information (for each group).
If applied to an object of class \code{ped}, the sample means of the
original data is returned.
Note: When applied to a \code{ped} object, that doesn't contain covariates
(only interval information), returns data frame with 0 columns.
}
\description{
Given a data set and grouping variables, this function returns mean values
for numeric variables and modus for characters and factors. Usually
this function should not be called directly but will rather be called
as part of a call to \code{make_newdata}.
}
\keyword{internal}
