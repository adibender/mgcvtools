% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-utils.R
\name{gg_re}
\alias{gg_re}
\title{Plot Normal QQ plots for random effects}
\usage{
gg_re(x, ...)
}
\arguments{
\item{x}{ a fitted \code{gam} object as produced by \code{gam()}.}

\item{...}{Further arguments passed to \code{\link[mgcv]{plot.gam}}}
}
\description{
Plot Normal QQ plots for random effects
}
\examples{
mod <- mgcv::gam(Sepal.Length ~ s(Species, bs = "re"), data = iris)
gg_re(mod)
}
\seealso{
\code{\link{tidy_re}}
}
