##' Plot smooth terms of gam objects
#' 
#' Given a gam model this convenience functions returns a plot of all 
#' smooth terms contained in the model. If more than one smooth is present, the 
#' different smooth are faceted. 
#' 
#' @inheritParams tidy_smooth
#' @import ggplot2
#' @seealso \code{\link[mgcvtools]{tidy_smooth}}
#' @return A \code{\link[ggplot2]{ggplot2}} object.
#' @export 
gg_smooth <- function(x, ...) {

	sobj <- tidy_smooth(x, ...)

	ggsmooth <- ggplot(sobj, aes(x=x, y=fit)) + 
		geom_hline(yintercept = 0, lty=3) +
		geom_line() + 
		geom_ribbon(aes(ymin=low, ymax=high), alpha=0.2) + 
		facet_wrap(~ylab) + 
		ylab("s(x)")

	return(ggsmooth)

}