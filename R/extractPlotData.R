

#' Extract plot information of all smooths
#' 
#' Given a \code{mgcv} \code{\link[mgcv]{gamObject}}, returns the information 
#' used for the default plots produced by \code{\link[mgcv]{plot.gam}}.
#' 
#' @inheritParams mgcv::plot.gam
#' @param ... Further arguments passed to \code{\link[mgcv]{plot.gam}}
#' @import mgcv
#' @importFrom checkmate assert_class
#' @export 
get_plotinfo <- function(x, ...) {

	assert_class(x, c("gam", "glm", "lm"))

	tmp <- paste0(tempfile(), ".png")
	png(tmp)
	po <- plot(mod, page=1)
	dev.off()
	file.remove(tmp)

	class(po) <- c("mgcv.plotlist", class(po))

	return(po)

}


#' Extract 1d smooth objects in tidy data format.
#' 
#' @param po A list of plot obects as returned from \code{\link[mgcvtools]{get_plotinfo}}.
#' @param keep A vector of variables to keep. 
#' @importFrom dplyr bind_rows
#' @export 
tidy_s <- function(
	po, 
	keep = c("x", "fit", "se", "xlab", "ylab"), 
	se = TRUE) {

	# index of list elements that are 1d smooths and not random effects 
	ind.1d <- vapply(
		X         = po,
		FUN       = function(z) !is.null(z$x) & is.null(z$main),
		FUN.VALUE = logical(1))
	# keep only variables of interes
	po <- lapply(po[ind.1d], "[", i=keep, drop=TRUE)
	# use cbind.data.frame here, b/c as_data_frame does not work here 
	po <- lapply(po, function(z) do.call(cbind.data.frame, c(z, stringsAsFactors=FALSE)))
	if(se) {
		po <- lapply(po, function(z) {
			z$low  = z$fit - z$se
			z$high = z$fit + z$se
			z
		})
	}

	return(bind_rows(po))

}


#' Extract random effects objects in tidy data format.
#' 
#' @inheritParams tidy_s
#' @importFrom dplyr bind_rows
#' @rdname tidy_s
#' @export 
tidy_re <- function(po, keep=c("fit", "main", "xlab", "ylab")) {

	ind.re <- vapply(
		X         = po,
		FUN       = function(z) !is.null(z$main) & z$xlab == "Gaussian quantiles",
		FUN.VALUE = logical(1))

	po <- lapply(po[ind.re], "[", i=keep, drop=TRUE)
	po <- lapply(po, function(z) {
		re.df = do.call(cbind.data.frame, c(z, stringsAsFactors=FALSE))
		re.df$x = qnorm(ppoints(length(re.df$fit))[order(order(re.df$fit))])
		yl <- quantile(re.df$fit, probs=c(0.1, 0.9), type=7, names=FALSE)
		xl <- qnorm(c(0.1, 0.9))
		re.df$qqslope <- diff(yl)/diff(xl)
		re.df$qqintercept <- yl[1L] - re.df$qqslope*xl[1L]

		re.df

	})

	return(bind_rows(po))

}
