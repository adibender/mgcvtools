#' Extract information of the sample contained in a data set
#'
#' Given a data set and grouping variables, this function returns mean values
#' for numeric variables and modus for characters and factors. Usually
#' this function should not be called directly but will rather be called
#' as part of a call to \code{make_newdata}.
#'
#' @rdname sample_info
#' @param x A data frame (or object that inherits from \code{data.frame}).
#' @importFrom stats median
#' @return A data frame containing sample information (for each group).
#' If applied to an object of class \code{ped}, the sample means of the
#' original data is returned.
#' Note: When applied to a \code{ped} object, that doesn't contain covariates
#' (only interval information), returns data frame with 0 columns.
#'
#' @export
#' @keywords internal
sample_info <- function(x) {
  UseMethod("sample_info", x)
}

#' @inheritParams sample_info
#' @import checkmate dplyr
#' @importFrom purrr compose
#' @export
#' @rdname sample_info
sample_info.data.frame <- function(x) {

  cn  <- colnames(x)
  num <- summarize_if (x, .predicate = is.numeric, ~mean(., na.rm = TRUE))
  fac <- summarize_if (x, .predicate = compose("!", is.numeric), modus)

  nnames <- intersect(names(num), names(fac))

  if (length(nnames) != 0) {
    suppressMessages(
      x <- left_join(num, fac) %>% grouped_df(vars = lapply(nnames, as.name))
    )
  } else {
    x <- bind_cols(num, fac)
  }

  return(select(x, one_of(cn)))

}

#' Create a data frame from all combinations of data frames
#'
#' Works like \code{\link[base]{expand.grid}} but for data frames.
#'
#' @importFrom dplyr slice bind_cols combine
#' @importFrom purrr map map_lgl map2 transpose cross
#' @importFrom checkmate test_data_frame
#' @param ... Data frames that should be combined to one data frame.
#' Elements of first df vary fastest, elements of last df vary slowest.
#' @examples
#' combine_df(
#'   data.frame(x=1:3, y=3:1),
#'   data.frame(x1=c("a", "b"), x2=c("c", "d")),
#'   data.frame(z=c(0, 1)))
#' @export
#' @keywords internal
combine_df <- function(...) {

  dots <- list(...)
  if (!all(sapply(dots, test_data_frame))) {
    stop("All elements in ... must inherit from data.frame!")
  }
  ind_seq   <- map(dots, ~ seq_len(nrow(.x)))
  not_empty <- map_lgl(ind_seq, ~ length(.x) > 0)
  ind_list  <- ind_seq[not_empty] %>% cross() %>% transpose() %>% map(combine)

  map2(dots[not_empty], ind_list, function(.x, .y) slice(.x, .y)) %>%
    bind_cols()

}

#' Construct a data frame suitable for prediction
#'
#' Given a data set, returns a data set that can be used
#' as \code{newdata} argument in a call to \code{predict} and similar functions.
#'
#' @rdname newdata
#' @aliases make_newdata
#' @inheritParams sample_info
#' @param ... Covariate specifications (expressions) that will be evaluated
#' by looking for variables in \code{x} (or \code{data}). Must be of the form \code{z = f(z)}
#' where \code{z} is a variable in the data set \code{x} and \code{f} a known
#' function that can be usefully applied to \code{z}. See examples below.
#' @import dplyr
#' @importFrom checkmate assert_data_frame assert_character
#' @importFrom purrr map cross_df
#' @details Depending on the class of \code{x}, mean or modus values will be
#' used for variables not specified in ellipsis. If x is an object that inherits
#' from class \code{ped}, useful data set completion will be attempted depending
#' on variables specified in ellipsis.
#' @examples
#' library(dplyr)
#' iris %>% make_newdata()
#' iris %>% make_newdata(Sepal.Length=c(5))
#' iris %>% make_newdata(Sepal.Length=seq_range(Sepal.Length, 3),
#'  Sepal.Width=c(2, 3))
#' iris %>% make_newdata(Sepal.Length=seq_range(Sepal.Length, 3),
#'  Species=unique(Species), Sepal.Width=c(2, 3))
#' # mean/modus values of unspecified variables are calculated over whole data
#' iris %>% make_newdata(Species=unique(Species))
#' iris %>% group_by(Species) %>% make_newdata()
#' # You can also pass a part of the data sets as data frame to make_newdata
#' purrr::cross_df(list(Sepal.Length = c(0, 500, 1000),
#'  Species = c("setosa", "versicolor"))) %>%
#'   make_newdata(x=iris)
#' @export
make_newdata <- function(x, ...) {
  UseMethod("make_newdata", x)
}


#' @inherit make_newdata
#' @importFrom purrr map_lgl is_atomic
#' @importFrom lazyeval f_eval
#' @rdname newdata
#' @export
make_newdata.default <- function(x, ...) {

  assert_data_frame(x, all.missing = FALSE, min.rows = 2, min.cols = 1)

  orig_names <- names(x)

  expressions    <- quos(...)
  expr_evaluated <- map(expressions, f_eval, data = x)

  # construct data parts depending on input type
  lgl_atomic <- map_lgl(expr_evaluated, is_atomic)
  part1      <- expr_evaluated[lgl_atomic] %>% cross_df()
  part2      <- do.call(combine_df, expr_evaluated[!lgl_atomic])

  ndf    <- combine_df(part1, part2)
  rest   <- x %>% select(-one_of(c(colnames(ndf))))
  if (ncol(rest) > 0) {
    si  <- sample_info(rest) %>% ungroup()
    ndf <- combine_df(si, ndf)

  }

  ndf %>% select(one_of(orig_names))

}
