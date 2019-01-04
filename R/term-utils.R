#' Add the contribution of a term to the linear predictor to data set
#'
#' Adds the contribution of a specific term to the
#' linear predictor to the data specified by \code{newdata}.
#' Essentially a wrapper to \code{\link[mgcv]{predict.gam}}, with \code{type="terms"}.
#' Thus most arguments and their documentation below is from \code{\link[mgcv]{predict.gam}}.
#'
#'
#' @inheritParams mgcv::predict.gam
#' @param term A character (vector) or regular expression indicating for
#' which term(s) information should be extracted and added to data set.
#' @param se_mult The factor by which standard errors are multiplied to form
#' confidence intervals.
#' @param reference A list of parameter specifications. The term contribution
#' will than be calculated realtive to the specified values.
#' @param ... Further arguments passed to \code{\link[mgcv]{predict.gam}}
#' @import checkmate dplyr mgcv
#' @importFrom stats predict
#' @importFrom purrr map
#' @examples
#' mod <- mgcv::gam(Sepal.Length ~ s(Sepal.Width)+Petal.Width, data = iris)
#' iris %>% make_newdata(Sepal.Width = seq_range(Sepal.Width, 10)) %>%
#'   add_term(mod, term="Sepal.Width")
#' @export
#' @importFrom stats model.matrix vcov
add_term <- function(
  newdata,
  object,
  term,
  se.fit    = TRUE,
  type      = "terms",
  se_mult   = 2,
  reference = NULL,
  ...) {

  assert_data_frame(newdata, all.missing = FALSE)
  assert_character(term, min.chars = 1, any.missing = FALSE, min.len = 1)

  col_ind <- map(term, grep, x = names(object$coefficients)) %>%
    unlist() %>% unique() %>% sort()
  is_gam <- inherits(object, "gam")

  X <- if (is_gam) {
    predict.gam(object, newdata = newdata,
      type = "lpmatrix", ...)[, col_ind, drop = FALSE]
  } else  {
    model.matrix(object$formula[-2], data = newdata)[, col_ind, drop = FALSE]
  }
  if (!is.null(reference)) {
    reference <- newdata %>% mutate(!!!reference)
    X_ref <- if (is_gam) {
      predict.gam(
        object,
        newdata = reference,
        type    = "lpmatrix")[, col_ind, drop = FALSE]
    } else {
      model.matrix(object$formula[-2], data = reference)[, col_ind, drop = FALSE]
    }
    X <- X - X_ref
  }

  newdata[["fit"]] <- drop(X %*% object$coefficients[col_ind])
  if (se.fit) {
    cov.coefs <- if (is_gam) {
      object$Vp[col_ind, col_ind]
    } else {
      vcov(object)[col_ind, col_ind]
    }
    se <- sqrt(rowSums( (X %*% cov.coefs) * X ))
    newdata <- newdata %>%
      mutate(
        ci_lower = .data$fit - se_mult * se,
        ci_upper = .data$fit + se_mult * se)
  }

  return(newdata)

}

#' Extract partial effects for specified model terms
#'
#' @param data A data frame containing variables used to fit the model. Only
#' first row will be used.
#' @param fit A fitted object of class \code{\link[mgcv]{gam}}.
#' @param term The (non-linear) model term of interest.
#' @param ... Further arguments passed to \code{\link{seq_range}}.
#' @inheritParams seq_range
#' @import dplyr
#' @importFrom stats predict
#' @keywords internal
get_term <- function(data, fit, term, n = 100, ...) {

  # values at which term contribution will be evaluated
  seq_term <- data %>% pull(term) %>% seq_range(n = n)

  # use first row as basis (values of other covariates irrelevant anyway)
  new_df <- data[1, ]

  # clean up as rest of the data not needed any longer
  rm(data)
  gc()

  term_name <- term
  # extract term contribution information (+ standard errors)
  new_df              <- new_df[rep(1, length(seq_term)), ]
  new_df[[term_name]] <- seq_term
  term_info           <- predict(fit, newdata = new_df, type = "terms",
    se.fit = TRUE)
  index_term          <- grep(term, colnames(term_info$fit), value = TRUE)

  new_df %>%
    mutate(
      term = term_name,
      fit  = as.numeric(term_info$fit[, index_term]),
      se   = as.numeric(term_info$se.fit[, index_term])) %>%
    mutate(
      ci_lower = .data$fit - 2 * .data$se,
      ci_upper = .data$fit + 2 * .data$se) %>%
  select(one_of(c("term", term_name, "fit", "se", "ci_lower", "ci_upper"))) %>%
  rename(x = !!term_name) %>%
  as_tibble()

}

#' Extract the partial effects of non-linear model terms
#'
#' This function basically creates a new \code{df} from \code{data} for
#' each term in \code{terms}, creating a range from minimum and maximum of the
# respective terms. For each \code{df} it then calls
#' \code{predict(fit, newdata=df, type="terms")}. Terms are then
#' stacked to a tidy data frame.
#'
#' @inheritParams get_term
#' @param terms A character vector (can be length one). Specifies the terms
#' for which partial effects will be returned
#' @import checkmate
#' @importFrom purrr map_dfr
#' @return A tibble with 5 columns.
#' @examples
#' mod <- mgcv::gam(Sepal.Length ~ s(Sepal.Width), data = iris)
#' terms_df <- iris %>% get_terms(mod, terms = c("Sepal.Width"))
#' head(terms_df)
#' tail(terms_df)
#' @export
get_terms <- function(data, fit, terms, ...) {

  # check inputs
  assert_class(data, "data.frame")
  assert_character(terms, min.len = 1, unique = TRUE)

  # apply get_term to each element of terms
  map_dfr(terms, function(x) get_term(data = data, fit = fit, term = x), ...)

}
