#' @name get_tidy_oxcalresult
#' @title tidy output
#'
#' @description Transforms oxcAAR output to a tidy data format.
#' See \url{http://vita.had.co.nz/papers/tidy-data.html} and
#' \url{https://CRAN.R-project.org/package=broom}
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return a data.frame (with list columns)
#' @export
#'
#' @rdname get_tidy_oxcalresult
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_tidy_oxcalresult(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_tidy_oxcalresult(y)
#' }
#'
get_tidy_oxcalresult <- function(x) {
  UseMethod("get_tidy_oxcalresult")
}

#' @rdname get_tidy_oxcalresult
#' @export
get_tidy_oxcalresult.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_tidy_oxcalresult
#' @export
get_tidy_oxcalresult.oxcAARCalibratedDate <- function(x) {
  res <- data.frame(
    name = get_name(x),
    bp = get_bp(x),
    std = get_std(x),
    cal_curve = I(list(get_cal_curve(x))),
    sigma_ranges = I(list(get_sigma_ranges(x))),
    raw_probabilities = I(list(get_raw_probabilities(x))),
    posterior_sigma_ranges = I(get_posterior_sigma_ranges(x)),
    posterior_probabilities = I(get_posterior_probabilities(x)),
    stringsAsFactors = FALSE
  )
  return(res)
}

#' @rdname get_tidy_oxcalresult
#' @export
get_tidy_oxcalresult.oxcAARCalibratedDatesList <- function(x) {
  res <- data.frame(
    name = get_name(x),
    bp = get_bp(x),
    std = get_std(x),
    cal_curve = I(get_cal_curve(x)),
    sigma_ranges = I(get_sigma_ranges(x)),
    raw_probabilities = I(get_raw_probabilities(x)),
    posterior_sigma_ranges = I(get_posterior_sigma_ranges(x)),
    posterior_probabilities = I(get_posterior_probabilities(x)),
    stringsAsFactors = FALSE
  )
  return(res)
}
