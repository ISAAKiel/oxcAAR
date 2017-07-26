#### get_name ####

#' @name get_name
#' @title get names (labcodes)
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return a string or a character vector
#' @export
#'
#' @rdname get_name
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_name(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_name(y)
#' }
#'
get_name <- function(x) {
  UseMethod("get_name")
}

#' @rdname get_name
#' @export
get_name.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_name
#' @export
get_name.oxcAARCalibratedDate <- function(x) {
  return(x[["name"]])
}

#' @rdname get_name
#' @export
get_name.oxcAARCalibratedDatesList <- function(x) {
  return(unname(sapply(x, function(x) {x[["name"]]})))
}

#### get_bp ####

#' @name get_bp
#' @title get bp values (ages)
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return an integer or a numeric vector
#' @export
#'
#' @rdname get_bp
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_bp(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_bp(y)
#' }
#'
get_bp <- function(x) {
  UseMethod("get_bp")
}

#' @rdname get_bp
#' @export
get_bp.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_bp
#' @export
get_bp.oxcAARCalibratedDate <- function(x) {
  return(x[["bp"]])
}

#' @rdname get_bp
#' @export
get_bp.oxcAARCalibratedDatesList <- function(x) {
  return(unname(sapply(x, function(x) {x[["bp"]]})))
}

#### get_std ####

#' @name get_std
#' @title get std values (standard deviations)
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return an integer or a numeric vector
#' @export
#'
#' @rdname get_std
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_std(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_std(y)
#' }
#'
get_std <- function(x) {
  UseMethod("get_std")
}

#' @rdname get_std
#' @export
get_std.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_std
#' @export
get_std.oxcAARCalibratedDate <- function(x) {
  return(x[["std"]])
}

#' @rdname get_std
#' @export
get_std.oxcAARCalibratedDatesList <- function(x) {
  return(unname(sapply(x, function(x) {x[["std"]]})))
}

#### get_cal_curve ####

#' @name get_cal_curve
#' @title get calibration curve names
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return a string or a character vector
#' @export
#'
#' @rdname get_cal_curve
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_cal_curve(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_cal_curve(y)
#' }
#'
get_cal_curve <- function(x) {
  UseMethod("get_cal_curve")
}

#' @rdname get_cal_curve
#' @export
get_cal_curve.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_cal_curve
#' @export
get_cal_curve.oxcAARCalibratedDate <- function(x) {
  return(x[["cal_curve"]])
}

#' @rdname get_cal_curve
#' @export
get_cal_curve.oxcAARCalibratedDatesList <- function(x) {
  return(unname(lapply(x, function(x) {x[["cal_curve"]]})))
}

#### get_sigma_ranges ####

#' @name get_sigma_ranges
#' @title get sigma ranges
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return a list of three data.frames or a list of those lists
#' @export
#'
#' @rdname get_sigma_ranges
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_sigma_ranges(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_sigma_ranges(y)
#' }
#'
get_sigma_ranges <- function(x) {
  UseMethod("get_sigma_ranges")
}

#' @rdname get_sigma_ranges
#' @export
get_sigma_ranges.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_sigma_ranges
#' @export
get_sigma_ranges.oxcAARCalibratedDate <- function(x) {
  return(x[["sigma_ranges"]])
}

#' @rdname get_sigma_ranges
#' @export
get_sigma_ranges.oxcAARCalibratedDatesList <- function(x) {
  return(unname(lapply(x, function(x) {x[["sigma_ranges"]]})))
}

#### get_raw_probabilities ####

#' @name get_raw_probabilities
#' @title get raw probabilities
#'
#' @description queries values from date objects
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#'
#' @return a data.frame or a list of data.frames
#' @export
#'
#' @rdname get_raw_probabilities
#' @family getter functions
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(c(5000, 4500, 3000), c(20, 50, 60))
#' get_raw_probabilities(x)
#' y <- oxcalCalibrate(5000, 20)[[1]]
#' get_raw_probabilities(y)
#' }
#'
get_raw_probabilities <- function(x) {
  UseMethod("get_raw_probabilities")
}

#' @rdname get_raw_probabilities
#' @export
get_raw_probabilities.default <- function(x) {
  stop("x is not an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList")
}

#' @rdname get_raw_probabilities
#' @export
get_raw_probabilities.oxcAARCalibratedDate <- function(x) {
  return(x[["raw_probabilities"]])
}

#' @rdname get_raw_probabilities
#' @export
get_raw_probabilities.oxcAARCalibratedDatesList <- function(x) {
  return(unname(lapply(x, function(x) {x[["raw_probabilities"]]})))
}
