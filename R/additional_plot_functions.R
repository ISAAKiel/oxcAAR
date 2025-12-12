#' Plot calibrated dates on the calibration curve
#'
#' Draws the calibration curve (with 1-sigma envelope) and overlays one or more calibrated
#' dates as points and/or error bars (uncalibrated and calibrated ranges).
#'
#' @param x An object of class \code{\link{oxcAARCalibratedDate}} or
#'   \code{\link{oxcAARCalibratedDatesList}}.
#' @param dates_sigma_ranges Character. The sigma range used for the calibrated error bars.
#'   One of \code{"two_sigma"} (default), \code{"one_sigma"}, \code{"three_sigma"}.
#' @param uncal_range Logical. If \code{TRUE} (default), the plot contains vertical error bars
#'   for the uncalibrated age (BP \eqn{\pm} k * std).
#' @param cal_range Logical. If \code{TRUE} (default), the plot contains horizontal error bars
#'   for the calibrated age (sigma range in calendar years).
#'
#' @return \code{NULL} (called for its side effect: base graphics plot).
#' @importFrom grDevices rgb
#' @importFrom graphics arrows plot polygon rug points
#' @export
calcurve_plot <- function(x,
                          dates_sigma_ranges = c("two_sigma", "one_sigma", "three_sigma"),
                          uncal_range = TRUE,
                          cal_range = TRUE) {
  dates_sigma_ranges <- match.arg(dates_sigma_ranges)

  if (!is.oxcAARCalibratedDate(x) && !is.oxcAARCalibratedDatesList(x)) {
    stop("`x` must be an oxcAARCalibratedDate or oxcAARCalibratedDatesList.", call. = FALSE)
  }

  # Normalise input: always work with a list
  if (is.oxcAARCalibratedDate(x)) {
    x <- list(x)
    class(x) <- c("oxcAARCalibratedDatesList", class(x))
  }

  if (length(x) == 0L) {
    warning("No dates supplied.", call. = FALSE)
    return(invisible(NULL))
  }

  curves <- get_cal_curve(x)

  # Avoid relying on %||% (unless you define it in-package)
  curve_names <- vapply(curves, function(cur) {
    nm <- cur$name
    if (is.null(nm)) NA_character_ else as.character(nm)
  }, character(1))

  if (!all(curve_names == curve_names[1], na.rm = TRUE)) {
    stop("calcurve_plot() for dates with different calibration curves is not implemented.", call. = FALSE)
  }

  raw_probs <- get_raw_probabilities(x)
  has_raw <- vapply(raw_probs, function(df) is.data.frame(df) && nrow(df) > 0L, logical(1))
  if (!any(has_raw)) {
    stop("No raw probabilities available to determine calibrated date ranges for plotting.", call. = FALSE)
  }

  bcs <- unlist(lapply(raw_probs[has_raw], function(df) df$dates), use.names = FALSE)
  min_bc <- min(bcs, na.rm = TRUE)
  max_bc <- max(bcs, na.rm = TRUE)

  this_curve <- curves[[which(has_raw)[1]]]
  curve_df <- data.frame(
    bc = this_curve$bc,
    bp = this_curve$bp,
    sigma = this_curve$sigma
  )

  # IMPORTANT FIX: no `.data` in base subset()
  curve_df <- curve_df[curve_df$bc >= min_bc & curve_df$bc <= max_bc, , drop = FALSE]

  if (nrow(curve_df) < 2L) {
    stop("Calibration curve subset is empty/too small for the selected date range.", call. = FALSE)
  }

  graphics::plot(curve_df$bc, curve_df$bp, type = "l", xlab = "bc", ylab = "bp")

  sigma_poly <- data.frame(
    x = c(curve_df$bc, rev(curve_df$bc)),
    y = c(curve_df$bp + curve_df$sigma, rev(curve_df$bp - curve_df$sigma))
  )
  graphics::polygon(
    sigma_poly$x, sigma_poly$y,
    col = grDevices::rgb(0.2, 0.2, 0.2, 0.2),
    border = FALSE
  )

  uncal_k <- switch(
    dates_sigma_ranges,
    one_sigma = 1,
    two_sigma = 2,
    three_sigma = 3
  )

  get_full_sigma_range <- function(date_obj) {
    sr <- date_obj[["sigma_ranges"]][[dates_sigma_ranges]]
    if (!is.data.frame(sr) || nrow(sr) == 0L) return(c(NA_real_, NA_real_))
    c(min(sr$start, na.rm = TRUE), max(sr$end, na.rm = TRUE))
  }

  for (i in seq_along(x)) {
    this_date <- x[[i]]

    full_sigma_range <- get_full_sigma_range(this_date)
    if (anyNA(full_sigma_range) || !is.finite(this_date$bp) || !is.finite(this_date$std)) next

    centre_bc <- mean(full_sigma_range)

    graphics::rug(centre_bc)
    graphics::rug(this_date$bp, side = 2)

    if (!isTRUE(uncal_range) || !isTRUE(cal_range)) {
      graphics::points(centre_bc, this_date$bp)
    }

    if (isTRUE(uncal_range)) {
      graphics::arrows(
        x0 = centre_bc,
        y0 = this_date$bp - this_date$std * uncal_k,
        x1 = centre_bc,
        y1 = this_date$bp + this_date$std * uncal_k,
        length = 0.05, angle = 90, code = 3
      )
    }

    if (isTRUE(cal_range)) {
      graphics::arrows(
        x0 = full_sigma_range[1],
        y0 = this_date$bp,
        x1 = full_sigma_range[2],
        y1 = this_date$bp,
        length = 0.05, angle = 90, code = 3
      )
    }
  }

  invisible(NULL)
}
