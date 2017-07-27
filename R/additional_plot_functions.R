#' Plots calibrated dates on the calibration curve
#'
#' @param x an object of class oxcAARCalibratedDate or oxcAARCalibratedDatesList
#' @param dates_sigma_ranges character. The sigma range used for the error bars ("two_sigma", "one_sigma" or "three_sigma")
#' @param uncal_range logical. If TRUE (default), the plot contains error bars for the the uncalibrated age
#' @param cal_range logical. If TRUE (default), the plot contains error bars for the the calibrated age
#'
#' @return NULL
#' @importFrom grDevices rgb
#' @importFrom graphics arrows plot polygon rug points
#'@export

calcurve_plot <- function(x, dates_sigma_ranges = NULL, uncal_range = TRUE, cal_range = TRUE) {
  dates_sigma_ranges <- match.arg(dates_sigma_ranges, c("two_sigma", "one_sigma", "three_sigma"))

  if(!(is.oxcAARCalibratedDate(x) | is.oxcAARCalibratedDatesList(x))) {
    stop("only working for oxcAARCalibratedDate or oxcAARCalibratedDatesList, sorry!")
  }

  if(is.oxcAARCalibratedDate(x)) {
    x <- list(one=x)
    class(x) <- append(class(x),"oxcAARCalibratedDatesList")
  }

  calcurve_names <- sapply(get_cal_curve(x), function(x) {x$name})
  if(!all(calcurve_names == calcurve_names[1])) {
    stop("calcurve_plot for dates with different calibration curves not implemented yet!")
  }

  this_cal_curve <- get_cal_curve(x)[[1]]
  this_bcs <- unlist(lapply(get_raw_probabilities(x),function(x) x$dates))
  min_bc <- min(this_bcs)
  max_bc <- max(this_bcs)
  this_cal_curve_core <- data.frame(bc = this_cal_curve$bc,
                                    bp = this_cal_curve$bp,
                                    sigma = this_cal_curve$sigma)
  this_cal_curve_core <- subset(this_cal_curve_core, this_cal_curve_core$bc > min_bc & this_cal_curve_core$bc < max_bc)
  plot(this_cal_curve_core$bc, this_cal_curve_core$bp, type="l", xlab = "bc", ylab = "bp")
  sigma_poly <- data.frame(x=c(this_cal_curve_core$bc,rev(this_cal_curve_core$bc)),
                           y = c(this_cal_curve_core$bp+this_cal_curve_core$sigma, rev(this_cal_curve_core$bp-this_cal_curve_core$sigma) ))
  polygon(sigma_poly$x,sigma_poly$y, col=rgb(0.2, 0.2, 0.2,0.2), border = F )

  for (i in 1:length(x)) {
    this_date <- x[[i]]

    # sigma ranges cal age
    this_sigma_range <- this_date[['sigma_ranges']][[dates_sigma_ranges]]
    full_sigma_range <- c(min(this_sigma_range$start),max(this_sigma_range$end))

    # define sigma range factor for the uncal age
    uncal_age_k <- switch(
      dates_sigma_ranges,
      one_sigma = 1,
      two_sigma = 2,
      three_sigma = 3
    )

    # rug plot
    rug(mean(full_sigma_range))
    rug(this_date$bp, side = 2)

    # plot point if one error bar is unwanted
    if(any(c(!uncal_range, !cal_range))) {
      points(mean(full_sigma_range), this_date$bp)
    }

    # error bars
    if(uncal_range){
      arrows(mean(full_sigma_range),
             this_date$bp - this_date$std * uncal_age_k,
             mean(full_sigma_range),
             this_date$bp + this_date$std * uncal_age_k,
             length=0.05, angle=90, code=3)
    }
    if(cal_range){
      arrows(min(full_sigma_range),
             this_date$bp,
             max(full_sigma_range),
             this_date$bp,
             length=0.05, angle=90, code=3)
    }
  }
}
