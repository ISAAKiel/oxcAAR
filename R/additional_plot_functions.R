#' Plots calibrated dates on the calibration curve
#'
#' @param calibrated_dates Either one or a list of calibrated dates
#' @param dates_sigma_ranges the sigma ranges used for the error bars
#'
#' @return NULL
#' @importFrom grDevices rgb
#' @importFrom graphics arrows plot polygon
#'@export

calcurve_plot <- function(calibrated_dates, dates_sigma_ranges = c("two_sigma", "one_sigma", "three_sigma")) {
  dates_sigma_ranges <- match.arg(dates_sigma_ranges)

  if(!(is.oxcAARCalibratedDate(calibrated_dates) | is.oxcAARCalibratedDatesList(calibrated_dates))) {
    stop("only working for oxcAARCalibratedDate or oxcAARCalibratedDatesList, sorry!")
  }

  if(is.oxcAARCalibratedDate(calibrated_dates)) {
    calibrated_dates <- list(one=calibrated_dates)
    class(calibrated_dates) <- append(class(calibrated_dates),"oxcAARCalibratedDatesList")
  }

  calcurve_names <- sapply(get_cal_curve(calibrated_dates), function(x) {x$name})
  if(!all(calcurve_names == calcurve_names[1])) {
    stop("calcurve_plot for dates with different calibration curves not implemented yet!")
  }

  this_cal_curve <- get_cal_curve(calibrated_dates)[[1]]
  this_bcs <- unlist(lapply(get_raw_probabilities(calibrated_dates),function(x) x$dates))
  min_bc <- min(this_bcs)
  max_bc <- max(this_bcs)
  this_cal_curve_core <- data.frame(bc = this_cal_curve$bc,
                                    bp = this_cal_curve$bp,
                                    sigma = this_cal_curve$sigma)
  this_cal_curve_core <- subset(this_cal_curve_core, bc > min_bc & bc < max_bc)
  plot(this_cal_curve_core$bc, this_cal_curve_core$bp, type="l", xlab = "bc", ylab = "bp")
  sigma_poly <- data.frame(x=c(this_cal_curve_core$bc,rev(this_cal_curve_core$bc)),
                           y = c(this_cal_curve_core$bp+this_cal_curve_core$sigma, rev(this_cal_curve_core$bp-this_cal_curve_core$sigma) ))
  polygon(sigma_poly$x,sigma_poly$y, col=rgb(0.2, 0.2, 0.2,0.2), border = F )

  for (i in 1:length(calibrated_dates)) {
    this_date <- calibrated_dates[[i]]
    this_sigma_range <- this_date[['sigma_ranges']][[dates_sigma_ranges]]
    full_sigma_range <- c(min(this_sigma_range$start),max(this_sigma_range$end))

    arrows(mean(full_sigma_range),
           this_date$bp - this_date$std,
           mean(full_sigma_range),
           this_date$bp + this_date$std,
           length=0.05, angle=90, code=3)

    arrows(min(full_sigma_range),
           this_date$bp,
           max(full_sigma_range),
           this_date$bp,
           length=0.05, angle=90, code=3)
  }
}
