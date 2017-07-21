#' oxcAAR Calibratred Dates Object
#'
#' The function \code{oxcAARCalibratedDate} is used to create an object for a calibrated date.
#'
#' @param name a string giving the name of the date (usually the lab number)
#' @param bp a integer giving the BP value for the date
#' @param std a integer giving the standard deviation for the date
#' @param cal_curve a string giving the used calibration curve
#' @param sigma_ranges a list of three elements (one, two, three sigma),
#' each a data frame with start, end and probability giving
#' @param raw_probabilities a data frame of dates and the related probabilities for each date
#'
#' @return an object of the class \code{'oxcAARCalibratedDate'}
#' @export
oxcAARCalibratedDate <- function(name, bp, std, cal_curve,
                                 sigma_ranges, raw_probabilities){

  RVA <- structure(list(),class="oxcAARCalibratedDate")
  RVA$name <- name
  RVA$bp <- bp
  RVA$std <- std
  RVA$cal_curve <- cal_curve
  RVA$sigma_ranges <- sigma_ranges
  RVA$raw_probabilities <- raw_probabilities
  RVA
}

##' @export
format.oxcAARCalibratedDate <- function(x, ...){

  out_str <- list()
  out_str$name_str <- paste("\n","\t",x$name,sep = "")
  out_str$uncal_str <- paste(sprintf("BP = %d, std = %d",
                                     x$bp,x$std),
                             "\n",sep = "")
  out_str$one_sigma_str <- formatFullSigmaRange(x$sigma_ranges$one_sigma,
                                                "one sigma")
  out_str$two_sigma_str <- formatFullSigmaRange(x$sigma_ranges$two_sigma,
                                                "two sigma")
  out_str$three_sigma_str <- formatFullSigmaRange(x$sigma_ranges$three_sigma,
                                                  "three sigma")
  out_str$cal_curve_str <- sprintf("\nCalibrated after:\n\t %s",x$cal_curve)

  RVA <- paste(out_str,collapse = "\n")
  invisible(RVA)
}

#' @export
print.oxcAARCalibratedDate <- function(x, ...) cat(format(x, ...), "\n")

##' @export
plot.oxcAARCalibratedDate <- function(x, ...){
  #   if (requireNamespace("ggplot2", quietly = TRUE)) {
  #     plotoxcAARDateGGPlot2(x, ...)
  #   } else {
  plotoxcAARDateSystemGraphics(x, ...)
  # }
}

plotoxcAARDateSystemGraphics <- function(x, ...){
  years <- x$raw_probabilities$dates
  probability <- x$raw_probabilities$probabilities
  max_prob <- max(probability)
  graphics::plot(years, probability, main = x$name, type = "n",
       ylim = c(max_prob / 7 * -1, max_prob))
  graphics::mtext(
    formatFullSigmaRange(x$sigma_ranges$one_sigma,"one sigma"),
    3, line=0, cex=0.6, adj=1
  )
  graphics::mtext(
    formatFullSigmaRange(x$sigma_ranges$two_sigma,"two sigma"),
    3, line=1,cex=0.6,adj=1
  )
  graphics::mtext(
    formatFullSigmaRange(x$sigma_ranges$three_sigma,"three sigma"),
    3, line=2,cex=0.6,adj=1
  )
  graphics::polygon(years, probability, col = "lightgrey")
  if (length(x$sigma_ranges$one_sigma[,1]) > 0){
    y_pos <- max_prob / 24 * -1
    arrow_length <- max_prob / 8
    graphics::arrows(
      x$sigma_ranges$one_sigma[,1],
      y_pos,
      x$sigma_ranges$one_sigma[,2],
      y_pos,
      length(x$sigma_ranges$one_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos * 2
    graphics::arrows(
      x$sigma_ranges$two_sigma[,1],
      y_pos,
      x$sigma_ranges$two_sigma[,2],
      y_pos,
      length(x$sigma_ranges$two_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos / 2 * 3
    graphics::arrows(
      x$sigma_ranges$three_sigma[,1],
      y_pos,
      x$sigma_ranges$three_sigma[,2],
      y_pos,
      length(x$sigma_ranges$three_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
  }

  graphics::mtext(paste(x$cal_curve), side = 1, line = 4, adj = 0,
        cex = 0.6)
}

#### getter ####

# helper function to check class attribute
check_if_class_is_oxcAARCalibratedDate <- function(x) {
  if (!("oxcAARCalibratedDate" %in% class(x))) stop("x is not an object of class oxcAARCalibratedDate")
}

#' get date name
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_name(x)
#' }
#'
get_name <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["name"]])
}

#' get date bp value (age)
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_bp(x)
#' }
#'
get_bp <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["bp"]])
}

#' get date std value (standard deviation)
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return an integer
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_std(x)
#' }
#'
get_std <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["std"]])
}

#' get calibration curve name
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return a string
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_cal_curve(x)
#' }
#'
get_cal_curve <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["cal_curve"]])
}

#' get sigma ranges
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return a list of three data.frames
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_sigma_ranges(x)
#' }
#'
get_sigma_ranges <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["sigma_ranges"]])
}

#' get raw probabilities
#'
#' @param x an object of class oxcAARCalibratedDate
#'
#' @return a data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' x <- oxcalCalibrate(5000, 20)[[1]]
#' get_raw_probabilities(x)
#' }
#'
get_raw_probabilities <- function(x) {
  check_if_class_is_oxcAARCalibratedDate(x)
  return(x[["raw_probabilities"]])
}
