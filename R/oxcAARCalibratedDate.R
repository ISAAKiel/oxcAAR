#' oxcAAR Calibrated Dates Object
#'
#' The function \code{oxcAARCalibratedDate} is used to create an object for a calibrated date.
#'
#' @param name a string giving the name of the date (usually the lab number)
#' @param bp a integer giving the BP value for the date
#' @param std a integer giving the standard deviation for the date
#' @param cal_curve a list containing information about the calibration curve (name, resolution, bp, bc, sigma)
#' @param sigma_ranges a list of three elements (one, two, three sigma),
#' each a data frame with start, end and probability giving
#' @param raw_probabilities a data frame of dates and the related probabilities for each date
#' @param posterior_sigma_ranges a list of three elements (one, two, three sigma),
#' each a data frame with start, end and probability giving for the posterior probabilities
#' @param posterior_probabilities a data frame of dates and the related posterior probabilities for each date
#'
#' @return an object of the class \code{'oxcAARCalibratedDate'}
#' @export
oxcAARCalibratedDate <- function(name, bp, std, cal_curve,
                                 sigma_ranges, raw_probabilities, posterior_probabilities=NA,
                                 posterior_sigma_ranges=NA){

  RVA <- structure(list(),class="oxcAARCalibratedDate")
  RVA$name <- name
  RVA$bp <- bp
  RVA$std <- std
  RVA$cal_curve <- cal_curve
  RVA$sigma_ranges <- sigma_ranges
  RVA$raw_probabilities <- raw_probabilities
  RVA$posterior_sigma_ranges <- posterior_sigma_ranges
  RVA$posterior_probabilities <- posterior_probabilities
  RVA
}

##' @export
format.oxcAARCalibratedDate <- function(x, ...){

  out_str <- list()
  sigma_str <- list()
  out_str$upper_sep <- "\n============================="
  out_str$name_str <- paste("\t",x$name,sep = "")
  out_str$name_sep <- "=============================\n"

  out_str$uncal_str <- paste(sprintf("\nBP = %d, std = %d",
                                     x$bp,x$std),
                             "\n",sep = "")

  sigma_str$unmodelled_remark <- paste("unmodelled:")
  sigma_str$one_sigma_str <- formatFullSigmaRange(x$sigma_ranges$one_sigma,
                                                "one sigma")
  sigma_str$two_sigma_str <- formatFullSigmaRange(x$sigma_ranges$two_sigma,
                                                "two sigma")
  sigma_str$three_sigma_str <- formatFullSigmaRange(x$sigma_ranges$three_sigma,
                                                  "three sigma")
  sigma_str$modelled_remark <- sigma_str$posterior_one_sigma_str <- sigma_str$posterior_two_sigma_str <- sigma_str$posterior_three_sigma_str <- ""
  if(class(x$posterior_probabilities)=="data.frame"){
    sigma_str$modelled_remark <- paste("posterior:")
    sigma_str$posterior_one_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,"one sigma")
    sigma_str$posterior_two_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,"two sigma")
    sigma_str$posterior_three_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma,"three sigma")
  }
  out_str$sigma_remark <- sprintf("%s   %31s",sigma_str$unmodelled_remark, sigma_str$modelled_remark)
  out_str$sigma_divider <- paste(rep(" ",63), collapse="")
  out_str$one_sigma <- sprintf("%32s   %s",sigma_str$one_sigma_str, sigma_str$posterior_one_sigma_str)
  out_str$two_sigma <- sprintf("%32s   %s",sigma_str$two_sigma_str, sigma_str$posterior_two_sigma_str)
  out_str$three_sigma <- sprintf("%32s   %s",sigma_str$three_sigma_str, sigma_str$posterior_three_sigma_str)
  out_str$cal_curve_str <- sprintf("\nCalibrated after:\n\t %s",x$cal_curve$name)

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

  graphics::mtext(paste(x$cal_curve$name), side = 1, line = 4, adj = 0,
        cex = 0.6)
}

#' Checks if a variable is of class oxcAARCalibratedDate
#'
#' Checks if a variable is of class oxcAARCalibratedDate
#'
#' @param x a variable
#'
#' @return true if x is a oxcAARCalibratedDate, false otherwise
#'
#' @export
is.oxcAARCalibratedDate <- function(x) {"oxcAARCalibratedDate" %in% class(x)}
