#' oxcAAR Calibrated Dates Object
#'
#' The function \code{oxcAARCalibratedDate} is used to create an object for a calibrated date.
#'
#' @param name a string giving the name of the date (usually the lab number)
#' @param type a string giving the type of the date in OxCal terminology ("R_Date", "R_Simulate", ...)
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
oxcAARCalibratedDate <- function(name, type, bp, std, cal_curve,
                                 sigma_ranges, raw_probabilities, posterior_probabilities=NA,posterior_sigma_ranges=NA){

  RVA <- structure(list(),class="oxcAARCalibratedDate")
  RVA$name <- name
  RVA$type <- type
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
  out_str$name_str <- paste("\t",print_label(x),sep = "")
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
  max_prob <- 0
  years <- years_post <- NA
  probability <- probability_post <- NA
  prob_present <- post_present <- FALSE

  if(has_raw_probabilities(x)) {prob_present <- TRUE}
  if(has_posterior_probabilities(x)) {post_present <- TRUE}

  if (prob_present){
    years <- x$raw_probabilities$dates
    probability <- x$raw_probabilities$probabilities
    unmodelled_color <- "lightgrey"
    max_prob <- max(probability)
    this_sigma_ranges <- x$sigma_ranges
  }
  if (post_present){
    years_post <- x$posterior_probabilities$dates
    probability_post <- x$posterior_probabilities$probabilities
    unmodelled_color <- "#eeeeeeee"
    max_prob <- max(max_prob, probability_post)
    this_sigma_ranges <- x$posterior_sigma_ranges
  }

  if(!prob_present & !post_present)
  {
    year_range <-c(0,1)
  } else {
    year_range <- get_years_range(x)
  }

  prob_range <- c(0,min(max_prob,1,na.rm=T))

  graphics::plot(year_range, prob_range, main = print_label(x), type = "n",
                 ylim = c(max_prob / 7 * -1, max_prob))
  if(prob_present){
    graphics::mtext(
      "unmodelled",
      3, line=3, cex=0.6, adj=0
    )
    graphics::mtext(
      formatFullSigmaRange(x$sigma_ranges$one_sigma,"one sigma"),
      3, line=0, cex=0.6, adj=0
    )
    graphics::mtext(
      formatFullSigmaRange(x$sigma_ranges$two_sigma,"two sigma"),
      3, line=1,cex=0.6,adj=0
    )
    graphics::mtext(
      formatFullSigmaRange(x$sigma_ranges$three_sigma,"three sigma"),
      3, line=2,cex=0.6,adj=0
    )
  }
  if (post_present) {
    graphics::mtext(
      "posterior",
      3, line=3, cex=0.6, adj=1
    )
    graphics::mtext(
      formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,"one sigma"),
      3, line=0, cex=0.6, adj=1
    )
    graphics::mtext(
      formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,"two sigma"),
      3, line=1,cex=0.6,adj=1
    )
    graphics::mtext(
      formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma,"three sigma"),
      3, line=2,cex=0.6,adj=1
    )
  }
  if(!prob_present & !post_present) {return()}
  if(prob_present){
    graphics::polygon(years, probability, border = "black", col = unmodelled_color)
  }
  if (post_present){
    graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
  }
  if (length(this_sigma_ranges$one_sigma[,1]) > 0){
    y_pos <- max_prob / 24 * -1
    arrow_length <- max_prob / 8
    graphics::arrows(
      this_sigma_ranges$one_sigma[,1],
      y_pos,
      this_sigma_ranges$one_sigma[,2],
      y_pos,
      length(this_sigma_ranges$one_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos * 2
    graphics::arrows(
      this_sigma_ranges$two_sigma[,1],
      y_pos,
      this_sigma_ranges$two_sigma[,2],
      y_pos,
      length(this_sigma_ranges$two_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos / 2 * 3
    graphics::arrows(
      this_sigma_ranges$three_sigma[,1],
      y_pos,
      this_sigma_ranges$three_sigma[,2],
      y_pos,
      length(this_sigma_ranges$three_sigma),
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

get_years_range <- function(calibrated_date) {
  years <- get_prior_years(calibrated_date)
  years_post <- get_posterior_years(calibrated_date)
  if (is.na(years) && is.na(years_post)) {
    return(NA)
  } else {
  return(
    c(
    min(years,years_post, na.rm = TRUE),
    max(years,years_post, na.rm = TRUE)
  )
  )
  }
}

get_prior_years <- function(calibrated_date) {
  years <- NA
  if (has_raw_probabilities(calibrated_date)){
    years <- calibrated_date$raw_probabilities$dates
  }
  return(years)
}

get_posterior_years <- function(calibrated_date) {
  years <- NA
  if (has_posterior_probabilities(calibrated_date)){
    years <- calibrated_date$posterior_probabilities$dates
  }
  return(years)
}

has_raw_probabilities <- function(calibrated_date) {
  class(calibrated_date$raw_probabilities)=="data.frame"
}

has_posterior_probabilities <- function(calibrated_date) {
  class(calibrated_date$posterior_probabilities)=="data.frame"
}

print_label <- function(calibrated_date) {
  paste(calibrated_date$type, ": " ,calibrated_date$name, sep="")
}
