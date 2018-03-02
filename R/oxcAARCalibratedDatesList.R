#' oxcAAR Calibrated Dates List
#'
#' A List of \code{\link{oxcAARCalibratedDate}}
#'
#' @return an object of the class 'oxcAARCalibratedDatesList'
#' @name oxcAARCalibratedDatesList
NULL

#' @export
print.oxcAARCalibratedDatesList <- function(x, ...){
  n <- length(x)
  if (n == 1) print(x[[1]],...)
  else{
    cat(sprintf("List of %d calibrated dates:\n",n))
    for (i in 1:n) {
      print(x[[i]])
    }
  }
}

#' @export
plot.oxcAARCalibratedDatesList <- function(x, ...){
  if (length(x) == 1) graphics::plot(x[[1]],...)
  else{
    #     if (requireNamespace("ggplot2", quietly = TRUE)) {
    #       #  if (FALSE) {
    #       plotoxcAARCalibratedDatesListGGPlot2(x, ...)
    #     } else {
    plotoxcAARCalibratedDatesListSystemGraphics(x, ...)
    # }
  }
}


plotoxcAARCalibratedDatesListSystemGraphics <- function(x, ...){
  op <- graphics::par(no.readonly = TRUE)
  indices <- 1:length(x)
  min_year <- min(
    sapply(
      indices,
      function(i) min(x[[i]]$raw_probabilities$dates)
    )
  )
  max_year <- max(
    sapply(
      indices,
      function(i) max(x[[i]]$raw_probabilities$dates)
    )
  )
  graphics::par(mfrow=c(length(x),1))
  graphics::par(oma = c(3,4,2,3) + 0.1,
                mar = c(0,1,0,1) + 0.1)
  for (i in indices) {
    years <- x[[i]]$raw_probabilities$dates
    probability <- x[[i]]$raw_probabilities$probabilities
    years_post <- probability_post <- NA
    unmodelled_color <- "lightgrey"
    max_prob <- max(probability)
    if (class(x[[i]]$posterior_probabilities)=="data.frame"){
      years_post <- x[[i]]$posterior_probabilities$dates
      probability_post <- x[[i]]$posterior_probabilities$probabilities
      unmodelled_color <- "#eeeeeeee"
      max_prob <- max(max_prob, probability_post)
    }
    graphics::plot(
      years, probability,
      type = "n",
      ylim=c(max_prob / 7 * -1,max_prob),
      xlim = c(min_year, max_year),
      axes = FALSE
    )
    graphics::axis(side=4)
    graphics::polygon(years, probability, border = "black", col = unmodelled_color)
    if (unmodelled_color!="lightgrey"){
      graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
    }
    graphics::mtext(x[[i]]$name,side=2,las=2,cex=0.6)
    graphics::grid()
  }
  graphics::axis(side=1)
  graphics::par(op)
}

#' Checks if a variable is of class oxcAARCalibratedDatesList
#'
#' Checks if a variable is of class oxcAARCalibratedDatesList
#'
#' @param x a variable
#'
#' @return true if x is a oxcAARCalibratedDatesList, false otherwise
#'
#' @export
is.oxcAARCalibratedDatesList <- function(x) {"oxcAARCalibratedDatesList" %in% class(x)}
