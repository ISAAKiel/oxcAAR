#' Roxcal Calibrated Dates List
#'
#' A List of \code{\link{roxcalCalibratedDate}}
#'
#' @return an object of the class 'roxcalCalibratedDatesList'
#' @name roxcalCalibratedDatesList
NULL

#' @export
print.roxcalCalibratedDatesList <- function(x, ...){
  n<-length(x)
  if (n==1) print(x[[1]],...)
  else{
    cat(sprintf("List of %d calibrated dates:\n",n))
    for (i in 1:n) {
      print(x[[i]])
    }
  }
}

#' @export
plot.roxcalCalibratedDatesList <- function(x, ...){
  if (length(x)==1) plot(x[[1]],...)
  else{
    #     if (requireNamespace("ggplot2", quietly = TRUE)) {
    #       #  if (FALSE) {
    #       plotRoxcalCalibratedDatesListGGPlot2(x, ...)
    #     } else {
    plotRoxcalCalibratedDatesListSystemGraphics(x, ...)
    # }
  }
}


plotRoxcalCalibratedDatesListSystemGraphics<-function(x, ...){
  op <- par(no.readonly = TRUE)
  indices <- 1:length(x)
  min_year<-min(sapply(indices, function(i) min(x[[i]]$raw_probabilities$dates)))
  max_year<-max(sapply(indices, function(i) max(x[[i]]$raw_probabilities$dates)))
  #layout(matrix(1:length(x), ncol = 1), widths = 1, respect = FALSE,heights = 1/length(x))
  par(mfrow=c(length(x),1))
  par(oma = c(3,4,2,3) + 0.1,
      mar = c(0,1,0,1) + 0.1)
  for (i in indices)
  {
    years <- x[[i]]$raw_probabilities$dates
    probability <- x[[i]]$raw_probabilities$probabilities
    max_prob<-max(probability)
    plot(years, probability, type = "n",ylim=c(max_prob/7*-1,max_prob),xlim = c(min_year,max_year), axes = FALSE)
    axis(side=4)
    polygon(years, probability, col="lightgrey")
    mtext(x[[i]]$name,side=2,las=2,cex=0.6)
    grid()
  }
  axis(side=1)
  par(op)
}
