#' Sum calibration for simulated dates
#'
#' @details The dates can be distributed using one of the following methods:
#' 'equidist' distributed the n dates within the time frame with equal distance,
#' 'uniform' random samples n dates from the given time interval with uniform distribution
#'
#' @param timeframe_begin,timeframe_end beginning and end of the time frame for which dates should be simulated
#' @param n the number of dates that should be simulated
#' @param stds either one standard deviation for all dates or a vector of standard deviations with length n
#' @param date_distribution a character string indicating which method should be used to distribute the dates in the given time frame, can be abbreviated
#' @return A list containing the following components:
#'  \item{dates}{the dates for the simulated sum calibration}
#'  \item{probabilities}{the probabilities for the simulated sum calibration}
#'  \item{date_distribution}{the distribution method used for the dates}
#' @export

oxcalSumSim <- function(timeframe_begin,
                        timeframe_end,
                        n,
                        stds,
                        date_distribution = c("equidist", "uniform")) {
    if (!(length(stds) %in% c(1, n))) {
        stop("Please give either one stds for all
             dates or a vector of stds of n length.")
    }

    date_distribution <- match.arg(date_distribution)

    if (date_distribution == "equidist") {
        cal_dates <- seq(timeframe_begin,
                         timeframe_end,
                         by = ( (timeframe_end - timeframe_begin) / n) )
    } else if (date_distribution == "uniform") {
        date_range <- seq(timeframe_begin, timeframe_end)
        cal_dates <- sample(date_range, n, replace = T)
    } else {
        stop("Please give an option for the date distribution.")
    }

    script <- oxcal_Sum(R_Simulate(cal_dates, stds))
    result_file <- executeOxcalScript(script)
    result <- readOxcalOutput(result_file)
    RVAL <- parseOxcalOutput(result,first=TRUE,only.R_Date=FALSE)[[1]]
#     RVAL <- list(date_distribution = date_distribution,
#                  dates = result$dates,
#                  probabilities = result$probabilities)
    RVAL
}
