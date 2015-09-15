#' Simulates 14C dates using oxcal
#'
#' @param c_date A vector containing the calender dates to be simulated
#' @param std A vector containing the standard deviations for the simulated dates
#' @param names The names of the measurements, usually the Laboratory numbers
#'
#' @return An object of class \code{\link{roxcalCalibratedDatesList}}
#'@export

oxcalSimulate <- function(c_date, std, names = 1:length(c_date)) {
    oxcal_script <- R_Simulate(c_date, std, names)
    result_file <- executeOxcalScript(oxcal_script)
    result <- readOxcalOutput(result_file)
    RVA <- parseOxcalOutput(result,only.R_Date = F)
    RVA
}
