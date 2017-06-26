#' Calibrates a 14C date using oxcal
#'
#' @param bp A vector containing the bp dates of the measurements
#' @param std A vector containing the standard deviations of the measurements
#' @param names The names of the measurements, usually the Laboratory numbers
#'
#' @return An object of class \code{\link{oxcAARCalibratedDatesList}}
#'@export

oxcalCalibrate <- function(bp, std, names = 1:length(bp)) {
    oxcal_script <- R_Date(names, bp, std)
    result_file <- executeOxcalScript(oxcal_script)
    result <- readOxcalOutput(result_file)
    RVA <- parseOxcalOutput(result)
    RVA
}
