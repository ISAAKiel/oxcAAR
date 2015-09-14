#' Reads the content of the Oxcal js output file
#'
#' Reads the content of the Oxcal js output file as vector of strings for each line.
#'
#' @param output_file The path to a Oxcal js output file.
#' @return The content of the Oxcal js output file as vector of strings for each line.
#'
#' @author Martin Hinz
#' @export

readOxcalOutput <- function(output_file) {
  result <- scan(output_file, character(0), sep = "\n")
  result
}
