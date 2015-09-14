#' Executes an Oxcal Script
#'
#' Takes an Oxcal Script, hands it over to oxcal and receives the output that is read from the output file
#'
#' @param oxcal_script A string containing the Oxcal commands that should be processed.
#' @return The path to the js output file
#'
#' @author Martin Hinz
#' @export
executeOxcalScript <- function(oxcal_script) {
    oxcal_path <- getOxcalExecutablePath()
    option_file <- tempfile()
    output_file <- paste(option_file, ".js", sep = "")
    print(output_file)
    cat(oxcal_script, file = option_file)
    system(paste(oxcal_path, option_file))
    result <- output_file
    result
}
