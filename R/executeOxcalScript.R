#' Executes an Oxcal Script
#'
#' Takes an Oxcal Script, hands it over to oxcal and receives the output that is read from the output file
#'
#' @param oxcal_script A string containing the Oxcal commands that should be processed.
#' @param file A string naming a file for writing. Elements of the path other
#'  than the last will be created if needed. If `NULL` (the default),
#'  a temporary file will be used.
#' @return The path to the js output file
#'
#' @author Martin Hinz
#' @export
executeOxcalScript <- function(oxcal_script, file = NULL) {
    oxcal_path <- getOxcalExecutablePath()
    if (is.null(file)) {
      option_file <- tempfile()
    } else {
      option_dir <- dirname(file)
      if (!dir.exists(option_dir)) dir.create(option_dir, recursive = TRUE)
      option_file <- file
    }
    output_file <- paste(option_file, ".js", sep = "")
    cat(oxcal_script, file = option_file)
    suppressWarnings(system(paste(shQuote(normalizePath(oxcal_path)), option_file)))
    result <- output_file
    result
}
