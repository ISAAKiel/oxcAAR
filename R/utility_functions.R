## ---------- public ----------

#' Setting the Oxcal program path for further use
#'
#' Stores the path to the oxcal executable it in internally for other functions.
#'
#' @param path The path to the Oxcal executable
#'
#' @author Martin Hinz
#' @export
#' @examples
#' \dontrun{
#' connectOxcal('/home/martin/Documents/scripte/OxCal/bin/OxCalLinux')
#' }
#'
#' @export
#'

setOxcalExecutablePath <- function(path) {
  if (!file.exists(path))
    stop("No file at given location")
  options(roxcal.oxcal_path=path)
  print("Oxcal path set!")
}

## ---------- private ----------

getOxcalExecutablePath <- function() {
  oxcal_path<-getOption("roxcal.oxcal_path")
  if (is.null(oxcal_path) || oxcal_path=="") {
    stop("Please set path to oxcal first (using 'setOxcalExecutablePath')!")
  }
  oxcal_path
}

formatDateAdBc <- function (value_to_print) {
  RVA<- abs(value_to_print)
  suffix <- if (value_to_print<0) "BC" else if (value_to_print>0) "AD" else ""
  paste(RVA, suffix)
}

formatFullSigmaRange <- function (sigma_range, name)
{
  sigma_str<-""
  if (length(sigma_range[,1])>0)
  {
  sigma_min <- formatDateAdBc(round(min(sigma_range[,1])))
  sigma_max <- formatDateAdBc(round(max(sigma_range[,2])))
  sigma_str <- sprintf("%11s: %s - %s",name, sigma_min,sigma_max)
  }
  sigma_str
}
