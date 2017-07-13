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
  options(oxcAAR.oxcal_path=path)
  message("Oxcal path set!")
}

#' Quick OxCal setup
#'
#' Downloads the latest version of Oxcal and sets the executable path correctly
#'
#'@param os The operating system of the workstation. Default: automatic determination. Options:
#' \itemize{
#'   \item{\bold{Linux}}
#'   \item{Windows}
#'   \item{Darwin}
#' }
#' @param path The path to the directory where Oxcal should be stored. Default: "."
#'
#' @author Clemens Schmid
#'
#' @examples
#' \dontrun{
#'   quickSetupOxcal()
#' }
#'
#' @export
#'

quickSetupOxcal <- function(os = Sys.info()["sysname"], path = "."){

  # download and unzip OxCal folder
  downloadOxcal(path = path)

  # parse path string depending on os
  os_exe <- switch(
    os,
    Linux = "OxCalLinux",
    Windows = "OxCalWin.exe",
    Darwin = "OxCalMac"
  )
  exe <- file.path(path, "OxCal/bin", os_exe)

  # change permissions to allow execution
  Sys.chmod(exe, mode = "0777")

  # set path
  setOxcalExecutablePath(exe)
}


## ---------- private ----------
downloadOxcal <- function(path = ".") {
  temp <- tempfile()
  utils::download.file("https://c14.arch.ox.ac.uk/OxCalDistribution.zip", temp)
  utils::unzip(temp, exdir = path)
  unlink(temp)
  message("Oxcal download to ", normalizePath(path), " successful!")
}

getOxcalExecutablePath <- function() {
  oxcal_path <- getOption("oxcAAR.oxcal_path")
  if (is.null(oxcal_path) || oxcal_path == "") {
    stop("Please set path to oxcal first (using 'setOxcalExecutablePath')!")
  }
  oxcal_path
}

formatDateAdBc <- function (value_to_print) {
  RVA <- abs(value_to_print)
  suffix <- if (value_to_print < 0) "BC" else if (value_to_print > 0) "AD" else ""
  paste(RVA, suffix)
}

formatFullSigmaRange <- function (sigma_range, name) {
  sigma_str <- ""
  if (length(sigma_range[,1]) > 0) {
  sigma_min <- formatDateAdBc(round(min(sigma_range[,1])))
  sigma_max <- formatDateAdBc(round(max(sigma_range[,2])))
  sigma_str <- sprintf("%11s: %s - %s",name, sigma_min,sigma_max)
  }
  sigma_str
}
