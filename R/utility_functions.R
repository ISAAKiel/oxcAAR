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
#' @param path The path to the directory where Oxcal is or should be stored. Default: "tempdir()".
#' I recommend thought to install it permanently.
#'
#' @return NULL
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

quickSetupOxcal <- function(os = Sys.info()["sysname"], path = tempdir()){

  # test if Oxcal is already setup correctly
  if (!("try-error" %in% class(try(suppressWarnings(oxcalCalibrate(5000, 25, "testdate")), silent = TRUE)))) {
    message("Oxcal is already installed correctly.")
    return()
  }

  # parse path string depending on os
  os_exe <- switch(
    os,
    Linux = "OxCalLinux",
    Windows = "OxCalWin.exe",
    Darwin = "OxCalMac"
  )
  exe <- file.path(path, "OxCal/bin", os_exe)

  # test if Oxcal folder is already present and only the path has to be set
  if (file.exists(exe)) {
    message("Oxcal is installed but Oxcal executable path is wrong. Let's have a look...")
    setOxcalExecutablePath(exe)
    return()
  }

  # download and unzip OxCal folder
  message("Oxcal doesn't seem to be installed. Downloading it now:")
  downloadOxcal(path = path)

  # change permissions to allow execution
  Sys.chmod(exe, mode = "0777")

  # set path
  setOxcalExecutablePath(exe)

  return()
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
  sigma_str <- character(length = nrow(sigma_range))
  if (length(sigma_range[,1]) > 0) {
    sigma_str <- apply(sigma_range,1,
                       function(x) sprintf("%s - %s (%s%%)",
                                           formatDateAdBc(round(x[1])),
                                           formatDateAdBc(round(x[2])),
                                           x[3]))
  sigma_str <- paste(sigma_str, collapse="\n")
  sigma_str <- paste(name,sigma_str, sep = "\n")
  }
  return(sigma_str)
}

side_by_side_output <- function(left, right) {
  RVA <- ""
  left_vec_tmp <- unlist(strsplit(left,"\n"))
  right_vec_tmp <- unlist(strsplit(right,"\n"))

  lines <- max(length(left_vec_tmp),length(right_vec_tmp))

  left_vec <- right_vec <- rep("",times = lines)

  if(length(left_vec_tmp)>0){
  left_vec[1:length(left_vec_tmp)] <- left_vec_tmp
  }
  if(length(right_vec_tmp)>0){
  right_vec[1:length(right_vec_tmp)] <- right_vec_tmp
  }
  if(lines>0){
  for(i in 1:lines){
    RVA <- paste(RVA,sprintf("%s %s",
                             paste(left_vec[i],strrep(" ", 30-nchar(left_vec[i])),sep = ""),
                             right_vec[i]),sep="\n")
  }
  }
  return(RVA)
}
