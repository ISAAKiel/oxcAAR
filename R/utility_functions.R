## ---------- public ----------

#' Set the OxCal executable path
#'
#' Stores the path to the OxCal executable for use by other oxcAAR functions.
#'
#' @param path The path to the OxCal executable.
#'
#' @author Martin Hinz
#'
#' @examples
#' \dontrun{
#' setOxcalExecutablePath("/home/martin/Documents/scripte/OxCal/bin/OxCalLinux")
#' }
#'
#' @export
setOxcalExecutablePath <- function(path) {
  if (!file.exists(path))
    stop("No file at given location")
  options(oxcAAR.oxcal_path = path)
  message("OxCal path set!")
}

#' @title Quick OxCal setup
#' @description Downloads OxCal and sets the executable path correctly.
#'
#' @param os The operating system of the workstation. Default: automatic
#'   determination. Options:
#' \itemize{
#'   \item{\bold{Linux}}
#'   \item{Windows}
#'   \item{Darwin}
#' }
#' @param path The path to the directory where OxCal is or should be stored.
#'   Default: \code{tempdir()}. For persistent use, installation in a permanent
#'   location is recommended.
#' @param version The OxCal version to install. Defaults to \code{"latest"}.
#'   Specific versions can be selected for reproducible installations.
#'   See \code{testedOxcalVersions()} for fixed releases explicitly tested
#'   with oxcAAR.
#' @param force Force download and setup even if an existing OxCal
#'   installation is detected.
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
#' @rdname quickSetupOxcal
#' @export
quickSetupOxcal <- function(
    os = Sys.info()["sysname"],
    path = tempdir(),
    version = "latest",
    force = FALSE
) {

  # test if OxCal is already setup correctly
  if (!force) {
    if (!("try-error" %in% class(
      try(
        suppressWarnings(oxcalCalibrate(5000, 25, "testdate")),
        silent = TRUE
      )
    ))) {
      message("A version of OxCal is already installed.")
      return()
    }
  }

  # parse path string depending on os
  os_exe <- switch(
    os,
    Linux = "OxCalLinux",
    Windows = "OxCalWin.exe",
    Darwin = "OxCalMac"
  )
  exe <- file.path(path, "OxCal/bin", os_exe)

  # test if OxCal folder is already present and only the path has to be set
  if (!force) {
    if (file.exists(exe)) {
      message(
        "OxCal is installed but the OxCal executable path is wrong. ",
        "Let's have a look..."
      )
      setOxcalExecutablePath(exe)
      return()
    }
  }

  # download and unzip OxCal folder
  message("Downloading OxCal now:")
  downloadOxcal(path = path, version = version)

  # change permissions to allow execution
  Sys.chmod(exe, mode = "0777")

  # set path
  test <- tryCatch(
    setOxcalExecutablePath(exe),
    error = function(e) {
      message("The OxCal executable path could not be set:")
      message(e)
      message(
        "\nIf you received an internet connection error before, ",
        "please resolve it and try again later"
      )
    }
  )

  if (!is.null(test) && test == 0) {
    message("OxCal setup successful!")
  }

  return()
}

#' OxCal versions tested with oxcAAR
#'
#' Returns the fixed OxCal releases that have been explicitly tested for
#' compatibility with oxcAAR. The special version \code{"latest"} is not
#' included because it refers to the current OxCal distribution and may
#' change independently of oxcAAR.
#'
#' @return A character vector of tested OxCal versions.
#'
#' @export
testedOxcalVersions <- function() {
  c(
    "4.4.4",
    "4.4.3",
    "4.4.2",
    "4.4.1",
    "4.3.2",
    "4.3.1",
    "4.2.4"
  )
}


.oxcal_download_url <- function(version) {
  if (
    !is.character(version) ||
    length(version) != 1L ||
    is.na(version) ||
    !nzchar(version)
  ) {
    stop(
      "'version' must be a single non-empty character string.",
      call. = FALSE
    )
  }

  if (identical(version, "latest")) {
    return("https://c14.arch.ox.ac.uk/OxCalDistribution.zip")
  }

  # OxCal 4.3.2 is stored under an exceptional archive name by Oxford.
  if (identical(version, "4.3.2")) {
    return("https://c14.arch.ox.ac.uk/OxCal_4_3_2_orig.zip")
  }

  paste0(
    "https://c14.arch.ox.ac.uk/OxCal_",
    gsub(".", "_", version, fixed = TRUE),
    ".zip"
  )
}


#' Download OxCal
#'
#' Downloads and extracts an OxCal distribution.
#'
#' @param path The directory in which OxCal should be stored.
#' @param version The OxCal version to download. Defaults to \code{"latest"}.
#'   See \code{testedOxcalVersions()} for fixed releases explicitly tested
#'   with oxcAAR. Other fixed versions can be requested but will generate a
#'   warning because their compatibility with oxcAAR has not been verified.
#'
#' @return NULL
#'
#' @export
downloadOxcal <- function(path = ".", version = "latest") {
  url <- .oxcal_download_url(version)

  if (
    !identical(version, "latest") &&
    !(version %in% testedOxcalVersions())
  ) {
    warning(
      "OxCal version ", version,
      " has not been tested with oxcAAR.",
      call. = FALSE
    )
  }

  temp <- tempfile()

  test <- tryCatch(
    utils::download.file(url, temp),
    warning = function(e) {
      message("Error downloading OxCal:")
      message(e)
      message(
        "\nNo internet connection, data source broken, ",
        "or requested version unavailable?"
      )
    }
  )

  if (!is.null(test) && test == 0) {
    utils::unzip(temp, exdir = path)
    unlink(temp)
    message("OxCal stored successfully at ", normalizePath(path), "!")
  }

  return()
}

## ---------- private ----------
getOxcalExecutablePath <- function() {
  oxcal_path <- getOption("oxcAAR.oxcal_path")
  if (is.null(oxcal_path) || oxcal_path == "") {
    stop("Please set path to oxcal first (using 'setOxcalExecutablePath')!")
  }
  oxcal_path
}

formatDateAdBc <- function (value_to_print) {
  RVA <- abs(value_to_print)
  suffix <- ""
  if (!is.na(value_to_print)) {
    suffix <- if (value_to_print < 0) " BC" else if (value_to_print > 0) " AD"
  }
  paste0(RVA, suffix)
}

formatFullSigmaRange <- function (sigma_range, name) {
  if(all(is.na(sigma_range))) return(NA)
  sigma_str <- character(length = nrow(sigma_range))
  if (length(sigma_range[,1]) > 0) {
    sigma_str <- apply(sigma_range,1,
                       function(x) sprintf("%s - %s (%s%%)",
                                           formatDateAdBc(round(x[1])),
                                           formatDateAdBc(round(x[2])),
                                           round(x[3], 2)))
    sigma_str <- paste(sigma_str, collapse="\n")
    sigma_str <- paste(name,sigma_str, sep = "\n")
  }
  return(sigma_str)
}

side_by_side_output <- function(left, right) {
  RVA <- ""
  if(is.na(left)) {left <- "NA"}
  if(is.na(right)) {right <- "NA"}
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
