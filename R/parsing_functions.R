## ---------- public ----------

# ---------- R to Oxcal ----------

#' Returns the Oxcal code for the calibration of 14C dates
#'
#' R_Date takes names, BP dates and standard deviation for those dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' For details concerning the Oxcal calibration please consult the help page of Oxcal.
#'
#' @param names a vector of names for the dates
#' @param r_dates a vector containing the BP dates that should be calibrated
#' @param stds a vector containing the standard deviation that should be calibrated
#'
#' @return a string containing the respective Oxcal code
#' @export

R_Date <- function(names, r_dates, stds) {
  if ((length(r_dates) != length(stds)) && (length(stds) != 1))
    stop("'r_dates' and 'stds' must have the same length")
  if (!is.numeric(r_dates))
    stop("'r_dates' must be a numeric vector")
  if (!is.numeric(stds))
    stop("'stds' must be a numeric vector")
  paste("R_Date(\"",
        names,
        "\", ",
        r_dates,
        ", ",
        stds,
        ");",
        collapse = "\n",
        sep = "")
}

#' Returns the Oxcal code for the simulation of 14C dates
#'
#' R_Simulate takes names, calendar dates and standard deviation for those dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param c_dates a vector containing the calendar dates that should be simulated
#' @param stds a vector containing the standard deviation that should be simulated
#' @param names a vector of names for the resulting simulated dates
#'
#' @return a string containing the respective Oxcal code
#' @export

R_Simulate <- function(c_dates, stds, names = 1:length(c_dates)) {
  if ((length(c_dates) != length(stds)) && (length(stds) != 1))
    stop("'c_dates' and 'stds' must have
             the same length or 'stds' must be an integer")
  if (!is.numeric(c_dates))
    stop("'c_dates' must be a numeric vector")
  if (!is.numeric(stds))
    stop("'stds' must be a numeric vector")
  paste(
    "R_Simulate(\"",
    names,
    "\",
          ",
    c_dates,
    ", ",
    stds,
    ");",
    collapse = "\n",
    sep = ""
  )
}

#' Wraps an Oxcal string into a Oxcal sum function
#'
#' @param oxcal_string The Oxcal script that should be wrapped
#' @param name The name attribute for the resulting sum function
#'
#' @return A new oxcal script as string
#' @export


oxcal_Sum <- function(oxcal_string, name = "Sum") {
  paste("Sum(\"", name, "\"){\n", oxcal_string, "\n};")
}

#' Returns the Oxcal code for Phase
#'
#' Phase takes a set of R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' In this code the R_Dates are encapsuled in an OxCal Phases, one Phase for each string.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param r_dates_strings a vector containing strings of OxCal code, usually consisting of R_Date commands, but any other code strings might be used that can be interpreted by OxCal within a Phase
#' @param names a optional vector of names for the resulting Phases
#'
#' @return a string containing the respective Oxcal code
#' @export
#'
Phase <- function(r_dates_strings, names='') {
  paste("Phase(\"", names, "\"){\n",r_dates_strings,"};", sep = "")
}

#' Returns the Oxcal code for a Boundary
#'
#' Boundary returns the OxCal code for a Boundary.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param names a optional vector of names for the resulting Phases dates. If given, for each name a boundary is returned. If not given, one Boundary without name is returned.
#'
#' @return a string containing the respective Oxcal code
#' @export
#'
Boundary <- function(names) {
  paste("Boundary(\"", names, "\");", sep = "")
}

#' Returns the Oxcal code for Sequence
#'
#' Sequence takes a set of Phases or R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into OxCal.
#' In this code the Phases and/or R_Dates are encapsuled in an OxCal Phases, one Phase for each string.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param sequence_elements a vector containing strings of OxCal code, usually consisting of Phase or R_Date commands, but any other code strings might be used that can be interpreted by OxCal within a Sequence
#' @param names a optional vector of names for the resulting Sequences
#'
#' @return a string containing the respective Oxcal code
#' @export
#'
Sequence <- function(sequence_elements, names='') {
  paste("Sequence(\"", names, "\")\n{", paste(sequence_elements,collapse="\n"), "};", sep="")
}

#' Wrap OxCal commands in Boundary commands
#'
#' wrap_in_boundaries takes a set of Phases or R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into OxCal.
#' In this code the Phases and/or R_Dates are interleaved and wraped in OxCal Boundaries, the number of Boundaries is equal to the number of strings + 1.
#' The resulting string starts with a boundary, than the OxCal strings from the vector are interleaved with Boundary commands.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param phases_strings a vector containing strings of OxCal code, usually consisting of Phase or R_Date commands, but any other code strings might be used that can be interpreted by OxCal inbetween a Boundary
#' @param boundary_names a optional vector of names for the resulting Boundaries (length of phases_strings + 1). If not given, the boundaries are named with consecutive numbers.
#'
#' @return a string containing the respective Oxcal code
#' @importFrom   utils tail
#' @export
#'
wrap_in_boundaries <- function(phases_strings, boundary_names=NA) {
  n_phases <- length(phases_strings)
  if(length(boundary_names)==1) {
    if(is.na(boundary_names)){
      boundary_names <- 1:n_phases + 1
    } else {
      boundary_names <- rep(boundary_names,n_phases + 1)
    }
  }
  my_result <- character(n_phases*2+1)
  for(i in 1:n_phases) {
    my_result[2*i-1] <- Boundary(boundary_names[i])
    my_result[2*i] <- phases_strings[i]
  }
  my_result[length(my_result)] <- Boundary(tail(boundary_names, n=1))
  return(my_result)
}

# ---------- Oxcal to R ----------

#' Parses an Oxcal Output File into R
#'
#' Takes the output of Oxcal as vector of strings (one string per line) and parse it as list.
#'
#' @param result The output of Oxcal as vector of strings (one string per line).
#' @param first Return the first date only
#' @param only.R_Date Return the informations for R_Dates
#' @return A list containing all informations provided by Oxcal as list.
#'
#' @importFrom stringr str_split
#' @importFrom jsonlite fromJSON
#' @export
#'
parseOxcalOutput <- function(result, first=FALSE, only.R_Date=T) {
  this_cal_curve <- extractCalCurveFromOxcalResult(result)

  if (only.R_Date == T){
    date_internal_names <- as.vector(
      stats::na.omit(
        do.call(rbind, stringi::stri_match_all_regex(result, "^(ocd\\[\\d+\\])\\.op=\"R_Date\";"))[, 2]
      )
    )
  } else {
    date_internal_names <- unique(
      as.vector(
        stats::na.omit(
          do.call(rbind, stringi::stri_match_all_regex(result, "^(ocd\\[\\d+\\]).*;$"))[, 2]
        )
      )
    )
  }

  date_internal_names <- date_internal_names[date_internal_names != "ocd[0]"]

  if (first == T){
    numeric_indices <- as.numeric(gsub("ocd\\[(\\d*)\\]$", "\\1",
                                       date_internal_names))
    select_vector <- numeric_indices == min(
      numeric_indices[numeric_indices != 0])
    date_internal_names <- date_internal_names[select_vector]
  }

  date_internal_names <- gsub("\\]", "\\\\\\]", date_internal_names)
  date_internal_names <- gsub("\\[", "\\\\\\[", date_internal_names)

  RVA <- lapply(1:length(date_internal_names), function(i) {
    reg_string <- paste("^(", date_internal_names[i], "\\..*)", sep = "")
    date_text <- stats::na.omit(do.call(rbind, stringi::stri_match_all_regex(result, reg_string)))[, 1]
    this_name <- extractNameFromOxcalResult(date_text)
    this_type <- extractTypeFromOxcalResult(date_text)
    this_bp <- extractBpFromOxcalResult(date_text)
    this_std <- extractStdFromOxcalResult(date_text)
    this_probs <- extractProbsFromOxcalResult(date_text)
    this_posterior_probs <- extractPosteriorProbsFromOxcalResult(date_text)
    this_sigma_ranges <- extractSigmaRangesFromOxcalResult(date_text)
    this_posterior_sigma_ranges <- extractPosteriorSigmaRangesFromOxcalResult(date_text)

    RVA <- oxcAARCalibratedDate(name = this_name,
                                type = this_type,
                                bp=this_bp,
                                std=this_std,
                                cal_curve = this_cal_curve,
                                sigma_ranges = this_sigma_ranges,
                                raw_probabilities = this_probs,
                                posterior_sigma_ranges = this_posterior_sigma_ranges,
                                posterior_probabilities = this_posterior_probs
    )
    RVA
  })
  names(RVA) <- sapply(RVA,function(x) x$name)
  class(RVA) <- append(class(RVA),"oxcAARCalibratedDatesList")
  RVA
}

#' Parses an Oxcal Output File completely into R
#'
#' Takes the output of Oxcal as vector of strings (one string per line) and parse it as list.
#'
#' @param output The output of Oxcal as vector of strings (one string per line).
#' @return A list containing all informations provided by Oxcal as list.
#'
#' @importFrom stringr str_split
#' @importFrom jsonlite fromJSON
#' @export

parseFullOxcalOutput <- function(output) {
  output <- gsub("if\\((.*)$", "", output)
  output <- gsub("(.*),\\];$", "\\1\\];", output)
  output <- gsub("\\\\'", "\\'", output)
  output <- gsub(";$", "", output)
  output2 <- stringr::str_split(output, "=", n = 2)
  output2 <- output2[!sapply(output2, function(x) all(is.na(x)))]

  output_names <- lapply(output2, function(x) {
    x[1]
  })
  output_names <- lapply(output_names, function(x) {
    unlist(strsplit(x, "\\."))
  })

  output.values <- lapply(output2, function(x) {
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub("(\\bNaN\\b)", "\"NaN\"", x)
    if (!(is.na(x[2]))) {
      if (substr(x[2], 1, 1) == "{") {
        return_value <- jsonlite::fromJSON(
          gsub("(?<!\")'(\\w+)'(?<!\")", "\"\\1\"",
               gsub("(\\w*):", "\"\\1\":", x[2]),perl = T)
        )
      } else {
        return_value <- jsonlite::fromJSON(x[2])
      }
      return_value
    }
  })

  out <- ""
  eval(parse(text = paste("out<-", namestolist(output_names))))

  ipar <- out
  initial.param <- utils::as.relistable(ipar)
  ul <- unlist(initial.param)

  output_names_joined <- sapply(output_names, paste, collapse = ".")

  for (i in 1:length(output_names_joined)) {
    actual_name <- output_names_joined[i]
    actual_value <- output.values[output_names_joined == actual_name]
    if (actual_name %in% names(ul)) {
      ul[actual_name] <- actual_value[length(actual_value)]
    }
  }

  rel <- utils::relist(ul, out)
  recursivelyPartialUnlist(rel)
}

## ---------- private ----------

extractPosteriorProbsFromOxcalResult <- function(result_text) {
  identifier <- "].posterior.prob="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  if(length(this_date_text)==0){return(NA)}
  regexp <- "(ocd\\[\\d+\\].posterior.prob=\\[)(.*)(\\];)"
  probs <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  if(length(probs)==0){return(NA)}

  identifier <- "].posterior.start="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].posterior.start=)(.*)(;)"
  probs_start <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  identifier <- "].posterior.resolution="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].posterior.resolution=)(.*)(;)"
  probs_resolution <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  identifier <- "].posterior.probNorm="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].posterior.probNorm=)(.*)(;)"
  probs_norm <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  if((length(probs_norm)==0) || is.na(probs_norm)) {probs_norm <- 1}

  dates <- seq(probs_start, by = probs_resolution, length.out = length(probs))

  RVAL <- data.frame(dates = dates, probabilities = probs * probs_norm)
  RVAL
}

extractProbsFromOxcalResult <- function(result_text) {
  identifier <- "].likelihood.prob="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].likelihood.prob=\\[)(.*)(\\];)"
  probs <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  if(length(probs)==0){return(NA)}

  identifier <- "].likelihood.start="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].likelihood.start=)(.*)(;)"
  probs_start <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  identifier <- "].likelihood.resolution="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].likelihood.resolution=)(.*)(;)"
  probs_resolution <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  identifier <- "].likelihood.probNorm="
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].likelihood.probNorm=)(.*)(;)"
  probs_norm <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)

  if((length(probs_norm)==0) || is.na(probs_norm)) {probs_norm <- 1}

  dates <- seq(probs_start, by = probs_resolution, length.out = length(probs))

  RVAL <- data.frame(dates = dates, probabilities = probs * probs_norm)
  RVAL
}

extractDoubleFromOxcalResult <- function(result_text, regexp, position) {
  as.double(
    stats::na.omit(
      unlist(
        strsplit(
          do.call(rbind,
                  stringi::stri_match_all_regex(
                    result_text,
                    regexp
                    )
                  )[, position],
          ", ",
          fixed = TRUE
          )
        )
      )
  )
  }

extractPosteriorSigmaRangesFromOxcalResult <- function(result_text) {
  one_sigma <- two_sigma <- three_sigma <- NA
  identifier <- "].posterior.range"
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  regexp <- "(ocd\\[\\d+\\].posterior.range\\[1\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    one_sigma <- data.frame(start = sigma_extract[, 1],
                            end = sigma_extract[, 2],
                            probability = sigma_extract[, 3])
  }

  regexp <- "(ocd\\[\\d+\\].posterior.range\\[2\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    two_sigma <- data.frame(start = sigma_extract[, 1],
                            end = sigma_extract[, 2],
                            probability = sigma_extract[, 3])
  }
  regexp <- "(ocd\\[\\d+\\].posterior.range\\[3\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    three_sigma <- data.frame(start = sigma_extract[, 1],
                              end = sigma_extract[, 2],
                              probability = sigma_extract[, 3])
  }
  RVAL <- list(one_sigma = one_sigma,
               two_sigma = two_sigma,
               three_sigma = three_sigma)
  RVAL
}

extractSigmaValuesFromOxcalResult <- function(result_text,regexp) {
  if(length(result_text)==0) result_text <- ""
  RVA <- matrix(
    as.double(
      stats::na.omit(
        unlist(
          strsplit(
            do.call(rbind, stringi::stri_match_all_regex(result_text, regexp))[, 4], ", ", fixed = TRUE)
        )
      )
    ), ncol = 3,
    byrow = T)
  RVA <- data.frame(RVA)
  RVA <- RVA[rowSums(is.na(RVA))<3,]
  return(RVA)
}

extractSigmaRangesFromOxcalResult <- function(result_text) {
  identifier <- "].likelihood.range"
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  one_sigma <- two_sigma <- three_sigma <- NA
  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[1\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    one_sigma <- data.frame(start = sigma_extract[, 1],
                            end = sigma_extract[, 2],
                            probability = sigma_extract[, 3])
  }
  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[2\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    two_sigma <- data.frame(start = sigma_extract[, 1],
                            end = sigma_extract[, 2],
                            probability = sigma_extract[, 3])
  }
  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[3\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
  if(nrow(na.omit(sigma_extract))>0){
    three_sigma <- data.frame(start = sigma_extract[, 1],
                              end = sigma_extract[, 2],
                              probability = sigma_extract[, 3])
  }

  RVAL <- list(one_sigma = one_sigma,
               two_sigma = two_sigma,
               three_sigma = three_sigma)
  RVAL
}

extractCalCurveFromOxcalResult <- function(date_text){
  identifier <- "calib[0].ref="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_name <- "calib\\[0\\].ref=\"(.*)\";"
  calcurve_name <- as.character(
    stats::na.omit(
      do.call(rbind, stringi::stri_match_all_regex(this_date_text, regexp_calcurve_name))[, 2], ", ")
  )
  identifier <- "calib[0].resolution="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_resolution <- "calib\\[0\\].resolution=(.*);"
  calcurve_resolution <- as.numeric(
    stats::na.omit(
      do.call(rbind, stringi::stri_match_all_regex(this_date_text, regexp_calcurve_resolution))[, 2], ", ")
  )
  identifier <- "calib[0].start="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_start <- "calib\\[0\\].start=(.*);"
  calcurve_start <- as.numeric(
    stats::na.omit(
      do.call(rbind, stringi::stri_match_all_regex(this_date_text, regexp_calcurve_start))[, 2], ", ")
  )
  identifier <- "calib[0].bp="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_bp <- "calib\\[0\\].bp=\\[(.*)\\];"
  calcurve_bp <- as.numeric(
    strsplit(stats::na.omit(
      do.call(rbind, stringi::stri_match_all_regex(this_date_text, regexp_calcurve_bp))[, 2]
    ), ",", fixed = T)[[1]]
  )
  identifier <- "calib[0].sigma="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_sigma <- "calib\\[0\\].sigma=\\[(.*)\\];"
  calcurve_sigma <- as.numeric(
    strsplit(stats::na.omit(
      do.call(rbind, stringi::stri_match_all_regex(this_date_text, regexp_calcurve_sigma))[, 2]
    ), ",", fixed = T)[[1]]
  )
  RVA <- list(name=calcurve_name,
              resolution = calcurve_resolution,
              bp = calcurve_bp,
              bc = seq( from = calcurve_start, by = calcurve_resolution, length.out = length(calcurve_bp) ),
              sigma = calcurve_sigma)
  RVA
}



namestolist <- function(x) {
  if (length(x) == 0) {
    return("NA")
  } else {
    this_level <- stats::na.omit(unique(sapply(x, `[`, 1)))
    collector <- vector()
    for (i in 1:length(this_level)) {
      this_element <- this_level[i]
      this_branch <- stats::na.omit(
        sapply(
          x[sapply(x, `[`, 1) == this_element], `[`, -1
        )
      )
      this_branch <- this_branch[lapply(this_branch, length) > 0]
      collector <- append(collector,
                          paste("`",
                                this_element,
                                "`",
                                " = ",
                                namestolist(this_branch),
                                sep = "")
      )
    }
    return(paste("list(", paste(collector, collapse = ","), ")"))
  }
}

recursivelyPartialUnlist <- function(l) {
  lapply(l,
         function(x) if (is.list(x) && length(x) == 1 &&
                         !(is.list(x[[1]])))
           x[[1]] else if (is.list(x))
             recursivelyPartialUnlist(x) else x)
}

extractNameFromOxcalResult <- function(date_text){
  identifier <- "].name="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.name=\")(.*)(\";)"
  my_name <- as.vector(
    stats::na.omit(
      unlist(
        strsplit(
          do.call(rbind, stringi::stri_match_all_regex(date_text, regexp))[, 3], ", ", fixed = T)
      )
    )
  )
  my_name
}

extractTypeFromOxcalResult <- function(date_text){
  identifier <- "].op="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.op=\")(.*)(\";)"
  my_name <- as.vector(
    stats::na.omit(
      unlist(
        strsplit(
          do.call(rbind, stringi::stri_match_all_regex(date_text, regexp))[, 3], ", ", fixed = T)
      )
    )
  )
  my_name
}

extractBpFromOxcalResult <- function(date_text){
  identifier <- "].date="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.date=)(.*)(;)"
  my_date <- NA
  my_result <- na.omit(do.call(rbind, stringi::stri_match_all_regex(date_text, regexp)))
  if (length(my_result) > 0){
    my_date <- as.integer(stats::na.omit(
      unlist(
        strsplit(
          my_result[, 3], ", ", fixed = T)
      )
    )
    )
  }
  my_date
}

extractStdFromOxcalResult <- function(date_text){
  identifier <- "].error="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.error=)(.*)(;)"
  my_error <- NA
  my_result <- as.integer(stats::na.omit(
    unlist(
      strsplit(
        stringr::str_match(date_text, regexp)[, 3], ", ", fixed = T)
    )
  )
  )
  if (length(my_result) > 0){
    my_error <- my_result
  }
  my_error
}

reduce_to_relevant_lines <- function(date_text, identifier) {
  date_text <- date_text[grepl(identifier, date_text, fixed = T)]
}
