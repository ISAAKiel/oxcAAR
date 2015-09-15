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
#' R_Simulate takes names, calender dates and standard deviation for those dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param c_dates a vector containing the calender dates that should be simulated
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
      na.omit(
        stringr::str_match(result, "^(ocd\\[\\d+\\])\\.op=\"R_Date\";")[, 2]
      )
    )
  } else {
    date_internal_names <- unique(
      as.vector(
        na.omit(
          stringr::str_match(result, "^(ocd\\[\\d+\\]).*;$")[, 2]
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
    date_text <- na.omit(stringr::str_match(result, reg_string))[, 1]
    this_name <- extractNameFromOxcalResult(date_text)
    this_bp <- extractBpFromOxcalResult(date_text)
    this_std <- extractStdFromOxcalResult(date_text)
    this_probs <- extractProbsFromOxcalResult(date_text)
    this_sigma_ranges <- extractSigmaRangesFromOxcalResult(date_text)

    RVA <- roxcalCalibratedDate(name = this_name,
                                bp=this_bp,
                                std=this_std,
                                cal_curve = this_cal_curve,
                                sigma_ranges = this_sigma_ranges,
                                raw_probabilities = this_probs
    )
    RVA
  })
  names(RVA) <- sapply(RVA,function(x) x$name)
  class(RVA) <- append(class(RVA),"roxcalCalibratedDatesList")
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
  eval(parse(text = paste("out<-", namestolist(output_names))))

  ipar <- out
  initial.param <- as.relistable(ipar)
  ul <- unlist(initial.param)

  output_names_joined <- sapply(output_names, paste, collapse = ".")

  for (i in 1:length(output_names_joined)) {
    actual_name <- output_names_joined[i]
    actual_value <- output.values[output_names_joined == actual_name]
    if (actual_name %in% names(ul)) {
      ul[actual_name] <- actual_value[length(actual_value)]
    }
  }

  rel <- relist(ul, out)
  recursivelyPartialUnlist(rel)
}

## ---------- private ----------

extractProbsFromOxcalResult <- function(result_text) {
  regexp <- "(ocd\\[\\d+\\].likelihood.prob=\\[)(.*)(\\];)"
  probs <- as.double(
    na.omit(
      unlist(
        strsplit(
          stringr::str_match(result_text, regexp)[, 3], ", ")
      )
    )
  )

  regexp <- "(ocd\\[\\d+\\].likelihood.start=)(.*)(;)"
  probs_start <- as.double(
    na.omit(
      unlist(
        strsplit(
          stringr::str_match(result_text, regexp)[, 3], ", ")
      )
    )
  )

  regexp <- "(ocd\\[\\d+\\].likelihood.resolution=)(.*)(;)"
  probs_resolution <- as.double(
    na.omit(
      unlist(
        strsplit(
          stringr::str_match(result_text, regexp)[, 3], ", ")
      )
    )
  )

  dates <- seq(probs_start, by = probs_resolution, length.out = length(probs))

  RVAL <- data.frame(dates = dates, probabilities = probs / probs_resolution)
  RVAL
}


extractSigmaRangesFromOxcalResult <- function(result_text) {
  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[1\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- matrix(
    as.double(
      na.omit(
        unlist(
          strsplit(
            stringr::str_match(result_text, regexp)[, 4], ", ")
        )
      )
    ), ncol = 3,
    byrow = T)
  one_sigma <- data.frame(start = sigma_extract[, 1],
                          end = sigma_extract[, 2],
                          probability = sigma_extract[, 3])

  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[2\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- matrix(
    as.double(
      na.omit(
        unlist(
          strsplit(
            stringr::str_match(result_text, regexp)[, 4], ", ")
        )
      )
    ), ncol = 3,
    byrow = T)
  two_sigma <- data.frame(start = sigma_extract[, 1],
                          end = sigma_extract[, 2],
                          probability = sigma_extract[, 3])

  regexp <- "(ocd\\[\\d+\\].likelihood.range\\[3\\]).*?(=\\[)(.*)(\\];)"
  sigma_extract <- matrix(
    as.double(
      na.omit(
        unlist(
          strsplit(
            stringr::str_match(result_text, regexp)[, 4], ", ")
        )
      )
    ), ncol = 3,
    byrow = T)
  three_sigma <- data.frame(start = sigma_extract[, 1],
                            end = sigma_extract[, 2],
                            probability = sigma_extract[, 3])

  RVAL <- list(one_sigma = one_sigma,
               two_sigma = two_sigma,
               three_sigma = three_sigma)
  RVAL
}

extractCalCurveFromOxcalResult <- function(date_text){
  regexp <- "calib\\[0\\].ref=\"(.*)\";"
  RVA <- as.character(
    na.omit(
      stringr::str_match(date_text, regexp)[, 2], ", ")
  )
  RVA
}

namestolist <- function(x) {
  if (length(x) == 0) {
    return("NA")
  } else {
    this_level <- na.omit(unique(sapply(x, `[`, 1)))
    collector <- vector()
    for (i in 1:length(this_level)) {
      this_element <- this_level[i]
      this_branch <- na.omit(
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
  regexp <- "(ocd\\[\\d+\\]\\.name=\")(.*)(\";)"
  my_name <- as.vector(
    na.omit(
      unlist(
        strsplit(
          stringr::str_match(date_text, regexp)[, 3], ", ")
      )
    )
  )
  my_name
}

extractBpFromOxcalResult <- function(date_text){
  regexp <- "(ocd\\[\\d+\\]\\.date=)(.*)(;)"
  my_date <- NA
  my_result <- stringr::str_match(date_text, regexp)
  if (length(my_result) > 0){
    my_date <- as.integer(na.omit(
      unlist(
        strsplit(
          my_result[, 3], ", ")
      )
    )
    )
  }
  my_date
}
extractStdFromOxcalResult <- function(date_text){
  regexp <- "(ocd\\[\\d+\\]\\.error=)(.*)(;)"
  my_error <- NA
  if (length(my_error) > 0){
    my_error <- as.integer(na.omit(
      unlist(
        strsplit(
          stringr::str_match(date_text, regexp)[, 3], ", ")
      )
    )
    )
  }
  my_error
}
