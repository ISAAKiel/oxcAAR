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
#' @param stds a vector containing the standard deviation that should be calibrated (length 1 or same length as r_dates)
#'
#' @return a string containing the respective Oxcal code
#' @export
R_Date <- function(names, r_dates, stds) {
  names <- .as_character(names, "names")
  .assert_numeric(r_dates, "r_dates")
  stds <- .recycle_or_fail(stds, target_length = length(r_dates), arg_name = "stds", target_name = "r_dates")

  if (length(names) != length(r_dates)) {
    stop("'names' and 'r_dates' must have the same length", call. = FALSE)
  }

  paste(sprintf('R_Date("%s", %s, %s);', names, r_dates, stds), collapse = "\n")
}

#' Returns the Oxcal code for the simulation of 14C dates
#'
#' R_Simulate takes names, calendar dates and standard deviation for those dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param c_dates a vector containing the calendar dates that should be simulated
#' @param stds a vector containing the standard deviation that should be simulated (length 1 or same length as c_dates)
#' @param names a vector of names for the resulting simulated dates
#'
#' @return a string containing the respective Oxcal code
#' @export
R_Simulate <- function(c_dates, stds, names = seq_along(c_dates)) {
  .assert_numeric(c_dates, "c_dates")
  names <- .as_character(names, "names")
  stds <- .recycle_or_fail(stds, target_length = length(c_dates), arg_name = "stds", target_name = "c_dates")

  if (length(names) != length(c_dates)) {
    stop("'names' and 'c_dates' must have the same length", call. = FALSE)
  }

  paste(sprintf('R_Simulate("%s",\n          %s, %s);', names, c_dates, stds), collapse = "\n")
}

#' Wraps an Oxcal string into a Oxcal sum function
#'
#' @param oxcal_string The Oxcal script that should be wrapped (vector or single string)
#' @param name The name attribute for the resulting sum function
#'
#' @return A new oxcal script as string
#' @export
oxcal_Sum <- function(oxcal_string, name = " Sum ") {

  name <- .as_character(name, "name")
  oxcal_string <- paste(oxcal_string, collapse = "\n")
  sprintf('Sum("%s"){\n %s \n};', name, oxcal_string)
}

#' Returns the Oxcal code for Phase
#'
#' Phase takes a set of R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into oxcal.
#' In this code the R_Dates are encapsulated in an OxCal Phases, one Phase for each string.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param r_dates_strings a vector containing strings of OxCal code, usually consisting of R_Date commands, but any other code strings might be used that can be interpreted by OxCal within a Phase
#' @param names a optional vector of names for the resulting Phases
#'
#' @return a string containing the respective Oxcal code
#' @export
Phase <- function(r_dates_strings, names = "") {
  names <- .as_character(names, "names")
  r_dates_strings <- paste(r_dates_strings, collapse = "\n")
  sprintf('Phase("%s"){\n%s\n};', names, r_dates_strings)
}

#' Returns the Oxcal code for a Boundary
#'
#' Boundary returns the OxCal code for a Boundary.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param names a optional vector of names for the resulting boundary. If missing or empty, an unnamed boundary is returned.
#'
#' @return a string containing the respective Oxcal code
#' @export
Boundary <- function(names = "") {
  if (missing(names) || is.null(names) || length(names) == 0) names <- ""
  names <- .as_character(names, "names")
  paste(sprintf('Boundary("%s");', names), collapse = "\n")
}

#' Returns the Oxcal code for Sequence
#'
#' Sequence takes a set of Phases or R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into OxCal.
#' In this code the Phases and/or R_Dates are encapsulated in an OxCal Sequence.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param sequence_elements a vector containing strings of OxCal code, usually consisting of Phase or R_Date commands, but any other code strings might be used that can be interpreted by OxCal within a Sequence
#' @param names a optional vector of names for the resulting Sequences
#'
#' @return a string containing the respective Oxcal code
#' @export
Sequence <- function(sequence_elements, names = "") {
  names <- .as_character(names, "names")
  sequence_elements <- paste(sequence_elements, collapse = "\n")
  sprintf('Sequence("%s"){\n%s\n};', names, sequence_elements)
}

#' Wrap OxCal commands in Boundary commands
#'
#' wrap_in_boundaries takes a set of Phases or R_Dates as vectors, and returns
#' a bit of oxcal code that can be used to feed it into OxCal.
#' In this code the Phases and/or R_Dates are interleaved and wrapped in OxCal Boundaries, the number of Boundaries is equal to the number of strings + 1.
#' The resulting string starts with a boundary, than the OxCal strings from the vector are interleaved with Boundary commands.
#' For details concerning the Oxcal simulation please consult the help page of Oxcal.
#'
#' @param phases_strings a vector containing strings of OxCal code, usually consisting of Phase or R_Date commands, but any other code strings might be used that can be interpreted by OxCal inbetween a Boundary
#' @param boundary_names a optional vector of names for the resulting Boundaries (length of phases_strings + 1). If not given, the boundaries are named with consecutive numbers.
#' @param collapse if TRUE, return a single string; if FALSE (default), return a character vector (backwards compatible)
#'
#' @return OxCal code (character vector or single string depending on collapse)
#' @importFrom utils tail
#' @export
wrap_in_boundaries <- function(phases_strings, boundary_names = NA, collapse = FALSE) {
  phases_strings <- .as_character(phases_strings, "phases_strings")
  n_phases <- length(phases_strings)

  if (length(boundary_names) == 1 && isTRUE(is.na(boundary_names))) {
    boundary_names <- seq_len(n_phases + 1)
  } else if (length(boundary_names) == 1) {
    boundary_names <- rep(boundary_names, n_phases + 1)
  } else if (length(boundary_names) != (n_phases + 1)) {
    stop("'boundary_names' must have length 1 or length(phases_strings) + 1", call. = FALSE)
  }
  boundary_names <- .as_character(boundary_names, "boundary_names")

  out <- character(n_phases * 2 + 1)
  for (i in seq_len(n_phases)) {
    out[2 * i - 1] <- Boundary(boundary_names[i])
    out[2 * i]     <- phases_strings[i]
  }
  out[length(out)] <- Boundary(utils::tail(boundary_names, n = 1))

  if (isTRUE(collapse)) return(paste(out, collapse = "\n"))
  out
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
parseOxcalOutput <- function(result, first = FALSE, only.R_Date = TRUE) {
  result <- .as_character(result, "result")

  this_cal_curve <- extractCalCurveFromOxcalResult(result)

  if (isTRUE(only.R_Date)) {
    date_internal_names <- .regex_capture_all(result, "^(ocd\\[\\d+\\])\\.op=\"R_Date\";", group = 2)
  } else {
    date_internal_names <- unique(.regex_capture_all(result, "^(ocd\\[\\d+\\]).*;$", group = 2))
  }

  date_internal_names <- stats::na.omit(date_internal_names)
  date_internal_names <- date_internal_names[date_internal_names != "ocd[0]"]

  if (length(date_internal_names) == 0) {
    RVA <- list()
    class(RVA) <- append(class(RVA), "oxcAARCalibratedDatesList")
    return(RVA)
  }

  if (isTRUE(first)) {
    numeric_indices <- suppressWarnings(as.numeric(gsub("ocd\\[(\\d*)\\]$", "\\1", date_internal_names)))
    numeric_indices <- numeric_indices[!is.na(numeric_indices) & numeric_indices != 0]
    if (length(numeric_indices) > 0) {
      min_idx <- min(numeric_indices)
      date_internal_names <- date_internal_names[grepl(sprintf("^ocd\\[%s\\]$", min_idx), date_internal_names)]
    }
  }

  # escape for regex use
  date_internal_names <- gsub("\\]", "\\\\\\]", date_internal_names)
  date_internal_names <- gsub("\\[", "\\\\\\[", date_internal_names)

  RVA <- lapply(seq_along(date_internal_names), function(i) {
    reg_string <- paste0("^(", date_internal_names[i], "\\..*)")
    date_text  <- .regex_capture_all(result, reg_string, group = 1)
    date_text  <- stats::na.omit(date_text)

    this_name <- extractNameFromOxcalResult(date_text)
    this_type <- extractTypeFromOxcalResult(date_text)
    this_bp   <- extractBpFromOxcalResult(date_text)
    this_std  <- extractStdFromOxcalResult(date_text)

    this_probs                 <- extractProbsFromOxcalResult(date_text)
    this_posterior_probs        <- extractPosteriorProbsFromOxcalResult(date_text)
    this_sigma_ranges           <- extractSigmaRangesFromOxcalResult(date_text)
    this_posterior_sigma_ranges <- extractPosteriorSigmaRangesFromOxcalResult(date_text)

    oxcAARCalibratedDate(
      name                    = this_name,
      type                    = this_type,
      bp                      = this_bp,
      std                     = this_std,
      cal_curve               = this_cal_curve,
      sigma_ranges            = this_sigma_ranges,
      raw_probabilities       = this_probs,
      posterior_sigma_ranges  = this_posterior_sigma_ranges,
      posterior_probabilities = this_posterior_probs
    )
  })

  names(RVA) <- vapply(RVA, function(x) x$name, character(1))
  class(RVA) <- append(class(RVA), "oxcAARCalibratedDatesList")
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
  output <- .as_character(output, "output")

  output <- gsub("if\\((.*)$", "", output)
  output <- gsub("(.*),\\];$", "\\1\\];", output)
  output <- gsub("\\\\'", "\\'", output)
  output <- gsub(";$", "", output)

  output2 <- stringr::str_split(output, "=", n = 2)
  output2 <- output2[!vapply(output2, function(x) all(is.na(x)), logical(1))]

  output_names <- lapply(output2, function(x) x[1])
  output_names <- lapply(output_names, function(x) unlist(strsplit(x, "\\.")))

  output.values <- lapply(output2, function(x) {
    x <- gsub("^\\s+|\\s+$", "", x)
    x <- gsub("(\\bNaN\\b)", "\"NaN\"", x)
    if (!is.na(x[2])) {
      if (substr(x[2], 1, 1) == "{") {
        jsonlite::fromJSON(
          gsub("(?<!\")'(\\w+)'(?<!\")", "\"\\1\"",
               gsub("(\\w*):", "\"\\1\":", x[2]),
               perl = TRUE
          )
        )
      } else {
        jsonlite::fromJSON(x[2])
      }
    } else {
      NULL
    }
  })

  out <- ""
  eval(parse(text = paste0("out<-", namestolist(output_names))))

  ipar <- out
  initial.param <- utils::as.relistable(ipar)
  ul <- unlist(initial.param)

  output_names_joined <- vapply(output_names, paste, character(1), collapse = ".")

  for (i in seq_along(output_names_joined)) {
    actual_name <- output_names_joined[i]
    if (!(actual_name %in% names(ul))) next

    val <- output.values[[i]]
    if (is.null(val) || length(val) == 0) next

    if (is.list(val)) {
      val <- unlist(val, recursive = TRUE, use.names = FALSE)
    }

    if (length(val) == 0) next
    if (length(val) > 1) val <- val[length(val)]  # keep last value, like your earlier "take last" logic

    ul[actual_name] <- val
  }

  rel <- utils::relist(ul, out)
  recursivelyPartialUnlist(rel)
}

## ---------- private ----------

# ---- small assertions/helpers ----

.assert_numeric <- function(x, arg_name) {
  if (!is.numeric(x)) stop(sprintf("'%s' must be a numeric vector", arg_name), call. = FALSE)
  invisible(TRUE)
}

.as_character <- function(x, arg_name) {
  if (is.null(x)) stop(sprintf("'%s' must not be NULL", arg_name), call. = FALSE)
  if (is.factor(x)) x <- as.character(x)
  if (!is.character(x)) x <- as.character(x)
  x
}

.recycle_or_fail <- function(x, target_length, arg_name, target_name) {
  if (!is.numeric(x)) stop(sprintf("'%s' must be a numeric vector", arg_name), call. = FALSE)
  if (length(x) == 1) return(rep(x, target_length))
  if (length(x) != target_length) {
    stop(sprintf("'%s' must have length 1 or the same length as '%s'", arg_name, target_name), call. = FALSE)
  }
  x
}

.regex_capture_all <- function(text, pattern, group = 1) {
  # Returns character vector of captures, safely empty if no matches
  m <- stringi::stri_match_all_regex(text, pattern)
  if (length(m) == 0) return(character(0))
  m <- do.call(rbind, m)
  if (is.null(m) || nrow(m) == 0) return(character(0))
  out <- m[, group]
  out[!is.na(out)]
}

# ---- extractors ----

extractPosteriorProbsFromOxcalResult <- function(result_text) {
  .extract_prob_block(result_text, prefix = "posterior")
}

extractProbsFromOxcalResult <- function(result_text) {
  .extract_prob_block(result_text, prefix = "likelihood")
}

.extract_prob_block <- function(result_text, prefix = c("likelihood", "posterior")) {
  prefix <- match.arg(prefix)
  identifier <- sprintf("].%s.prob=", prefix)
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)
  if (length(this_date_text) == 0) return(NA)

  regexp <- sprintf("(ocd\\[\\d+\\].%s.prob=\\[)(.*)(\\];)", prefix)
  probs <- extractDoubleFromOxcalResult(this_date_text, regexp, 3)
  if (length(probs) == 0) return(NA)

  regexp_start <- sprintf("(ocd\\[\\d+\\].%s.start=)(.*)(;)", prefix)
  probs_start <- extractDoubleFromOxcalResult(
    reduce_to_relevant_lines(result_text, sprintf("].%s.start=", prefix)),
    regexp_start, 3
  )

  regexp_res <- sprintf("(ocd\\[\\d+\\].%s.resolution=)(.*)(;)", prefix)
  probs_resolution <- extractDoubleFromOxcalResult(
    reduce_to_relevant_lines(result_text, sprintf("].%s.resolution=", prefix)),
    regexp_res, 3
  )

  regexp_norm <- sprintf("(ocd\\[\\d+\\].%s.probNorm=)(.*)(;)", prefix)
  probs_norm <- extractDoubleFromOxcalResult(
    reduce_to_relevant_lines(result_text, sprintf("].%s.probNorm=", prefix)),
    regexp_norm, 3
  )

  if (length(probs_norm) == 0 || is.na(probs_norm)) probs_norm <- 1
  if (length(probs_start) == 0 || is.na(probs_start)) return(NA)
  if (length(probs_resolution) == 0 || is.na(probs_resolution)) return(NA)

  dates <- seq(probs_start, by = probs_resolution, length.out = length(probs))
  data.frame(dates = dates, probabilities = probs * probs_norm)
}

extractDoubleFromOxcalResult <- function(result_text, regexp, position) {
  if (length(result_text) == 0) return(numeric(0))

  m <- stringi::stri_match_all_regex(result_text, regexp)
  if (length(m) == 0) return(numeric(0))

  m <- do.call(rbind, m)
  if (is.null(m) || nrow(m) == 0) return(numeric(0))

  raw <- m[, position]
  raw <- raw[!is.na(raw)]
  if (length(raw) == 0) return(numeric(0))

  as.double(stats::na.omit(unlist(strsplit(raw, ", ", fixed = TRUE))))
}

extractPosteriorSigmaRangesFromOxcalResult <- function(result_text) {
  .extract_sigma_ranges(result_text, prefix = "posterior")
}

extractSigmaRangesFromOxcalResult <- function(result_text) {
  .extract_sigma_ranges(result_text, prefix = "likelihood")
}

.extract_sigma_ranges <- function(result_text, prefix = c("likelihood", "posterior")) {
  prefix <- match.arg(prefix)
  identifier <- sprintf("].%s.range", prefix)
  this_date_text <- reduce_to_relevant_lines(result_text, identifier)

  one_sigma <- two_sigma <- three_sigma <- NA

  for (k in 1:3) {
    regexp <- sprintf("(ocd\\[\\d+\\].%s.range\\[%d\\]).*?(=\\[)(.*)(\\];)", prefix, k)
    sigma_extract <- suppressWarnings(extractSigmaValuesFromOxcalResult(this_date_text, regexp))
    if (is.data.frame(sigma_extract) && nrow(stats::na.omit(sigma_extract)) > 0) {
      df <- data.frame(start = sigma_extract[, 1],
                       end = sigma_extract[, 2],
                       probability = sigma_extract[, 3])
      if (k == 1) one_sigma <- df
      if (k == 2) two_sigma <- df
      if (k == 3) three_sigma <- df
    }
  }

  list(one_sigma = one_sigma, two_sigma = two_sigma, three_sigma = three_sigma)
}

extractSigmaValuesFromOxcalResult <- function(result_text, regexp) {
  if (length(result_text) == 0) return(data.frame())

  m <- stringi::stri_match_all_regex(result_text, regexp)
  if (length(m) == 0) return(data.frame())

  m <- do.call(rbind, m)
  if (is.null(m) || nrow(m) == 0) return(data.frame())

  raw <- m[, 4]
  raw <- raw[!is.na(raw)]
  if (length(raw) == 0) return(data.frame())

  vals <- suppressWarnings(as.double(stats::na.omit(unlist(strsplit(raw, ", ", fixed = TRUE)))))
  if (length(vals) == 0) return(data.frame())

  mat <- matrix(vals, ncol = 3, byrow = TRUE)
  # Keep legacy structure (and column names) consistent with data.frame(matrix(...))
  df <- data.frame(mat)
  df <- df[rowSums(is.na(df)) < 3, , drop = FALSE]
  df
}

extractCalCurveFromOxcalResult <- function(date_text) {
  identifier <- "calib[0].ref="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_name <- "calib\\[0\\].ref=\"(.*)\";"
  calcurve_name <- .regex_capture_all(this_date_text, regexp_calcurve_name, group = 2)
  calcurve_name <- if (length(calcurve_name)) calcurve_name[length(calcurve_name)] else NA_character_

  identifier <- "calib[0].resolution="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_resolution <- "calib\\[0\\].resolution=(.*);"
  calcurve_resolution <- .regex_capture_all(this_date_text, regexp_calcurve_resolution, group = 2)
  calcurve_resolution <- if (length(calcurve_resolution)) as.numeric(calcurve_resolution[length(calcurve_resolution)]) else NA_real_

  identifier <- "calib[0].start="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_start <- "calib\\[0\\].start=(.*);"
  calcurve_start <- .regex_capture_all(this_date_text, regexp_calcurve_start, group = 2)
  calcurve_start <- if (length(calcurve_start)) as.numeric(calcurve_start[length(calcurve_start)]) else NA_real_

  identifier <- "calib[0].bp="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_bp <- "calib\\[0\\].bp=\\[(.*)\\];"
  bp_raw <- .regex_capture_all(this_date_text, regexp_calcurve_bp, group = 2)
  calcurve_bp <- if (length(bp_raw)) as.numeric(strsplit(bp_raw[length(bp_raw)], ",", fixed = TRUE)[[1]]) else numeric(0)

  identifier <- "calib[0].sigma="
  this_date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp_calcurve_sigma <- "calib\\[0\\].sigma=\\[(.*)\\];"
  sigma_raw <- .regex_capture_all(this_date_text, regexp_calcurve_sigma, group = 2)
  calcurve_sigma <- if (length(sigma_raw)) as.numeric(strsplit(sigma_raw[length(sigma_raw)], ",", fixed = TRUE)[[1]]) else numeric(0)

  list(
    name       = calcurve_name,
    resolution = calcurve_resolution,
    bp         = calcurve_bp,
    bc         = seq(from = calcurve_start, by = calcurve_resolution, length.out = length(calcurve_bp)),
    sigma      = calcurve_sigma
  )
}

namestolist <- function(x) {
  if (length(x) == 0) {
    return("NA")
  } else {
    this_level <- stats::na.omit(unique(sapply(x, `[`, 1)))
    collector <- vector()
    for (i in seq_along(this_level)) {
      this_element <- this_level[i]
      this_branch <- stats::na.omit(
        sapply(x[sapply(x, `[`, 1) == this_element], `[`, -1)
      )
      this_branch <- this_branch[lapply(this_branch, length) > 0]
      collector <- append(
        collector,
        paste0("`", this_element, "` = ", namestolist(this_branch))
      )
    }
    return(paste0("list(", paste(collector, collapse = ","), ")"))
  }
}

recursivelyPartialUnlist <- function(l) {
  lapply(l, function(x) {
    if (is.list(x) && length(x) == 1 && !is.list(x[[1]])) {
      x[[1]]
    } else if (is.list(x)) {
      recursivelyPartialUnlist(x)
    } else {
      x
    }
  })
}

extractNameFromOxcalResult <- function(date_text) {
  identifier <- "].name="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.name=\")(.*)(\";)"
  .regex_capture_all(date_text, regexp, group = 3)
}

extractTypeFromOxcalResult <- function(date_text) {
  identifier <- "].op="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.op=\")(.*)(\";)"
  .regex_capture_all(date_text, regexp, group = 3)
}

extractBpFromOxcalResult <- function(date_text) {
  identifier <- "].date="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.date=)(.*)(;)"
  cap <- .regex_capture_all(date_text, regexp, group = 3)
  if (!length(cap)) return(NA_integer_)
  suppressWarnings(as.integer(cap[1]))
}

extractStdFromOxcalResult <- function(date_text) {
  identifier <- "].error="
  date_text <- reduce_to_relevant_lines(date_text, identifier)
  regexp <- "(ocd\\[\\d+\\]\\.error=)(.*)(;)"
  cap <- .regex_capture_all(date_text, regexp, group = 3)
  if (!length(cap)) return(NA_integer_)
  suppressWarnings(as.integer(cap[1]))
}

reduce_to_relevant_lines <- function(date_text, identifier) {
  date_text[grepl(identifier, date_text, fixed = TRUE)]
}
