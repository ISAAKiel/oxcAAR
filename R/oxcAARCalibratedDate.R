#' oxcAAR Calibrated Dates Object
#'
#' The function \code{oxcAARCalibratedDate()} creates an object representing a single
#' calibrated radiocarbon date (and optionally its posterior, if a model was run in OxCal).
#'
#' @param name A string giving the name of the date (usually the lab number).
#' @param type A string giving the type of the date in OxCal terminology (e.g. "R_Date", "R_Simulate").
#' @param bp An integer/numeric giving the uncalibrated BP value.
#' @param std An integer/numeric giving the standard deviation.
#' @param cal_curve A list with calibration curve information (name, resolution, bp, bc, sigma).
#' @param sigma_ranges A list of three elements (one, two, three sigma), each a data frame
#'   with columns \code{start}, \code{end}, \code{probability}.
#' @param raw_probabilities A data frame with columns \code{dates} and \code{probabilities}.
#' @param posterior_probabilities A data frame with columns \code{dates} and \code{probabilities},
#'   or \code{NA} if not available.
#' @param posterior_sigma_ranges A list of three elements (one, two, three sigma), each a data frame
#'   with columns \code{start}, \code{end}, \code{probability}, or \code{NA} if not available.
#'
#' @return An object of class \code{"oxcAARCalibratedDate"}.
#' @export
oxcAARCalibratedDate <- function(
    name,
    type,
    bp,
    std,
    cal_curve,
    sigma_ranges,
    raw_probabilities,
    posterior_probabilities = NA,
    posterior_sigma_ranges  = NA
) {
  structure(
    list(
      name = name,
      type = type,
      bp   = bp,
      std  = std,
      cal_curve = cal_curve,
      sigma_ranges = sigma_ranges,
      raw_probabilities = raw_probabilities,
      posterior_sigma_ranges = posterior_sigma_ranges,
      posterior_probabilities = posterior_probabilities
    ),
    class = "oxcAARCalibratedDate"
  )
}

#' Format an oxcAARCalibratedDate
#'
#' @param x An \code{\link{oxcAARCalibratedDate}}.
#' @param ... Unused (kept for S3 consistency).
#'
#' @return A character string (invisibly).
#' @export
format.oxcAARCalibratedDate <- function(x, ...) {
  out_str <- list()
  sigma_str <- list()

  out_str$upper_sep <- "\n============================="
  out_str$name_str  <- paste("\t", print_label(x), sep = "")
  out_str$name_sep  <- "=============================\n"

  if (!is.na(x$bp)) {
    out_str$uncal_str <- paste(
      sprintf("\nBP = %d, std = %d", x$bp, x$std),
      "\n",
      sep = ""
    )
  }

  sigma_str$unmodelled_remark <- ""
  sigma_str$one_sigma_str <- ""
  sigma_str$two_sigma_str <- ""
  sigma_str$three_sigma_str <- ""

  if (has_raw_probabilities(x)) {
    sigma_str$unmodelled_remark <- "unmodelled:"
    sigma_str$one_sigma_str   <- formatFullSigmaRange(x$sigma_ranges$one_sigma,   "one sigma")
    sigma_str$two_sigma_str   <- formatFullSigmaRange(x$sigma_ranges$two_sigma,   "two sigma")
    sigma_str$three_sigma_str <- formatFullSigmaRange(x$sigma_ranges$three_sigma, "three sigma")
  }

  sigma_str$modelled_remark <- "posterior:"
  sigma_str$posterior_one_sigma_str <- ""
  sigma_str$posterior_two_sigma_str <- ""
  sigma_str$posterior_three_sigma_str <- ""

  if (has_posterior_probabilities(x)) {
    sigma_str$posterior_one_sigma_str   <- formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,   "one sigma")
    sigma_str$posterior_two_sigma_str   <- formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,   "two sigma")
    sigma_str$posterior_three_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma, "three sigma")
  }

  if (has_posterior_probabilities(x) || has_raw_probabilities(x)) {
    out_str$sigma_remark <- side_by_side_output(
      sigma_str$unmodelled_remark,
      sigma_str$modelled_remark
    )
    out_str$one_sigma <- side_by_side_output(
      sigma_str$one_sigma_str,
      sigma_str$posterior_one_sigma_str
    )
    out_str$two_sigma <- side_by_side_output(
      sigma_str$two_sigma_str,
      sigma_str$posterior_two_sigma_str
    )
    out_str$three_sigma <- side_by_side_output(
      sigma_str$three_sigma_str,
      sigma_str$posterior_three_sigma_str
    )
  }

  out_str$cal_curve_str <- sprintf("\nCalibrated with:\n\t %s", x$cal_curve$name)

  RVA <- paste(out_str, collapse = "\n")
  invisible(RVA)
}

#' Print an oxcAARCalibratedDate
#'
#' @param x An \code{\link{oxcAARCalibratedDate}}.
#' @param ... Passed to \code{format()}.
#'
#' @export
print.oxcAARCalibratedDate <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' Plot an oxcAARCalibratedDate
#'
#' @param x An \code{\link{oxcAARCalibratedDate}}.
#' @param use_ggplot Logical. If \code{TRUE} (default) and ggplot2 is available,
#'   a ggplot2 plot is produced.
#' @param ... Further arguments passed to the underlying plotting function.
#'
#' @export
plot.oxcAARCalibratedDate <- function(x, use_ggplot = TRUE, ...) {
  if (isTRUE(use_ggplot) && requireNamespace("ggplot2", quietly = TRUE)) {
    plotoxcAARDateGGPlot2(x, ...)
  } else {
    plotoxcAARDateSystemGraphics(x, ...)
  }
}

# ---- internal helpers (date object) ----

.is_df_probs <- function(p) {
  is.data.frame(p) && nrow(p) > 0L && !all(is.na(p$probabilities))
}

.has_curve <- function(curve) {
  is.list(curve) &&
    !is.null(curve$bp) && !is.null(curve$bc) && !is.null(curve$sigma) &&
    length(curve$bp) == length(curve$bc) && length(curve$bp) == length(curve$sigma)
}

.build_prob_df <- function(x) {
  # Returns a data.frame with dates/probability/class (unmodelled/modelled).
  out <- NULL

  if (has_raw_probabilities(x)) {
    out <- data.frame(
      dates = x$raw_probabilities$dates,
      probability = x$raw_probabilities$probabilities,
      class = "unmodelled",
      stringsAsFactors = FALSE
    )
  }

  if (has_posterior_probabilities(x)) {
    out_post <- data.frame(
      dates = x$posterior_probabilities$dates,
      probability = x$posterior_probabilities$probabilities,
      class = "modelled",
      stringsAsFactors = FALSE
    )
    out <- if (is.null(out)) out_post else rbind(out, out_post)
  }

  if (is.null(out)) {
    data.frame(dates = numeric(0), probability = numeric(0), class = character(0))
  } else {
    out
  }
}

.rescale_linear <- function(x, old_min, old_range, new_min, new_range) {
  (x - old_min) / old_range * new_range + new_min
}

# ---- ggplot2 plotting ----

plotoxcAARDateGGPlot2 <- function(x, ...) {
  to_plot <- .build_prob_df(x)
  if (nrow(to_plot) == 0L) {
    warning("No probabilities found to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  if (!.has_curve(x$cal_curve)) {
    warning("Calibration curve information incomplete; plotting without curve overlay.", call. = FALSE)
  }

  base_unit_y <- max(to_plot$probability, na.rm = TRUE) / 25

  # calibration curve data (subset to plot range)
  cal_curve_df <- NULL
  if (.has_curve(x$cal_curve)) {
    cal_curve_df <- data.frame(
      bp = x$cal_curve$bp,
      bc = x$cal_curve$bc,
      sigma = x$cal_curve$sigma
    )
    cal_curve_df <- subset(
      cal_curve_df,
      bc >= min(to_plot$dates, na.rm = TRUE) & bc <= max(to_plot$dates, na.rm = TRUE)
    )
  }

  # Rescale BP-axis onto probability axis range (for ribbon overlay)
  cal_curve_df_old_min <- if (!is.null(cal_curve_df)) min(cal_curve_df$bp, na.rm = TRUE) else NA_real_
  cal_curve_df_old_rng <- if (!is.null(cal_curve_df)) diff(range(cal_curve_df$bp, na.rm = TRUE)) else NA_real_
  cal_curve_df_new_min <- max(to_plot$probability, na.rm = TRUE)
  cal_curve_df_new_rng <- diff(range(to_plot$probability, na.rm = TRUE))

  if (!is.finite(cal_curve_df_old_rng) || cal_curve_df_old_rng == 0) {
    cal_curve_df_old_rng <- NA_real_
  }
  if (!is.finite(cal_curve_df_new_rng) || cal_curve_df_new_rng == 0) {
    cal_curve_df_new_rng <- 1
  }

  # BP distribution curve (left side)
  this_bp_distribution <- list()
  this_bp_distribution$y <- pretty(c(x$bp + 5 * x$std, x$bp - 5 * x$std), n = 20)
  this_bp_distribution$x <- stats::dnorm(this_bp_distribution$y, x$bp, x$std)
  this_bp_distribution <- as.data.frame(this_bp_distribution)

  if (!is.null(cal_curve_df) && is.finite(cal_curve_df_old_min) && is.finite(cal_curve_df_old_rng)) {
    this_bp_distribution$y_rescaled <- .rescale_linear(
      this_bp_distribution$y,
      old_min = cal_curve_df_old_min,
      old_range = cal_curve_df_old_rng,
      new_min = cal_curve_df_new_min,
      new_range = cal_curve_df_new_rng
    )

    cal_curve_df$bp_rescaled <- .rescale_linear(
      cal_curve_df$bp,
      old_min = cal_curve_df_old_min,
      old_range = cal_curve_df_old_rng,
      new_min = cal_curve_df_new_min,
      new_range = cal_curve_df_new_rng
    )

    cal_curve_df$sigma_rescaled <- cal_curve_df$sigma / cal_curve_df_old_rng * cal_curve_df_new_rng
  }

  graph <- ggplot2::ggplot() +
    ggplot2::theme_light() +
    ggplot2::geom_area(
      data = to_plot,
      ggplot2::aes(x = dates, y = probability, group = class, alpha = class),
      fill = "#fc8d62",
      position = "identity",
      color = "#00000077"
    ) +
    ggplot2::labs(
      title   = paste0(x$name, ": ", x$bp, "\u00B1", x$std),
      caption = x$cal_curve$name,
      x       = "Calibrated Date"
    ) +
    ggplot2::scale_alpha_manual(values = c(unmodelled = 0.75, modelled = 0.25), guide = "none")

  # Add calibration curve ribbon if possible
  if (!is.null(cal_curve_df) && all(c("bp_rescaled", "sigma_rescaled") %in% names(cal_curve_df))) {
    graph <- graph +
      ggplot2::geom_ribbon(
        data = cal_curve_df,
        ggplot2::aes(
          x = bc,
          ymax = bp_rescaled + sigma_rescaled,
          ymin = bp_rescaled - sigma_rescaled
        ),
        color = "#8da0cb",
        fill  = "#8da0cb",
        alpha = 0.5
      ) +
      ggplot2::scale_y_continuous(
        "Probability",
        sec.axis = ggplot2::sec_axis(
          ~ (. - cal_curve_df_new_min) / cal_curve_df_new_rng * cal_curve_df_old_rng + cal_curve_df_old_min,
          name = "BP",
          breaks = pretty(cal_curve_df$bp)
        ),
        position = "right"
      )
  }

  x_extend <- ggplot2::ggplot_build(graph)$layout$panel_scales_x[[1]]$range$range

  # Place BP distribution polygon on the left
  this_bp_distribution$x_rescaled <- this_bp_distribution$x / max(this_bp_distribution$x, na.rm = TRUE) *
    diff(x_extend) / 4 + x_extend[1]

  if (!is.null(this_bp_distribution$y_rescaled)) {
    graph <- graph +
      ggplot2::geom_polygon(
        data = this_bp_distribution,
        ggplot2::aes(x = x_rescaled, y = y_rescaled),
        fill = "#66c2a5",
        alpha = 0.5
      ) +
      ggplot2::scale_x_continuous(limits = x_extend, expand = c(0, 0))
  }

  # Sigma ranges: prefer posterior if present, otherwise unmodelled
  this_sigma_ranges <- x$sigma_ranges
  this_sigma_qualifier <- "unmodelled"
  if (!is.null(x$posterior_sigma_ranges) && !is.na(x$posterior_sigma_ranges)[1] && has_posterior_probabilities(x)) {
    this_sigma_ranges <- x$posterior_sigma_ranges
    this_sigma_qualifier <- "modelled"
  }

  if (!any(is.na(this_sigma_ranges))) {
    sigma_text <- paste(
      this_sigma_qualifier,
      formatFullSigmaRange(this_sigma_ranges$one_sigma,   "one sigma"),
      formatFullSigmaRange(this_sigma_ranges$two_sigma,   "two sigma"),
      formatFullSigmaRange(this_sigma_ranges$three_sigma, "three sigma"),
      sep = "\n"
    )

    graph <- graph +
      ggplot2::annotate(
        "text",
        x = x_extend[2] - diff(x_extend) / 20,
        y = max(x$raw_probabilities$probabilities, na.rm = TRUE) * 2,
        label = sigma_text,
        hjust = 1,
        vjust = 1,
        size  = 2
      ) +
      ggplot2::geom_errorbar(
        data = this_sigma_ranges$one_sigma,
        ggplot2::aes(y = -1 * base_unit_y, xmin = start, xmax = end),
        orientation = "y",
        height = base_unit_y
      ) +
      ggplot2::geom_errorbar(
        data = this_sigma_ranges$two_sigma,
        ggplot2::aes(y = -2 * base_unit_y, xmin = start, xmax = end),
        orientation = "y",
        height = base_unit_y
      ) +
      ggplot2::geom_errorbar(
        data = this_sigma_ranges$three_sigma,
        ggplot2::aes(y = -3 * base_unit_y, xmin = start, xmax = end),
        orientation = "y",
        height = base_unit_y
      )
  }

  plot(graph)
  invisible(graph)
}

#' @importFrom graphics text
#' @importFrom stats na.omit
plotoxcAARDateSystemGraphics <- function(x, ...) {
  max_prob <- 0
  years <- years_post <- NA
  probability <- probability_post <- NA
  prob_present <- has_raw_probabilities(x)
  post_present <- has_posterior_probabilities(x)

  if (prob_present) {
    x$raw_probabilities <- protect_against_out_of_range(x$raw_probabilities)
    years <- x$raw_probabilities$dates
    probability <- x$raw_probabilities$probabilities
    unmodelled_color <- "lightgrey"
    max_prob <- max(probability, na.rm = TRUE)
    this_sigma_ranges <- x$sigma_ranges
  }

  if (post_present) {
    x$posterior_probabilities <- protect_against_out_of_range(x$posterior_probabilities)
    years_post <- x$posterior_probabilities$dates
    probability_post <- x$posterior_probabilities$probabilities
    unmodelled_color <- "#eeeeeeee"
    max_prob <- max(max_prob, max(probability_post, na.rm = TRUE))
    this_sigma_ranges <- x$posterior_sigma_ranges
  }

  if (!prob_present && !post_present) {
    year_range <- c(0, 1)
  } else {
    year_range <- get_years_range(x)
  }

  prob_range <- c(0, min(max_prob, 1, na.rm = TRUE))

  graphics::plot(year_range, prob_range, type = "n",
                 ylim = c(max_prob / 7 * -1, max_prob))
  graphics::title(paste(print_label(x), print_bp_std_bracket(x)), line = 2)

  if (prob_present) {
    sigma_text <- paste(
      "unmodelled",
      formatFullSigmaRange(x$sigma_ranges$one_sigma,   "one sigma"),
      formatFullSigmaRange(x$sigma_ranges$two_sigma,   "two sigma"),
      formatFullSigmaRange(x$sigma_ranges$three_sigma, "three sigma"),
      sep = "\n"
    )
    graphics::text(
      x = year_range[1], y = prob_range[2],
      labels = format(sigma_text), cex = 0.4, adj = c(0, 1)
    )
  }

  if (post_present) {
    sigma_text <- paste(
      "posterior",
      formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,   "one sigma"),
      formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,   "two sigma"),
      formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma, "three sigma"),
      sep = "\n"
    )
    graphics::text(
      x = year_range[2], y = prob_range[2],
      labels = format(sigma_text), cex = 0.4, adj = c(1, 1)
    )
  }

  if (!prob_present && !post_present) return(invisible(NULL))

  if (prob_present) {
    graphics::polygon(years, probability, border = "black", col = unmodelled_color)
  }
  if (post_present) {
    graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
  }

  if (!any(is.na(this_sigma_ranges$one_sigma)) &&
      nrow(this_sigma_ranges$one_sigma) > 0) {
    y_pos <- max_prob / 24 * -1
    arrow_length <- max_prob / 8

    graphics::arrows(
      this_sigma_ranges$one_sigma[, 1], y_pos,
      this_sigma_ranges$one_sigma[, 2], y_pos,
      length(this_sigma_ranges$one_sigma),
      col = "black", code = 3, angle = 90,
      length = arrow_length, lty = 1, lwd = 2
    )

    y_pos <- y_pos * 2
    graphics::arrows(
      this_sigma_ranges$two_sigma[, 1], y_pos,
      this_sigma_ranges$two_sigma[, 2], y_pos,
      length(this_sigma_ranges$two_sigma),
      col = "black", code = 3, angle = 90,
      length = arrow_length, lty = 1, lwd = 2
    )

    y_pos <- y_pos / 2 * 3
    graphics::arrows(
      this_sigma_ranges$three_sigma[, 1], y_pos,
      this_sigma_ranges$three_sigma[, 2], y_pos,
      length(this_sigma_ranges$three_sigma),
      col = "black", code = 3, angle = 90,
      length = arrow_length, lty = 1, lwd = 2
    )
  }

  graphics::mtext(x$cal_curve$name, side = 1, line = 4, adj = 1, cex = 0.6)
  invisible(NULL)
}

#' Checks if a variable is of class oxcAARCalibratedDate
#'
#' @param x An object to test.
#'
#' @return \code{TRUE} if \code{x} inherits from \code{"oxcAARCalibratedDate"},
#'   \code{FALSE} otherwise.
#'
#' @export
is.oxcAARCalibratedDate <- function(x) {
  inherits(x, "oxcAARCalibratedDate")
}

get_years_range <- function(calibrated_date) {
  years <- get_prior_years(calibrated_date)
  years_post <- get_posterior_years(calibrated_date)

  if (all(is.na(years)) && all(is.na(years_post))) {
    return(NA)
  }

  c(
    min(years, years_post, na.rm = TRUE),
    max(years, years_post, na.rm = TRUE)
  )
}

get_prior_years <- function(calibrated_date) {
  if (has_raw_probabilities(calibrated_date)) {
    return(calibrated_date$raw_probabilities$dates)
  }
  NA
}

get_posterior_years <- function(calibrated_date) {
  if (has_posterior_probabilities(calibrated_date)) {
    return(calibrated_date$posterior_probabilities$dates)
  }
  NA
}

has_raw_probabilities <- function(calibrated_date) {
  is.data.frame(calibrated_date$raw_probabilities)
}

has_posterior_probabilities <- function(calibrated_date) {
  is.data.frame(calibrated_date$posterior_probabilities)
}

print_label <- function(calibrated_date) {
  paste0(calibrated_date$type, ": ", calibrated_date$name)
}

print_bp_std_bracket <- function(calibrated_date) {
  if (!is.na(calibrated_date$bp)) paste0("(", print_bp_std(calibrated_date), ")") else ""
}

print_bp_std <- function(calibrated_date) {
  if (!is.na(calibrated_date$bp)) paste0(calibrated_date$bp, " \u00b1 ", calibrated_date$std) else ""
}

protect_against_out_of_range <- function(x) {
  x <- rbind(c(min(x$dates) - 1, 0), x)
  x <- rbind(x, c(max(x$dates) + 1, 0))
  x
}
