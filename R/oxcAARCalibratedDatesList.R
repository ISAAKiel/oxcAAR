#' oxcAAR Calibrated Dates List
#'
#' A list of calibrated dates, i.e. objects of class \code{\link{oxcAARCalibratedDate}}.
#'
#' @return An object of class \code{"oxcAARCalibratedDatesList"} (a list).
#' @name oxcAARCalibratedDatesList
NULL

#' Print an oxcAARCalibratedDatesList
#'
#' @param x An \code{\link{oxcAARCalibratedDatesList}}.
#' @param ... Further arguments passed to \code{print()} of single dates.
#'
#' @export
print.oxcAARCalibratedDatesList <- function(x, ...) {
  n <- length(x)

  if (n == 1L) {
    print(x[[1L]], ...)
    return(invisible(x))
  }

  cat(sprintf("List of %d calibrated dates:\n", n))
  for (i in seq_len(n)) {
    print(x[[i]], ...)
  }

  invisible(x)
}

#' Plot an oxcAARCalibratedDatesList
#'
#' For lists of length 1, this dispatches to \code{plot()} for
#' \code{\link{oxcAARCalibratedDate}}. For longer lists, it uses either
#' ggplot2 (ridge plots) or base graphics.
#'
#' @param x An \code{\link{oxcAARCalibratedDatesList}}.
#' @param use_ggplot Logical. If \code{TRUE} (default) and the required packages
#'   are available, a ggplot2-based ridge plot is produced.
#' @param ... Further arguments passed to the underlying plotting function.
#'
#' @export
plot.oxcAARCalibratedDatesList <- function(x, use_ggplot = TRUE, ...) {
  if (length(x) == 1L) {
    return(plot(x[[1L]], use_ggplot = use_ggplot, ...))
  }

  if (isTRUE(use_ggplot) &&
      requireNamespace("ggplot2", quietly = TRUE) &&
      requireNamespace("ggridges", quietly = TRUE)) {
    plotoxcAARCalibratedDatesListGGPlot2(x, ...)
  } else {
    plotoxcAARCalibratedDatesListSystemGraphics(x, ...)
  }
}

# ---- internal helpers ----

.has_posterior <- function(date) {
  is.data.frame(date$posterior_probabilities) &&
    nrow(date$posterior_probabilities) > 0L &&
    !all(is.na(date$posterior_probabilities$probabilities))
}

.has_raw <- function(date) {
  is.data.frame(date$raw_probabilities) &&
    nrow(date$raw_probabilities) > 0L &&
    !all(is.na(date$raw_probabilities$probabilities))
}

.build_ridge_df <- function(x) {
  dfs <- lapply(x, function(y) {
    post <- .has_posterior(y)
    raw  <- .has_raw(y)

    if (!raw && !post) {
      return(NULL)
    }

    alpha <- if (post) c(0.25, 0.75) else c(0.75, NA_real_)

    out <- NULL
    if (raw) {
      out <- data.frame(
        dates       = y$raw_probabilities$dates,
        probability = y$raw_probabilities$probabilities,
        name        = y$name,
        class       = "unmodelled",
        alpha       = alpha[1],
        stringsAsFactors = FALSE
      )
    }

    if (post) {
      out_post <- data.frame(
        dates       = y$posterior_probabilities$dates,
        probability = y$posterior_probabilities$probabilities,
        name        = y$name,
        class       = "modelled",
        alpha       = alpha[2],
        stringsAsFactors = FALSE
      )
      out <- if (is.null(out)) out_post else rbind(out, out_post)
    }

    out
  })

  dfs <- Filter(Negate(is.null), dfs)
  if (length(dfs) == 0L) {
    return(data.frame(
      dates = numeric(0),
      probability = numeric(0),
      name = character(0),
      class = character(0),
      alpha = numeric(0),
      stringsAsFactors = FALSE
    ))
  }

  do.call(rbind, dfs)
}

# ---- plotting implementations ----

plotoxcAARCalibratedDatesListGGPlot2 <- function(x, ...) {
  to_plot <- .build_ridge_df(x)
  if (nrow(to_plot) == 0L) {
    warning("No probabilities found to plot.", call. = FALSE)
    return(invisible(NULL))
  }

  graph <- ggplot2::ggplot(to_plot) +
    ggridges::geom_ridgeline(
      ggplot2::aes(
        x      = dates,
        y      = name,
        height = probability,
        alpha  = alpha,
        group  = interaction(class, name)
      ),
      scale = 100,
      fill  = "#fc8d62",
      color = "#00000077"
    ) +
    ggplot2::theme_light() +
    ggplot2::labs(y = "Dates") +
    ggplot2::scale_alpha_continuous(guide = "none")

  methods::show(graph)
  invisible(graph)
}

plotoxcAARCalibratedDatesListSystemGraphics <- function(x, ...) {
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op), add = TRUE)

  indices <- seq_along(x)

  ranges <- lapply(indices, function(i) get_years_range(x[[i]]))
  mins <- vapply(ranges, function(r) if (all(is.na(r))) NA_real_ else min(r, na.rm = TRUE), numeric(1))
  maxs <- vapply(ranges, function(r) if (all(is.na(r))) NA_real_ else max(r, na.rm = TRUE), numeric(1))

  min_year <- suppressWarnings(min(mins, na.rm = TRUE))
  max_year <- suppressWarnings(max(maxs, na.rm = TRUE))

  if (!is.finite(min_year) || !is.finite(max_year)) {
    min_year <- 0
    max_year <- 1
  }

  graphics::par(mfrow = c(length(x) + 1L, 1L))
  graphics::par(oma = c(3, 1, 2, 2) + 0.1,
                mar = c(0, 1, 0, 1) + 0.1)

  for (i in indices) {
    this <- x[[i]]

    raw_present  <- .has_raw(this)
    post_present <- .has_posterior(this)

    years <- probability <- NA
    years_post <- probability_post <- NA
    max_prob <- 0

    if (raw_present) {
      years <- this$raw_probabilities$dates
      probability <- this$raw_probabilities$probabilities
      max_prob <- max(probability, na.rm = TRUE)
    }

    unmodelled_color <- "lightgrey"

    if (post_present) {
      years_post <- this$posterior_probabilities$dates
      probability_post <- this$posterior_probabilities$probabilities
      unmodelled_color <- "#eeeeeeee"
      max_prob <- max(max_prob, max(probability_post, na.rm = TRUE))
    }

    if (!raw_present && !post_present) {
      max_prob <- 1
    }

    graphics::plot(
      years, probability,
      type = "n",
      ylim = c(max_prob / 7 * -1, max_prob),
      xlim = c(min_year, max_year),
      axes = FALSE
    )
    graphics::axis(side = 4)
    if (raw_present) {
      graphics::polygon(years, probability, border = "black", col = unmodelled_color)
    }
    if (post_present) {
      graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
    }

    graphics::text(
      x = min_year,
      y = max_prob,
      labels = print_label(this),
      las = 2,
      cex = 0.6,
      adj = 0
    )
    graphics::grid()
  }

  graphics::plot(c(min_year, max_year), c(0, 0), axes = FALSE, type = "n")
  graphics::axis(side = 1, at = pretty(c(min_year, max_year)))
  invisible(NULL)
}

#' Checks if a variable is of class oxcAARCalibratedDatesList
#'
#' @param x An object to test.
#'
#' @return \code{TRUE} if \code{x} inherits from \code{"oxcAARCalibratedDatesList"},
#'   \code{FALSE} otherwise.
#'
#' @export
is.oxcAARCalibratedDatesList <- function(x) {
  inherits(x, "oxcAARCalibratedDatesList")
}
