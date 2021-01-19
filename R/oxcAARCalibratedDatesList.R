#' oxcAAR Calibrated Dates List
#'
#' A List of \code{\link{oxcAARCalibratedDate}}
#'
#' @return an object of the class 'oxcAARCalibratedDatesList'
#' @name oxcAARCalibratedDatesList
NULL

#' @export
print.oxcAARCalibratedDatesList <- function(x, ...){
  n <- length(x)
  if (n == 1) print(x[[1]],...)
  else{
    cat(sprintf("List of %d calibrated dates:\n",n))
    for (i in 1:n) {
      print(x[[i]])
    }
  }
}

#' @export
plot.oxcAARCalibratedDatesList <- function(x, use_ggplot = T, ...){
  if (length(x) == 1) plot(x[[1]], use_ggplot, ...)
  else{
    if (requireNamespace("ggplot2", quietly = TRUE) & use_ggplot) {
      plotoxcAARCalibratedDatesListGGPlot2(x, ...)
    } else {
      plotoxcAARCalibratedDatesListSystemGraphics(x, ...)
    }
  }
}

#' @importFrom "ggplot2" "ggplot" "aes" "theme" "theme_light" "scale_alpha_manual" "scale_alpha_continuous"
#' @importFrom "ggridges" "geom_ridgeline"
#' @importFrom "methods" "show"
plotoxcAARCalibratedDatesListGGPlot2<-function(x, ...){
  .data <- NULL
  to_plot<-lapply(x,function(y){
    alpha <- ifelse((!(is.null(y$posterior_probabilities))& !(is.na(y$posterior_probabilities))),
                    c(0.25,0.75),
                      c(0.75,0)
                      )
    outdata <- data.frame(
      dates=y$raw_probabilities$dates,
      probability=y$raw_probabilities$probabilities,
      name=y$name,
      class = "unmodelled",
      alpha=alpha[1])
    if(!(is.null(y$posterior_probabilities)) & !(is.na(y$posterior_probabilities))) {
      outdata <- rbind(outdata,
                   data.frame(
                     dates=y$posterior_probabilities$dates,
                     probability=y$posterior_probabilities$probabilities,
                     name=y$name,
                     class = "modelled",
                     alpha=alpha[2])
      )
    }
    return(outdata)

  })
  to_plot <- do.call(rbind,to_plot)

  m <- ggplot(to_plot)

  graph <- m + geom_ridgeline(aes(x = .data$dates,
                                  y = .data$name,
                                  height = .data$probability,
                                  alpha = .data$alpha,
                                  group = interaction(.data$class,.data$name)),
                              scale=100,
                              fill = "#fc8d62",
                              color="#00000077") +
    theme_light() + labs(y="Dates")  +
    scale_alpha_continuous(guide = FALSE)

  show(graph)
}

plotoxcAARCalibratedDatesListSystemGraphics <- function(x, ...){
  op <- graphics::par(no.readonly = TRUE)
  years <- years_post <- NA

  indices <- 1:length(x)

  min_year <- min(
    sapply(
      indices,
      function(i) {
        this_year_range <- get_years_range(x[[i]])
        if (!all(is.na(this_year_range))) {
          min(this_year_range, na.rm=T)
        } else {
          NA
        }
      }
    ), na.rm=T
  )

  max_year <- max(
    sapply(
      indices,
      function(i) {
        this_year_range <- get_years_range(x[[i]])
        if (!all(is.na(this_year_range))) {
          max(this_year_range, na.rm=T)
        } else {
          NA
        }
      }
    ), na.rm=T
  )

  graphics::par(mfrow=c(length(x)+1,1))
  graphics::par(oma = c(3,4,2,3) + 0.1,
                mar = c(0,1,0,1) + 0.1)
  for (i in indices) {
    max_prob <- 0
    years <- probability <- NA
    post_present <- prob_present <- FALSE

    if(class(x[[i]]$raw_probabilities)=="data.frame") {
      prob_present <- TRUE
      years <- x[[i]]$raw_probabilities$dates
      probability <- x[[i]]$raw_probabilities$probabilities
      max_prob <- max(probability)
    }

    unmodelled_color <- "lightgrey"

    years_post <- probability_post <- NA

    if(class(x[[i]]$posterior_probabilities)=="data.frame") {
      post_present <- TRUE
      years_post <- x[[i]]$posterior_probabilities$dates
      probability_post <- x[[i]]$posterior_probabilities$probabilities
      unmodelled_color <- "#eeeeeeee"
      max_prob <- max(max_prob, probability_post)
    }

    if(!prob_present & !post_present)
    {
      year_range <-c(0,1)
    } else {
      year_range <- get_years_range(x[[i]])
    }

    graphics::plot(
      years,
      probability,
      type = "n",
      ylim=c(max_prob / 7 * -1,max_prob),
      xlim = c(min_year,max_year),
      axes = FALSE
    )
    graphics::axis(side=4)
    graphics::polygon(years, probability, border = "black", col = unmodelled_color)
    if (unmodelled_color!="lightgrey"){
      graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
    }
    graphics::mtext(print_label(x[[i]]),side=2,las=2,cex=0.6)
    graphics::grid()
  }
  plot(c(min_year,max_year),c(0,0),
       axes = FALSE,
       type="n")
  graphics::axis(side=1)
  graphics::par(op)
}

#' Checks if a variable is of class oxcAARCalibratedDatesList
#'
#' Checks if a variable is of class oxcAARCalibratedDatesList
#'
#' @param x a variable
#'
#' @return true if x is a oxcAARCalibratedDatesList, false otherwise
#'
#' @export
is.oxcAARCalibratedDatesList <- function(x) {"oxcAARCalibratedDatesList" %in% class(x)}
