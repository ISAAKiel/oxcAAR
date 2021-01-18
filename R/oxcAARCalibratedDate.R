#' oxcAAR Calibrated Dates Object
#'
#' The function \code{oxcAARCalibratedDate} is used to create an object for a calibrated date.
#'
#' @param name a string giving the name of the date (usually the lab number)
#' @param type a string giving the type of the date in OxCal terminology ("R_Date", "R_Simulate", ...)
#' @param bp a integer giving the BP value for the date
#' @param std a integer giving the standard deviation for the date
#' @param cal_curve a list containing information about the calibration curve (name, resolution, bp, bc, sigma)
#' @param sigma_ranges a list of three elements (one, two, three sigma),
#' each a data frame with start, end and probability giving
#' @param raw_probabilities a data frame of dates and the related probabilities for each date
#' @param posterior_sigma_ranges a list of three elements (one, two, three sigma),
#' each a data frame with start, end and probability giving for the posterior probabilities
#' @param posterior_probabilities a data frame of dates and the related posterior probabilities for each date
#'
#' @return an object of the class \code{'oxcAARCalibratedDate'}
#' @export
oxcAARCalibratedDate <- function(name, type, bp, std, cal_curve,
                                 sigma_ranges, raw_probabilities, posterior_probabilities=NA,posterior_sigma_ranges=NA){

  RVA <- structure(list(),class="oxcAARCalibratedDate")
  RVA$name <- name
  RVA$type <- type
  RVA$bp <- bp
  RVA$std <- std
  RVA$cal_curve <- cal_curve
  RVA$sigma_ranges <- sigma_ranges
  RVA$raw_probabilities <- raw_probabilities
  RVA$posterior_sigma_ranges <- posterior_sigma_ranges
  RVA$posterior_probabilities <- posterior_probabilities
  RVA
}

##' @export
format.oxcAARCalibratedDate <- function(x, ...){

  out_str <- list()
  sigma_str <- list()
  out_str$upper_sep <- "\n============================="
  out_str$name_str <- paste("\t",print_label(x),sep = "")
  out_str$name_sep <- "=============================\n"

  if(!is.na(x$bp)){
    out_str$uncal_str <- paste(sprintf("\nBP = %d, std = %d",
                                       x$bp,x$std),
                               "\n",sep = "")
  }

  sigma_str$unmodelled_remark <- sigma_str$one_sigma_str <- sigma_str$two_sigma_str <- sigma_str$three_sigma_str <- ""
  if(class(x$raw_probabilities)=="data.frame"){
    sigma_str$unmodelled_remark <- paste("unmodelled:")
    sigma_str$one_sigma_str <- formatFullSigmaRange(x$sigma_ranges$one_sigma,
                                                    "one sigma")
    sigma_str$two_sigma_str <- formatFullSigmaRange(x$sigma_ranges$two_sigma,
                                                    "two sigma")
    sigma_str$three_sigma_str <- formatFullSigmaRange(x$sigma_ranges$three_sigma,
                                                      "three sigma")
  }
  sigma_str$posterior_one_sigma_str <- sigma_str$posterior_two_sigma_str <- sigma_str$posterior_three_sigma_str <- ""
  sigma_str$modelled_remark <- paste("posterior:")
  if(class(x$posterior_probabilities)=="data.frame"){
    sigma_str$posterior_one_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,"one sigma")
    sigma_str$posterior_two_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,"two sigma")
    sigma_str$posterior_three_sigma_str <- formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma,"three sigma")
  }
  if(has_posterior_probabilities(x) | has_raw_probabilities(x))
  {
    out_str$sigma_remark <- side_by_side_output(sigma_str$unmodelled_remark, sigma_str$modelled_remark)
    out_str$one_sigma <- side_by_side_output(sigma_str$one_sigma_str, sigma_str$posterior_one_sigma_str)
    out_str$two_sigma <- side_by_side_output(sigma_str$two_sigma_str, sigma_str$posterior_two_sigma_str)
    out_str$three_sigma <- side_by_side_output(sigma_str$three_sigma_str, sigma_str$posterior_three_sigma_str)
  }
  out_str$cal_curve_str <- sprintf("\nCalibrated with:\n\t %s",x$cal_curve$name)

  RVA <- paste(out_str,collapse = "\n")
  invisible(RVA)
}

#' @export
print.oxcAARCalibratedDate <- function(x, ...) cat(format(x, ...), "\n")

##' @export
plot.oxcAARCalibratedDate <- function(x, use_ggplot=T, ...){
  if (requireNamespace("ggplot2", quietly = TRUE) & use_ggplot) {
    plotoxcAARDateGGPlot2(x, ...)
  } else {
    plotoxcAARDateSystemGraphics(x, ...)
  }
}

#' @importFrom "ggplot2" "ggplot" "aes" "geom_area" "labs" "theme_light" "geom_ribbon" "scale_y_continuous" "sec_axis" "ggplot_build" "geom_polygon" "scale_x_continuous" "annotate" "geom_errorbarh"
plotoxcAARDateGGPlot2<-function(x, ...){
  to_plot <- data.frame(dates=x$raw_probabilities$dates,
                        probability = x$raw_probabilities$probabilities,
                        class = "unmodelled")
  if(!(is.null(x$posterior_probabilities))) {
    to_plot <- rbind(to_plot,
                     data.frame(dates=x$posterior_probabilities$dates,
                                probability = x$posterior_probabilities$probabilities,
                                class = "modelled")
                     )
  }

  cal_curve_df <- data.frame(bp = x$cal_curve$bp,
                             bc = x$cal_curve$bc,
                             sigma = x$cal_curve$sigma)

  base_unit_y <- max(to_plot$probability)/25

  cal_curve_df <- subset(cal_curve_df, bc >= min(to_plot$dates) & bc <= max(to_plot$dates))

  cal_curve_df_old_min <- min(cal_curve_df$bp)
  cal_curve_df_old_range <- diff(range(cal_curve_df$bp))
  cal_curve_df_new_min <- max(to_plot$probability)
  cal_curve_df_new_range <- diff(range(to_plot$probability))

  this_bp_distribution<-NULL
  this_bp_distribution$y <- pretty(c(x$bp+5*x$std, x$bp-5*x$std), n=20)
  this_bp_distribution$x <- dnorm(this_bp_distribution$y,x$bp, x$std)
  this_bp_distribution <- as.data.frame(this_bp_distribution)

  this_bp_distribution$y_rescaled <- (this_bp_distribution$y - cal_curve_df_old_min) / cal_curve_df_old_range * cal_curve_df_new_range + cal_curve_df_new_min

  cal_curve_df$bp_rescaled <-
    (cal_curve_df$bp - cal_curve_df_old_min) / cal_curve_df_old_range * cal_curve_df_new_range + cal_curve_df_new_min

  cal_curve_df$sigma_rescaled <-
    cal_curve_df$sigma / cal_curve_df_old_range * cal_curve_df_new_range

  m <- ggplot() + theme_light()

  graph <- m +
    geom_area(data = to_plot, aes(x=dates,
                                  y=probability,
                                  group = class,
                                  alpha = class),
              fill = "#fc8d62",
              position = "identity") +
    labs(title = paste0(x$name,
                        ": ",
                        x$bp,
                        "\u00B1",
                        x$std),
         caption = x$cal_curve$name,
         x = "Calibrated Date")  +
    scale_alpha_manual(values = c(0.75, 0.25), guide = FALSE)

  graph <- graph +
    geom_ribbon(data = cal_curve_df, aes(x = bc,
                                         ymax = bp_rescaled + sigma_rescaled,
                                         ymin = bp_rescaled - sigma_rescaled),
                color = "#8da0cb",
                fill = "#8da0cb",
                alpha = 0.5) +
    scale_y_continuous("Probability",
                       sec.axis = sec_axis(~ (. - cal_curve_df_new_min)/ cal_curve_df_new_range * cal_curve_df_old_range + cal_curve_df_old_min,
                                           name = "BP",
                                           breaks=pretty(cal_curve_df$bp)),
                       position = "right")

  x_extend <- ggplot_build(graph)$layout$panel_scales_x[[1]]$range$range

  this_bp_distribution$x_rescaled <- this_bp_distribution$x / max(this_bp_distribution$x) * diff(x_extend)/4 + x_extend[1]

  graph <- graph +
    geom_polygon(data = this_bp_distribution, aes(x=x_rescaled, y=y_rescaled),
                 fill = "#66c2a5",
                 alpha=0.5) +
    scale_x_continuous(limits=x_extend, expand = c(0,0))

  this_sigma_ranges <- x$sigma_ranges
  this_sigma_qualifier <- "unmodelled"
  if(!(is.null(x$posterior_sigma_ranges))) {
    this_sigma_ranges <- x$posterior_sigma_ranges
    this_sigma_qualifier <- "modelled"
  }

  sigma_text<-paste(this_sigma_qualifier,
                    oxcAAR:::formatFullSigmaRange(this_sigma_ranges$one_sigma,"one sigma"),
                    oxcAAR:::formatFullSigmaRange(this_sigma_ranges$two_sigma,"two sigma"),
                    oxcAAR:::formatFullSigmaRange(this_sigma_ranges$three_sigma,"three sigma"),
                    sep="\n")

  g2 <- graph + annotate("text",
                         x=x_extend[2] - diff(x_extend)/20,
                         y=max(x$raw_probabilities$probabilities)*2,
                         label= sigma_text,
                         hjust=1,
                         vjust=1,
                         size=2) +
    geom_errorbarh(data=this_sigma_ranges$one_sigma,
                   aes(y=-1*base_unit_y,
                       xmin=start,
                       xmax=end),
                   height = base_unit_y) +
    geom_errorbarh(data=this_sigma_ranges$two_sigma,
                   aes(y=-2*base_unit_y,
                       xmin=start,
                       xmax=end),
                   height = base_unit_y)+
    geom_errorbarh(data=this_sigma_ranges$three_sigma,
                   aes(y=-3*base_unit_y,
                       xmin=start,
                       xmax=end),
                   height = base_unit_y)

  plot(g2)

}

#' @importFrom "graphics" "text"
#' @importFrom "stats" "na.omit"
plotoxcAARDateSystemGraphics <- function(x, ...){
  max_prob <- 0
  years <- years_post <- NA
  probability <- probability_post <- NA
  prob_present <- post_present <- FALSE

  if(has_raw_probabilities(x)) {prob_present <- TRUE}
  if(has_posterior_probabilities(x)) {post_present <- TRUE}

  if (prob_present){

    x$raw_probabilities <- protect_against_out_of_range(x$raw_probabilities)

    years <- x$raw_probabilities$dates

    probability <- x$raw_probabilities$probabilities
    unmodelled_color <- "lightgrey"
    max_prob <- max(probability)
    this_sigma_ranges <- x$sigma_ranges
  }
  if (post_present){

    x$posterior_probabilities <- protect_against_out_of_range(x$posterior_probabilities)

    years_post <- x$posterior_probabilities$dates
    probability_post <- x$posterior_probabilities$probabilities
    unmodelled_color <- "#eeeeeeee"
    max_prob <- max(max_prob, probability_post)
    this_sigma_ranges <- x$posterior_sigma_ranges
  }

  if(!prob_present & !post_present)
  {
    year_range <-c(0,1)
  } else {
    year_range <- get_years_range(x)
  }

  prob_range <- c(0,min(max_prob,1,na.rm=T))

  graphics::plot(year_range, prob_range, type = "n",
                 ylim = c(max_prob / 7 * -1, max_prob))
  graphics::title(paste(print_label(x), print_bp_std_bracket(x)), line = 2)
  if(prob_present){
    sigma_text <- paste(
      "unmodelled",
      formatFullSigmaRange(x$sigma_ranges$one_sigma,"one sigma"),
      formatFullSigmaRange(x$sigma_ranges$two_sigma,"two sigma"),
      formatFullSigmaRange(x$sigma_ranges$three_sigma,"three sigma"),
      sep = "\n"
    )
    text(x = year_range[1], y = prob_range[2], labels = format(sigma_text), cex = 0.4, adj=c(0,1))
  }
  if (post_present) {
    sigma_text <- paste(
      "posterior",
      formatFullSigmaRange(x$posterior_sigma_ranges$one_sigma,"one sigma"),
      formatFullSigmaRange(x$posterior_sigma_ranges$two_sigma,"two sigma"),
      formatFullSigmaRange(x$posterior_sigma_ranges$three_sigma,"three sigma"),
      sep = "\n"
    )
    text(x = year_range[2], y = prob_range[2], labels = format(sigma_text), cex = 0.4, adj=c(1,1))
  }
  if(!prob_present & !post_present) {return()}
  if(prob_present){
    graphics::polygon(years, probability, border = "black", col = unmodelled_color)
  }
  if (post_present){
    graphics::polygon(years_post, probability_post, border = "black", col = "#aaaaaaaa")
  }
  if (any((!is.na(this_sigma_ranges$one_sigma))) && any((length(this_sigma_ranges$one_sigma[,1])) > 0)){
    y_pos <- max_prob / 24 * -1
    arrow_length <- max_prob / 8
    graphics::arrows(
      this_sigma_ranges$one_sigma[,1],
      y_pos,
      this_sigma_ranges$one_sigma[,2],
      y_pos,
      length(this_sigma_ranges$one_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos * 2
    graphics::arrows(
      this_sigma_ranges$two_sigma[,1],
      y_pos,
      this_sigma_ranges$two_sigma[,2],
      y_pos,
      length(this_sigma_ranges$two_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
    y_pos <- y_pos / 2 * 3
    graphics::arrows(
      this_sigma_ranges$three_sigma[,1],
      y_pos,
      this_sigma_ranges$three_sigma[,2],
      y_pos,
      length(this_sigma_ranges$three_sigma),
      col="black",code=3,angle=90,length=arrow_length,lty=1,lwd=2
    )
  }

  graphics::mtext(x$cal_curve$name, side = 1, line = 4, adj = 1,
                  cex = 0.6)
}

#' Checks if a variable is of class oxcAARCalibratedDate
#'
#' Checks if a variable is of class oxcAARCalibratedDate
#'
#' @param x a variable
#'
#' @return true if x is a oxcAARCalibratedDate, false otherwise
#'
#' @export
is.oxcAARCalibratedDate <- function(x) {"oxcAARCalibratedDate" %in% class(x)}

#' @title Convert to a CalDates object
#' @description Convert oxcAARCalibratedDatesList to an rcarbon CalDates object. The function is taken and adapted from rcarbon (Bevan, A. and Crema, E.R., 2019).
#' @param x One or more calibrated dated in the form of an oxcAARCalibratedDatesList
#' @return A CalDates object
#' @examples
#' \dontrun{
#' library(oxcAAR)
#' library(rcarbon)
#' quickSetupOxcal()
#' dates <- data.frame(CRA=c(3200,2100,1900), Error=c(35,40,50))
#' rcaldates <- rcarbon::calibrate(dates$CRA, dates$Error, calCurves=rep("intcal13"))
#' ocaldates <- oxcAAR::oxcalCalibrate(c(3200,2100,1900),c(35,40,50),c("a","b","c"))
#' ## Convert to rcarbon format
#' caldates.o <- as.CalDates(ocaldates)
#' ## Comparison plot
#' plot(rcaldates$grids[[2]]$calBP,rcaldates$grids[[2]]$PrDens,
#' type="l", col="green", xlim=c(2300,1900))
#' lines(caldates.o$grids[[2]]$calBP,caldates.o$grids[[2]]$PrDens, col="blue")
#' legend("topright", legend=c("rcarbon","OxCal"), col=c("green","blue"), lwd=2)
#' }
#' @export
#'
as.CalDates <- function(x){
  if (!any(class(x)%in%c("oxcAARCalibratedDatesList"))){
    stop("x must be of class oxcAARCalibratedDatesList")
  }
  if (any(class(x)=="oxcAARCalibratedDatesList")){
    reslist <- vector(mode="list", length=2)
    sublist <- vector(mode="list", length=length(x))
    names(sublist) <- names(x)
    names(reslist) <- c("metadata","grids")
    ## metadata
    df <- as.data.frame(matrix(ncol=12, nrow=length(x)), stringsAFactors=FALSE)
    names(df) <- c("DateID","CRA","Error","Details","CalCurve","ResOffsets","ResErrors","StartBP","EndBP","Normalised","CalEPS", "Method")
    df$DateID <- names(x)
    df$CRA <- as.numeric(unlist(lapply(X=x, FUN=`[[`, "bp")))
    df$Error <- as.numeric(unlist(lapply(X=x, FUN=`[[`, "std")))
    df$CalCurve=lapply(lapply(lapply(lapply(lapply(x,FUN=`[[`,"cal_curve"),FUN=`[[`,"name"),strsplit," "),unlist),FUN=`[[`,2)
    df$CalCurve=tolower(df$CalCurve)
    df$ResOffsets <- NA
    df$ResErrors <- NA
    df$StartBP <- NA
    df$EndBP <- NA
    df$Normalised <- TRUE
    df$CalEPS <- 0
    df$Method <- 'oxcAAR'
    reslist[["metadata"]] <- df
    ## grids
    for (i in 1:length(x)){
      tmp <- x[[i]]$raw_probabilities
      rr <- range(tmp$dates)
      res <- 	approx(x=tmp$dates,y=tmp$probabilities,xout=ceiling(rr[1]):floor(rr[2]))
      res$x <- abs(res$x-1950)
      res <- data.frame(calBP=res$x,PrDens=res$y)
      res$PrDens <- res$PrDens/sum(res$PrDens)
      class(res) <- append(class(res),"calGrid")
      sublist[[i]] <- res
    }
    reslist[["grids"]] <- sublist
    reslist[["calmatrix"]] <- NA
    class(reslist) <- c("CalDates",class(reslist))
    return(reslist)
  }
}

get_years_range <- function(calibrated_date) {
  years <- get_prior_years(calibrated_date)
  years_post <- get_posterior_years(calibrated_date)
  if (all(is.na(years)) && all(is.na(years_post))) {
    return(NA)
  } else {
    return(
      c(
        min(years,years_post, na.rm = TRUE),
        max(years,years_post, na.rm = TRUE)
      )
    )
  }
}

get_prior_years <- function(calibrated_date) {
  years <- NA
  if (has_raw_probabilities(calibrated_date)){
    years <- calibrated_date$raw_probabilities$dates
  }
  return(years)
}

get_posterior_years <- function(calibrated_date) {
  years <- NA
  if (has_posterior_probabilities(calibrated_date)){
    years <- calibrated_date$posterior_probabilities$dates
  }
  return(years)
}

has_raw_probabilities <- function(calibrated_date) {
  class(calibrated_date$raw_probabilities)=="data.frame"
}

has_posterior_probabilities <- function(calibrated_date) {
  class(calibrated_date$posterior_probabilities)=="data.frame"
}

print_label <- function(calibrated_date) {
  paste(calibrated_date$type, ": " ,calibrated_date$name, sep="")
}

print_bp_std_bracket <- function(calibrated_date) {
  RVA <- ""
  if(!is.na(calibrated_date$bp)){
    RVA <- paste("(",print_bp_std(calibrated_date),")", sep="")
  }
  return(RVA)
}

print_bp_std <- function(calibrated_date) {
  RVA <- ""
  if(!is.na(calibrated_date$bp)){
    RVA <- paste(calibrated_date$bp, " \u00b1 ", calibrated_date$std)
  }
  return(RVA)
}

protect_against_out_of_range <- function(x) {
  x <- rbind(c(min(x$dates)-1,0), x)
  x <- rbind(x, c(max(x$dates)+1,0))
  return(x)
}
