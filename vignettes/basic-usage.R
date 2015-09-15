## ----fig.width=7,fig.height=4--------------------------------------------
library(roxcal)
setOxcalExecutablePath("/home/martin/Documents/scripte/OxCal/bin/OxCalLinux")
my_date <- oxcalCalibrate(5000,25,"KIA-12345")
my_date
plot(my_date)

## ----fig.width=7,fig.height=4--------------------------------------------
my_uncal_dates <- data.frame(bp=c(5000,4500,4000),
                             std=c(45,35,25),
                             names=c("Date 1", "Date 2", "Date 3")
                             )
my_cal_dates <- oxcalCalibrate(my_uncal_dates$bp, my_uncal_dates$std, my_uncal_dates$names)
my_cal_dates
plot(my_cal_dates)

## ------------------------------------------------------------------------
str(my_cal_dates, max.level = 1)
my_cal_dates[[1]] # equivalent to my_cal_dates[["Date 1"]] or my_cal_dates$`Date 1`
str(my_cal_dates$`Date 1`)
plot(
  my_cal_dates$`Date 1`$raw_probabilities$dates,
  my_cal_dates$`Date 1`$raw_probabilities$probabilities,
  type = "l",
  xlab = "years",
  ylab = "probs"
  )

## ----fig.width=7,fig.height=4--------------------------------------------
my_cal_date <- data.frame(bp=c(-3400),
                             std=c(25),
                             names=c("SimDate_1")
                             )
my_simulated_dates <- oxcalSimulate(my_cal_date$bp, my_cal_date$std, my_cal_date$names)
# equivalent to
my_simulated_dates <- oxcalSimulate(-3400, 25, "SimDate_1")
my_simulated_dates
plot(my_simulated_dates)

## ----fig.width=7,fig.height=4--------------------------------------------
my_sum_sim<-oxcalSumSim(
  timeframe_begin = -4000,
  timeframe_end = -3000,
  n = 50,
  stds = 35,
  date_distribution = "uniform"
  )
str(my_sum_sim)
plot(my_sum_sim)

## ----echo=F--------------------------------------------------------------
library(knitr)
hook_output <- knit_hooks$get("output")
knit_hooks$set(output = function(x, options) {
   lines <- options$output.lines
   if (is.null(lines)) {
     return(hook_output(x, options))  # pass to default hook
   }
   x <- unlist(strsplit(x, "\n"))
   more <- "..."
   if (length(lines)==1) {        # first n lines
     if (length(x) > lines) {
       # truncate the output, but add ....
       x <- c(head(x, lines), more)
     }
   } else {
     x <- c(more, x[lines], more)
   }
   # paste these lines together
   x <- paste(c(x, ""), collapse = "\n")
   hook_output(x, options)
 })

## ------------------------------------------------------------------------
R_Simulate(-4000, 25, "MySimDate")
my_dates <- R_Date(c("Lab-12345","Lab-54321"), c(5000,4500), 25)
cat(my_dates)
my_sum <- oxcal_Sum(my_dates)
cat(my_sum)

## ----output.lines=8------------------------------------------------------
knitr::opts_chunk$set(cache=TRUE)
my_oxcal_code <- ' Plot()
 {
  Sequence("Sequence1")
  {
   Boundary("Beginn");
   Phase("Phase1")
   {
    R_Date("Lab-1",5000,25);
    R_Date("Lab-2",4900,37);
   };
   Boundary("Between");
   Phase("Phase2")
   {
    R_Date("Lab-3",4800,43);
   };
   Boundary("End");
  };
 };'
my_result_file <- executeOxcalScript(my_oxcal_code)
my_result_text <- readOxcalOutput(my_result_file)
my_result_data <- parseFullOxcalOutput(my_result_text)
str(my_result_data)

