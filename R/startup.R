.onLoad <- function(libname, pkgname) {
  op <- options()
  op.roxcal <- list(
    roxcal.oxcal_path = ""
  )
  toset <- !(names(op.roxcal) %in% names(op))
  if(any(toset)) options(op.roxcal[toset])

  invisible()
}
