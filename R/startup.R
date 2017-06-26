.onLoad <- function(libname, pkgname) {
  op <- options()
  op.oxcAAR <- list(
    oxcAAR.oxcal_path = ""
  )
  toset <- !(names(op.oxcAAR) %in% names(op))
  if(any(toset)) options(op.oxcAAR[toset])

  invisible()
}
