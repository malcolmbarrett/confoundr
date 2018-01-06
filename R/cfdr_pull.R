cfdr_pull <- function(.cfdr) {
  #method to pull confounders from any of the detect methods
  if (is(.cfdr, "cfdr_cie")) {

  } else if (is(.cfdr, "cfdr_glmnet")) {

  } else  {
    stop("`.cfdr` must be of class 'cfdr_glmnet' or 'cfdr_cie'")
  }
}

print.cfdr <- function(.cfdr) {
  print(.cfdr$results)
}
