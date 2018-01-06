cfdr_pull <- function(.cfdr) {
  #method to pull confounders from any of the detect methods
  UseMethod("cfdr_pull")
}

cfdr_pull.cfdr_glmnet <- function(.cfdr) {

}

cfdr_pull.cfdr_cie <- function(.cfdr) {

}

cfdr_to_formula <- function(.cfdr) {
  cfdr_pull(.cfdr)
}

cfdr_to_character <- function(.cfdr) {
  cfdr_pull(.cfdr)
}

#' Print method for cfdr
#'
#' @param .cfdr a cfdr object
#'
#' @export
#'
print.cfdr <- function(.cfdr) {
  print(.cfdr$results)
}
