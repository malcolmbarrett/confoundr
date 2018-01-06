cfdr_pull <- function(.cfdr) {
  #method to pull confounders from any of the detect methods
  UseMethod("cfdr_pull")
}

cfdr_pull.cfdr_glmnet <- function(.cfdr, model_f = glm) {
  do.call(model_f, .cfdr$model$model_args)
}

cfdr_pull.cfdr_cie <- function(.cfdr, cutoff = 15) {
  results <- .cfdr$results %>%
    dplyr::filter(abs(pct_cie) > 15) %>%
    dplyr::pull(z) %>%
    unique()
  .fmla <- remove_var_from_fmla(.cfdr$model$formula, c("z", "noise"))
  .cfdr$model$model_args$formula <- .fmla
  .cfdr$model$model_args$data <- .cfdr$model$data
  do.call(.cfdr$model$f, .cfdr$model$model_args)
}

cfdr_to_formula <- function(.cfdr) {
  UseMethod("cfdr_to_formula")
}

cfdr_to_formula.cfdr_glmnet <- function(.cfdr) {

}

cfdr_to_formula.cfdr_cie <- function(.cfdr) {
  results <- .cfdr$results %>%
    dplyr::filter(abs(pct_cie) > 15) %>%
    dplyr::pull(z) %>%
    unique()
  keep_vars <- c(.cfdr$model$exposure, results)
  if (!is.null(.cfdr$model$force_include)) keep_vars <- c(keep_vars, .cfdr$model$force_include)
  keep_var_in_fmla(.cfdr$model$formula, keep_vars)
}

cfdr_to_character <- function(.cfdr) {
  UseMethod("cfdr_to_character")
}

cfdr_to_character.cfdr_glmnet <- function(.cfdr) {

}

cfdr_to_character.cfdr_cie <- function(.cfdr) {
  .cfdr$results %>%
    dplyr::filter(abs(pct_cie) > 15) %>%
    dplyr::pull(z) %>%
    unique()
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
