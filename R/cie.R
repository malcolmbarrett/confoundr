library(boot)
library(dplyr)
library(broom)

#' Change in estimate
#'
#' @param .data data
#' @param .fmla formula
#' @param .model model
#' @param ... args
#' @param exposure e
#' @param force_include g
#' @param flow gg
#' @param exponentiate gg
#'
#' @return .cfdr object
#' @export
#'
#' @examples
cfdr_cie <- function(.data, .fmla, .model = lm, ..., exposure = NULL, force_include = NULL, flow = "full", exponentiate = FALSE) {

 lhs <- as.character(.fmla)[2]
 rhs <- as.character(.fmla)[3]
 fmla_vars <- stringr::str_split(rhs, pattern = "\\+")[[1]] %>% stringr::str_replace_all(" ", "")
 if (is.null(exposure)) {
   outcome <- fmla_vars[1]
 } else {
   if (!is.character(exposure)) stop("`exposure` must be a character vector")
   outcome <- exposure
 }
 covars <- fmla_vars[fmla_vars != outcome]
# args <- list(...)
.f <- function(.x, .y, ...) {
  #browser()
  cfdr_bootstrap_cie(.fmla = .fmla, .x = .x,
                     .z = .y, .data = .data,
                     .model = .model, ...,
                     exponentiate = exponentiate)
}
 results <- purrr::map2_df(outcome, covars, .f, ...)
 cfdr_cie_results <- list(results = results, model = list(f = .model, formula = .fmla, data = .data, model_args = list(...), exposure = outcome, force_include = force_include))
 class(cfdr_cie_results) <- c("cfdr", "cfdr_cie")
 cfdr_cie_results
}

cfdr_bootstrap_cie <- function(.fmla, .x, .z, .data, .model = lm, ..., n_samples = 200,
                               conf.method = "bca", conf.level = .95,
                               exponentiate = FALSE) {
  #browser()

  bootstrap_samples <- boot::boot(data = .data, statistic = cie, R = n_samples,
                                  sim = "ordinary", stype = "i", strata = rep(1, nrow(.data)),
                                  .fmla = .fmla, .x = .x, .z = .z,
                                  .model = .model,
                                  exponentiate = exponentiate, ...) %>%
    broom::tidy(conf.int = TRUE, conf.method = conf.method, conf.level = conf.level)
  names(bootstrap_samples)[1] <- "pct_cie"
  cbind(x = .x, z = .z, bootstrap_samples, stringsAsFactors = FALSE)
}

cie <- function(.data, indices, .fmla, .x, .z, .model, ..., exponentiate = FALSE) {
  .data <- .data[indices, ]
  #args <- list(...)
  #browser()
  full <- .model(.fmla, data = .data, ...) %>%
    broom::tidy(exponentiate = exponentiate) %>%
    dplyr::filter(term == .x) %>%
    dplyr::pull(estimate)

  no_z <- .model(remove_var_from_fmla(.fmla, .z), data = .data, ...) %>%
    broom::tidy(exponentiate = exponentiate) %>%
    dplyr::filter(term == .x) %>%
    dplyr::pull(estimate)

  ((full - no_z) / full ) * 100 # should percent or prop be an option?
}

remove_var_from_fmla <- function(.fmla, .var) {
  if (length(.var) == 0) return(.fmla)
  #.fmla <- as.character(.fmla)
  lhs <- as.character(.fmla)[2]
  rhs <- as.character(.fmla)[3]
  fmla_vars <- stringr::str_split(rhs, pattern = "\\+")[[1]] %>% stringr::str_replace_all(" ", "")
  rhs <- fmla_vars[!(fmla_vars %in% .var)]

  rhs <- paste(rhs, collapse = " + ")

  as.formula(paste(lhs, "~", rhs))
}

keep_var_in_fmla <- function(.fmla, .var) {
  #.fmla <- as.character(.fmla)
  lhs <- as.character(.fmla)[2]
  rhs <- as.character(.fmla)[3]
  fmla_vars <- stringr::str_split(rhs, pattern = "\\+")[[1]] %>% stringr::str_replace_all(" ", "")
  rhs <- fmla_vars[fmla_vars %in% .var]
  rhs <- paste(rhs, collapse = " + ")
  as.formula(paste(lhs, "~", rhs))
}

cfdr_cie_plot <- function(.cfdr_cie, height = .15) {
  p <- ggplot2::ggplot(.cfdr_cie$results,
                       ggplot2::aes(x = pct_cie, y = z,
                                    xmin = conf.low,
                                    xmax = conf.high)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbarh(height = height) +
    ggplot2::geom_vline(ggplot2::aes(xintercept = 0), linetype = "dashed") +
    ggplot2::labs(x = "Percent Change in Coefficient", y = "Potential Confounder")

  if (dplyr::n_distinct(.cfdr_cie$results$x) > 1) p <- p + ggplot2::facet_wrap(~x, scales = "free")
  p
}
