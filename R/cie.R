library(boot)
library(dplyr)
library(broom)

cfdr_cie <- function(.data, .fmla, .model = lm, ..., exposure = NULL, force_include = NULL, flow = "full", exponentiate = FALSE) {

 lhs <- as.character(.fmla)[2]
 rhs <- as.character(.fmla)[3]
 fmla_vars <- stringr::str_split(rhs, pattern = "\\+")[[1]] %>% stringr::str_replace_all(" ", "") # should make this better since assumes there's a space
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
 cfdr_cie_results <- list(results = results, model = list(f = .model, model_args = ...), outcome = outcome, force_include = force_include)
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
  .fmla <- as.character(.fmla)
  lhs <- as.character(.fmla)[2]
  rhs <- as.character(.fmla)[3]
  fmla_vars <- stringr::str_split(rhs, pattern = " \\+ ")[[1]] # should make this better since assumes there's a space
  rhs <- fmla_vars[fmla_vars != .var]
  rhs <- paste(rhs, collapse = " + ")
  as.formula(paste(lhs, "~", rhs))
}

