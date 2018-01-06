library(boot)
library(dplyr)
library(broom)
n <- 100
z <- rnorm(n)
x <- rnorm(n) + z
noise <- rnorm(n)
collider <- rnorm(n) + x + y
y <- rnorm(n) + x + z
y2 <- rbinom(n, 1, .5)
df <- data.frame(y, x, z, noise, collider)

lm(y ~ x + z + noise, data = df) %>% broom::tidy() %>% mutate(p.value = round(p.value, 3))

cfdr_cie(df, y ~ x + z + noise, .model = lm)
cfdr_cie(df, y ~ x + z + noise, .model = lm)

cfdr_cie <- function(.data, .fmla, .model = lm, ..., exposure = NULL, flow = "full") {

 lhs <- as.character(.fmla)[2]
 rhs <- as.character(.fmla)[3]
 fmla_vars <- stringr::str_split(rhs, pattern = " \\+ ")[[1]] # should make this better since assumes there's a space
 if (is.null(exposure)) {
   outcome <- fmla_vars[1]
 } else {
   if (!is.character(exposure)) stop("`exposure` must be a character vector")
   outcome <- exposure
 }
 covars <- fmla_vars[fmla_vars != outcome]

 purrr::map2_df(outcome, covars, ~cfdr_bootstrap_cie(.fmla, .x, .y, .model, .data))

}

cfdr_bootstrap_cie <- function(.fmla, .x, .z, .model, .data, samples = 200, conf.method = "bca", conf.level = .95) {
  bootstrap_samples <- boot::boot(.data, cie, R = samples, .fmla = .fmla, .x = .x, .z = .z, .model = .model) %>%
    broom:::tidy.boot(conf.int = TRUE, conf.method = conf.method, conf.level = conf.level)
  names(bootstrap_samples)[1] <- "pct_cie"
  cbind(x = .x, z = .z, bootstrap_samples, stringsAsFactors = FALSE)
}

cie <- function(.data, indices, .fmla, .x, .z, .model) {
  .data <- .data[indices, ]
  full <- .model(.fmla, data = .data) %>%
    broom::tidy() %>%
    dplyr::filter(term == .x) %>%
    dplyr::pull(estimate)

  no_z <- .model(remove_var_from_fmla(.fmla, .z), data = .data) %>%
    broom::tidy() %>%
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

