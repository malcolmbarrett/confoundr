# should also have a method for doing this for just any broom supported model that is not a cfdr obj

cfdr_predict <- function(.cfdr, style = "average", conf.int = TRUE,
                         conf.level = 0.95, exponentiate = FALSE, ...) {
  UseMethod("cfdr_predict")
}

cfdr_predict.cfdr_glmnet <- function(.cfdr, style = "average", conf.int = TRUE,
                         conf.level = 0.95, exponentiate = FALSE, ...) {

}

cfdr_predict.cfdr_cie <- function(.cfdr, style = "average", conf.int = TRUE,
                         conf.level = 0.95, exponentiate = FALSE, ...) {

}

cfdr_predict.lm <- function(.cfdr, exposure = NULL, style = "average", conf.int = TRUE,
                                  conf.level = 0.95, exponentiate = FALSE, ...) {
  # capture exposure, manipulate data

  fmla <- formula(.cfdr)
  data <- .cfdr$data
  lhs <- as.character(fmla)[2]
  rhs <- as.character(fmla)[3]
  rhs <- stringr::str_split(rhs, pattern = "\\+")[[1]] %>% stringr::str_replace_all(" ", "")

  if (is.null(exposure)) {
    exposure_var <- rhs[1]
  } else {
    if (!is.character(exposure)) stop("`exposure` must be a character vector")
    exposure_var <- exposure
  }

  rhs <- rhs[!(rhs %in% exposure_var)]

  most_freq <- function(x) dplyr::arrange(data.frame(table(x)), dplyr::desc(Freq))[[1, 1]]
  newdata <- data %>%
    dplyr::select(rlang::UQ(rhs)) %>%
    dplyr::mutate_if(is.numeric, mean) %>%
    dplyr::mutate_if(is.factor, most_freq) %>%
    dplyr::mutate_if(is.character, most_freq)

  z_vals <- dplyr::slice(newdata, 1)

  newdata <- data %>%
    dplyr::select(rlang::UQ(exposure_var)) %>%
    purrr::map_dfc(function(.x) {
      if (is.numeric(.x)) {
        seq(min(.x), max(.x), length.out = 100)
      } else if (is.factor(.x) || is.character(.x)) {
        unique(.x)
      } else {
        stop("`exposure` must be of class numeric, factor, or character")
      }
    }) %>%
    cbind(z_vals)


  bind_data <- function(data, z_vals) {
    data %>%
      dplyr::select(rlang::UQ(lhs), rlang::UQ(exposure_var)) %>%
      cbind(data, z_vals)
  }

  predict(.cfdr, newdata = newdata, interval = 'confidence', level = 0.95)
  df %>% broom::bootstrap(200) %>% dplyr::do(broom::augment(lm(y ~ x + z, data = .), newdata = newdata)) %>%
    #filter(replicate == 1) %>%
    ggplot2::ggplot(ggplot2::aes(x, y = .fitted)) +
    ggplot2::geom_line(alpha = .2)


}
