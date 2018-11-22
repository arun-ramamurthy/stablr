###################
#### STABILITY ####
ES.lm <- function(.data, lm_formula, V = 100, perturb_fn = perturb) {
  simulate_single_yhat <- function() {
    .data %>%
      perturb_fn() %>%
      lm(formula = lm_formula) %>%
      predict()
  }

  V_yhats <- purrr::rerun(V, simulate_single_yhat())
  mean_yhat <- purrr::invoke(cbind, V_yhats) %>% rowMeans()
  T_hat <-
    V_yhats %>%
    purrr::map_dbl(~ norm(. - mean_yhat, type = "2")) %>%
    mean()
  ES <- T_hat / norm(mean_yhat, type = "2")
  ES
}
###################

#########################
#### DATA GENERATION ####
generate_uniform_vector <- function(n = 150, bound = 10) {
  runif(n, -bound, bound)
}

generate_normal_vector <- function(n = 150, sd = 5) {
  rnorm(n, sd = sd)
}

#' @import magrittr
generate_data <- function(n = 150, k = 3,
                          covariate_generation_fn = generate_uniform_vector,
                          y_formula, y_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  y_formula <- rlang::enquo(y_formula)
  covariates <-
    purrr::rerun(k, purrr::partial(covariate_generation_fn, n = n)()) %>%
    dplyr::bind_cols()
  covariates %>%
    dplyr::rename_all(dplyr::funs(stringr::`str_sub<-`(string = ., value = "x_", start = 1, end = 1))) %>%
    dplyr::mutate(y := (!! y_formula) + purrr::partial(y_noise_fn, n = n)())
}

augment_collinearity <- function(.data,
                                 new_col_formula, new_col_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  new_col_formula <- rlang::enquo(new_col_formula)
  n <- nrow(.data)
  integer <- sample(100, size = 1)
  while ((paste('x_', integer, sep = '')) %in% names(.data)){
    integer <- sample(100, size = 1)
  }
  .data %>%
    dplyr::mutate((!! paste('x_', integer, sep = '')) :=
                    (!! new_col_formula) + purrr::partial(new_col_noise_fn, n = n)())
}

augment_multicollinearity <- function(.data, k = 3,
                                      new_col_formula, new_col_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  new_col_formula <- rlang::enquo(new_col_formula)
  augment_col <- purrr::partial(augment_collinearity,
                          new_col_formula = (!! new_col_formula),
                          new_col_noise_fn = new_col_noise_fn)
  augment_multicol <- purrr::rerun(k, augment_col) %>% purrr::reduce(purrr::compose)
  augment_multicol(.data)
}
#########################

######################
#### PERTURBATION ####
perturb <- function(.data, variable = x, sd = 1) {
  variable <- rlang::enquo(variable)
  raw_vector <- .data %>% dplyr::pull(!! variable)
  additional_noise <- rnorm(n = length(raw_vector), sd = sd)
  .data %>%
    dplyr::mutate((!! variable) := (!! variable) + additional_noise)
}

perturb_unif <- function(.data, variable = x, bound = 1) {
  variable <- rlang::enquo(variable)
  raw_vector = .data %>% dplyr::pull(!! variable)
  additional_noise <- runif(n = length(raw_vector), min = -bound, max = bound)
  .data %>%
    dplyr::mutate((!! variable) := (!! variable) + additional_noise)
}

estimate_perturbed_slope <- function(.data, coeff = c("perturbed", "(Intercept)"), perturb_fn = perturb) {
  coeff <- match.arg(coeff)
  .data %>%
    perturb_fn() %>%
    lm(formula = y ~ perturbed) %>%
    magrittr::use_series(coefficients) %>% magrittr::extract(coeff)
}
######################
