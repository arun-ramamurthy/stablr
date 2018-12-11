#' @import magrittr

###################
#### STABILITY ####

#' @export
ES.lm <- function(.data, lm_formula, V = 100, multiperturb_fn = multiperturb_norm, ...) {
  perturb_cols <- rlang::ensyms(...)
  simulate_single_yhat <- function() {
    .data %>%
      multiperturb_fn(... = !!! perturb_cols) %>%
      lm(formula = lm_formula) %>%
      predict()
  }

  V_yhats <- purrr::rerun(V, simulate_single_yhat())
  mean_yhat <- purrr::invoke(cbind, V_yhats) %>% rowMeans()
  T_hat <-
    V_yhats %>%
    purrr::map_dbl(~ norm(. - mean_yhat, type = "2")^2) %>%
    mean()
  ES <- T_hat / norm(mean_yhat, type = "2")^2
  ES
}
###################

#########################
#### DATA GENERATION ####

#' @export
generate_uniform_vector <- function(n = 150, bound = 10) {
  runif(n, -bound, bound)
}

#' @export
generate_normal_vector <- function(n = 150, sd = 5) {
  rnorm(n, sd = sd)
}

#' @export
generate_covariates <- function(n = 150, k = 3,
                                covariate_generation_fn = generate_uniform_vector) {
  covariates <-
    purrr::rerun(k, purrr::partial(covariate_generation_fn, n = n)()) %>%
    dplyr::bind_cols()
  covariates %>%
    dplyr::rename_all(dplyr::funs(stringr::`str_sub<-`(string = ., value = "x_", start = 1, end = 1)))
}

#' @export
augment_y <- function(.data, y_formula, y_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  n <- nrow(.data)
  y_formula <- rlang::enquo(y_formula)
  .data %>%
    dplyr::mutate(y := (!! y_formula) + purrr::partial(y_noise_fn, n = n)())
}

#' @export
generate_data <- function(n = 150, k = 3,
                          covariate_generation_fn = generate_uniform_vector,
                          y_formula, y_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  y_formula <- rlang::enquo(y_formula)
  generate_covariates(n, k, covariate_generation_fn) %>%
    augment_y(!! y_formula, y_noise_fn)
}

#' @export
augment_collinearity <- function(.data,
                                 new_col_formula, new_col_noise_fn = purrr::partial(generate_normal_vector, sd = 1)) {
  new_col_formula <- rlang::enquo(new_col_formula)
  n <- nrow(.data)
  integer <- 1
  while ((paste('x_', integer, sep = '')) %in% names(.data)){
    integer %<>% magrittr::add(1)
  }
  .data %>%
    dplyr::mutate((!! paste('x_', integer, sep = '')) :=
                    (!! new_col_formula) + purrr::partial(new_col_noise_fn, n = n)())
}

#' @export
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
perturb <- function(.data, variable, base_formula, noise_fn) {
  variable <- rlang::ensym(variable)
  base_formula <- rlang::enquo(base_formula)
  if(is.null(base_formula))
    base_formula <- variable
  additional_noise <- noise_fn(n = nrow(.data))
  .data %>%
    dplyr::mutate((!! variable) := (!! base_formula) + additional_noise)

}

perturb_norm <- function(.data, variable = x, sd = 1) {
  variable <- rlang::ensym(variable)
  raw_vector <- .data %>% dplyr::pull(!! variable)
  .data %>%
    dplyr::mutate((!! variable) := (!! variable) + additional_noise)
}

perturb_unif <- function(.data, variable = x, bound = 1) {
  variable <- rlang::ensym(variable)
  raw_vector = .data %>% dplyr::pull(!! variable)
  additional_noise <- runif(n = length(raw_vector), min = -bound, max = bound)
  .data %>%
    dplyr::mutate((!! variable) := (!! variable) + additional_noise)
}

multiperturb <- function(.data, perturb_fn, ...) {
  perturb_cols <- rlang::ensyms(...)
  perturbs <-
    perturb_cols %>%
    purrr::map(~ purrr::partial(perturb_fn, variable = !! .))
  multiperturb <-
    perturbs %>%
    purrr::reduce(purrr::compose)
  multiperturb(.data)
}

#' @export
multiperturb_norm <- function(.data, sd = 1, ...) {
  multiperturb(.data, purrr::partial(perturb_norm, sd = sd), ...)
}

#' @export
multiperturb_unif <- function(.data, bound = 1, ...) {
  multiperturb(.data, purrr::partial(perturb_unif, bound = bound), ...)
}

## (column, base_formula, noise) (.data, .vars, base_formula, noise_fn)
generate_perturbation_matrix <- function(.data, .vars, base_formula, noise_fn = perturb_unif) {
  selected_vars <- tidyselect::vars_select(names(.data), .vars)
  base_formula <- enquo(base_formula)
  data_frame(
    variable = selected_vars,
    base_formula = list(base_formula),
    noise_fn = list(noise_fn))
}

multiperturb_df <- function(.data, perturbation_matrix) {
  make_perturb_fn <- function(variable, base_formula, noise_fn) {
    variable <- ensym(variable)
    base_formula <- enquo(base_formula)
    purrr::partial(perturb, variable = !! variable, base_formula = !! base_formula, noise_fn = noise_fn)
  }
  perturbs <-
    perturbation_matrix %>%
    purrr::pmap(make_perturb_fn)
  multiperturb <-
    perturbs %>%
    purrr::reduce(purrr::compose)
  multiperturb(.data)
}
######################

silence <- function(n){rep(0, n)}
M <- iris %>% generate_perturbation_matrix(starts_with("Sepal"), Petal.Length/2, silence)
iris %>%
  perturb(Sepal.Length, Petal.Length/2, silence)
iris %>%
  multiperturb_df(M)
