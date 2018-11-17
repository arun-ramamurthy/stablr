###################
#### STABILITY ####
ES.lm <- function(.data, formula, V = 100, perturb_fn = perturb) { ## perturb can be user-defined specially
  simulate_single_yhat <- function() {
    .data %>%
      perturb_fn() %>%
      lm(formula = formula) %>%
      predict()
  }

  V_yhats <- purrr::rerun(V, simulate_single_yhat())
  mean_yhat <- purrr::invoke(cbind, V_yhats) %>% rowMeans()
  T_hat <-
    V_yhats %>%
    purrr::map_dbl(~ norm(. - mean_yhat, type = "2")) %>%
    mean()
  ES <-
    T_hat / norm(mean_yhat, type = "2")
  ES
}
###################

#########################
#### DATA GENERATION ####
generate_uniform_covariate <- function(n = 150, bound = 10) {
  runif(n, -bound, bound)
}

generate_normal_covariate <- function(n = 150, sd = 5) {
  rnorm(n, sd = sd)
}

generate_data <- function(n = 150, k = 3, generation_fn = generate_uniform_covariate, formula = x1 + x2 + x3) {
  formula <- rlang::enquo(formula)
  covariates <-
    purrr::rerun(k, purrr::partial(generation_fn, n = n)()) %>%
    dplyr::bind_cols()
  covariates %>%
    dplyr::rename_all(dplyr::funs(stringr::`str_sub<-`(string = ., value = "x", start = 1, end = 1))) %>%
    dplyr::mutate(y := !! formula)
}
#########################

######################
#### PERTURBATION ####
perturb <- function(.data, sigma = 1, variable = x) {
  variable <- enquo(variable)
  raw_vector = .data %>% pull(!! variable)
  additional_noise <- rnorm(n = length(raw_vector), sd = sigma)
  .data %>%
    mutate(perturbed := (!! variable) + additional_noise) %>%
    select_if(is.numeric) %>%
    select(- (!! variable))
}

perturb_unif <- function(.data, sigma = 1, variable = x) {
  variable <- enquo(variable)
  raw_vector = .data %>% pull(!! variable)
  additional_noise <- runif(n = length(raw_vector), min = -sigma, max = sigma)
  .data %>%
    mutate(additional_noise,
           perturbed := (!! variable) + additional_noise)
}

estimate_perturbed_slope <- function(.data, coeff = c("perturbed", "(Intercept)"), perturb_fn = perturb) {
  coeff <- match.arg(coeff)
  .data %>%
    perturb_fn() %>%
    lm(formula = y ~ perturbed) %>%
    use_series(coefficients) %>% extract(coeff)
}

add_collinearity <- function(.data) {
  #create more x columns that are just +c of each other
  .data['x_1'] = .data$x+1
  .data['x_2'] = .data$x+2
  .data['x_3'] = .data$x+3
  .data
}
######################