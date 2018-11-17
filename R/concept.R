###################
#### STABILITY ####
ES <- function(.data, V = 100, formula = y ~ ., perturb_fn = perturb) { ## perturb can be user-defined specially
  get_yhat_singleton <- function() {
    .data %>%
      perturb_fn() %>%
      lm(formula = formula) %>%
      predict()
  }

  V_yhats <- V %>% rerun(get_yhat_singleton())
  m <- V_yhats %>% {do.call(cbind, .)} %>% rowMeans()
  V_yhats %>%
    map_dbl(~ norm(. - m, type = "2")) %>%
    mean()
}
###################

#########################
#### DATA GENERATION ####
add_collinearity <- function(.data, formula, random) {
  formula <- rlang::enquo(formula)
  n <- nrow(.data)
  integer = sample(100, size = 1)
  while ((paste('x_', integer, sep = '')) %in% names(.data)){
    integer = sample(100, size = 1)
  }
  .data %>% mutate((!!paste('x_', integer, sep = '')) := (!!formula) + purrr::partial(random, n = n)())
}

add_multicoll <- function(.data, formula, random, k) {
  formula <- rlang::enquo(formula)
  add_coll <- purrr::partial(add_collinearity, formula = (!!formula), random = random)
  multicoll_fxn <- purrr::rerun(k, add_coll) %>% purrr::reduce(purrr::compose)
  multicoll_fxn(.data)
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

######################




