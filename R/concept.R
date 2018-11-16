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




