mse_pauger <- function(fitobj){
  betas <- list(
    b1 = c(0, 0, 1, 1, 2, 2, 4, 4),
    b2 = c(0, 0, 0, 0, 0, 0, 0, 0),
    b3 = c(0, 0,-2,-2),
    b4 = c(0, 0, 0, 0),
    b5 = c(0, 0, 1, 1, 1, 1,-2,-2),
    b6 = c(0, 0, 0, 0, 0, 0, 0, 0),
    b7 = c(0, 0, 2, 2),
    b8 = c(0, 0, 0, 0))
  
  coefmat <- matrix(rep(unlist(map(betas, `[`, -1)), 3000),
                    nrow = 3000, byrow = TRUE)
  
  tibble(
    covariate = rep(1:8, times = sapply(betas, length)-1),
    mse = (fitobj$refit$beta[,-1] - coefmat)**2 %>% apply(2, mean)
    ) %>%
    group_by(covariate) %>%
    summarise(mse = mean(mse))
}

sim_pauger <- function(){
  x_prob <- list(
    fact4 = c(0.1, 0.4, 0.2, 0.3),
    fact8 = c(0.1, 0.1, 0.2, 0.05, 0.2, 0.1, 0.2, 0.5)
  )
  
  betas <- list(
    b1 = c(0, 0, 1, 1, 2, 2, 4, 4),
    b2 = c(0, 0, 0, 0, 0, 0, 0, 0),
    b3 = c(0, 0,-2,-2),
    b4 = c(0, 0, 0, 0),
    b5 = c(0, 0, 1, 1, 1, 1,-2,-2),
    b6 = c(0, 0, 0, 0, 0, 0, 0, 0),
    b7 = c(0, 0, 2, 2),
    b8 = c(0, 0, 0, 0))
  
  X <- lapply(betas,
              function(z){
                lz <- length(z)
                sample(lz, 500, replace = TRUE,
                       prob = x_prob[[lz/4]])
              }) %>% as_tibble
  
  pmap(list(betas, X), function(x,y){x[y]}) %>%
    as_tibble %>%
    apply(1, sum) + 1 -> linpred
  
  y <- rnorm(500, linpred, 1)
  
  list(y = y, X = X,
       types = c(rep("o", 4), rep("n", 4)))
}

mse_summary <- function(...){
  bind_rows(...) %>%
    group_by(actual) %>%
    summarise(mse = mean(mse))
}

get_mse <- function(tib, val){
  tib %>% select(variable, mean) %>%
    filter(substr(variable, 1, 4) == "beta") %>%
    mutate(actual = rep(c(rep(val, 20), rep(0, 380)), 100),
           mse = (mean-actual)**2,
           actual = max(actual))
}

# Funciones de fitting ----
# Fit usando stan
fit_stan <- function(stan_model, input_data, nonzeros){
  data_ready <- format_stan(input_data, nonzeros)
  stan_call(data_ready, stan_model)
}

stan_call <- function(input_data, stan_model){
  stan_model$sample(data = input_data,
                    adapt_delta = 0.98,
                    parallel_chains = 2,
                    refresh = 0,
                    show_messages = FALSE)
}

format_stan <- function(input_data, p0){
  model.matrix(~ 1 + .,
               data = input_data %>% select(-y))[,-1] -> X
  
  list(
    n = nrow(X),
    d = ncol(X),
    x = X,
    y = input_data$y
  ) -> data
  
  list(
    scale_icept = 5,
    scale_global = p0/((data$d - p0)*sqrt(data$n)),
    nu_global = 1,
    nu_local = 1,
    slab_scale = 2,
    slab_df = 4
  ) -> hyperparameters
  
  c(data, hyperparameters)
}

# # Funciones de simulacion ----
# # Toy example (Piironen sec. 4.1.1)
sim_piironen_toy <- function(A, n_obs = 400){

  betas <- c(rep(list(c(0,A)), 20),
             rep(list(c(0,0)), 380))
  names(betas) <- paste0("b",1:400)

  X <- as_tibble(diag(n_obs)+1) %>% mutate_all(as.factor)
  colnames(X) <- names(betas)

  val_X <- value_categorical_X(X, betas)

  y <- rnorm(n_obs, mean = linear_predictor(0, val_X), sd = 1)

  bind_cols(y = y, X)
}

value_categorical_X <- function(X, coeffs){
  list(coeffs, X) %>%
    pmap(~.x[.y]) %>%
    as_tibble
}

linear_predictor <- function(intercept, val_cat_X){
  val_cat_X %>%
    rowwise %>%
    summarise(
      catbX_sum = sum(c_across(cols = everything()))
    ) %>%
    pull(catbX_sum) -> total_cat_X

  intercept + total_cat_X
}