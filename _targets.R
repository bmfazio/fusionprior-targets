library(tarchetypes)
library(targets)
library(stantargets)
tar_option_set(packages =
                 c("dplyr", "tidyr", "purrr", "tidybayes",
                   "cmdstanr", "bayesplot", "effectFusion"))
source("R/functions.R")

list(
  tar_map(
    values = list(A = 1:10),
    tar_stan_mcmc_rep_summary(
      name = fit_piironen_toy_hs,
      stan_files = "stan/hs-no-intercept.stan",
      format_stan(sim_piironen_toy(A), 20),
      parallel_chains = 2,
      batches = 100,
      refresh = 0,
      show_messages = FALSE
      )
  ),
  tar_target(
    mse_table_piironen_toy,
    mse_summary(
      fit_piironen_toy_hs_1 %>% get_mse(1),
      fit_piironen_toy_hs_2 %>% get_mse(2),
      fit_piironen_toy_hs_3 %>% get_mse(3),
      fit_piironen_toy_hs_4 %>% get_mse(4),
      fit_piironen_toy_hs_5 %>% get_mse(5),
      fit_piironen_toy_hs_6 %>% get_mse(6),
      fit_piironen_toy_hs_7 %>% get_mse(7),
      fit_piironen_toy_hs_8 %>% get_mse(8),
      fit_piironen_toy_hs_9 %>% get_mse(9),
      fit_piironen_toy_hs_10 %>% get_mse(10)
    )
  ),
  tar_target(iter_pauger, 1:100),
  tar_target(data_pauger, sim_pauger(),
             pattern = map(iter_pauger), iteration = "list"),
  tar_target(fit_pauger,
             effectFusion(
               data_pauger$y, data_pauger$X, data_pauger$types,
               method = "SpikeSlab",
               mcmc = list(M = 10000, burnin = 5000),
               prior = list(r = 20000, G0 = 20)),
             pattern = map(data_pauger), iteration = "list"),
  tar_target(mse_table_pauger, mse_pauger(fit_pauger),
             pattern = map(fit_pauger))
)

# library(ggplot2)
# tar_load(mse_table_piironen_toy)
# mse_table_piironen_toy %>%
#   ggplot(aes(x = actual, y = mse)) + geom_line() + geom_point()
# tar_load(mse_table_pauger)
# mse_table_pauger %>% ggplot(aes(x = covariate, y = mse, group = covariate)) + geom_boxplot()