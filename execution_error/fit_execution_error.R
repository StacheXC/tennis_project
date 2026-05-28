
library(tidyverse)
# library(cmdstanr)
# library(posterior)
library(rstan)

server_name = "N.DJOKOVIC"

fit_execution_error = function(server_name) {
  
  pbp_df = read_csv("tennis_project/data/pbp_df.csv") %>% 
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1 | court_side == "DeuceCourt"
    ) %>% 
    mutate(
      court_side = ifelse(
        court_side == "DeuceCourt", 1, 2
      )
    )
  
  z_obs = pbp_df %>% 
    filter(x_serve_bounce > 3) %>% 
    select(x_serve_bounce, y_serve_bounce) %>% 
    as.matrix()
  
  z_ind_serve = pbp_df %>% 
    filter(x_serve_bounce > 3) %>% 
    pull(serve_num)
  
  z_ind_court = pbp_df %>% 
    filter(x_serve_bounce > 3) %>% 
    pull(court_side)
  
  N_obs = z_obs %>% 
    nrow()
  
  N_cens = nrow(pbp_df) - N_obs

  c_ind_serve = pbp_df %>%
    filter(x_serve_bounce < 3) %>%
    pull(serve_num)

  c_ind_court = pbp_df %>%
    filter(x_serve_bounce < 3) %>%
    pull(court_side)

  # technically, this is a logical error
  df_obs = pbp_df |>
    filter(x_serve_bounce > 3)
  
  t_upper_base <- unname(
    tapply(
      df_obs$x_serve_bounce,
      list(df_obs$serve_num, df_obs$court_side),
      min
    )
  )

  t_upper = array(NA_real_, dim = c(2, 2, 2))
  
  for (d in 1:2) {
    t_upper[, , d] = t_upper_base
  }
  
  stan_data <- list(
    N_obs = N_obs,
    N_cens = N_cens,
    z_obs = z_obs,
    t_upper = t_upper,
    z_ind_serve = z_ind_serve,
    z_ind_court = z_ind_court,
    c_ind_serve = c_ind_serve,
    c_ind_court = c_ind_court
  )
  
  model <- cmdstan_model("tennis_project/execution_error/execution_error.stan")
  
  fit <- model$sample(
    data = stan_data,
    seed = 42,
    chains = 1,
    parallel_chains = 1,
    iter_warmup = 1000,
    iter_sampling = 1000
  )
  
  draws_raw <- fit$draws(format = "draws_df") %>% as_tibble()

  mu_df <- draws_raw %>%
    select(.draw, matches("^mu\\[")) %>%
    pivot_longer(cols = -".draw", names_to = "variable", values_to = "value") %>%
    extract(variable, into = c("serve_num", "court_side", "serve_dir", "coord"),
            regex = "mu\\[(\\d+),(\\d+),(\\d+),(\\d+)\\]", convert = TRUE) %>%
    mutate(coord = ifelse(coord == 1, "mu_x", "mu_y")) %>%
    pivot_wider(names_from = coord, values_from = value)

  tau_df <- draws_raw %>%
    select(.draw, matches("^tau\\[")) %>%
    pivot_longer(cols = -".draw", names_to = "variable", values_to = "value") %>%
    extract(variable, into = c("serve_num", "court_side", "serve_dir", "coord"),
            regex = "tau\\[(\\d+),(\\d+),(\\d+),(\\d+)\\]", convert = TRUE) %>%
    mutate(coord = ifelse(coord == 1, "tau_x", "tau_y")) %>%
    pivot_wider(names_from = coord, values_from = value)

  rho_df <- draws_raw %>%
    select(.draw, matches("^rho\\[")) %>%
    pivot_longer(cols = -".draw", names_to = "variable", values_to = "rho") %>%
    extract(variable, into = c("serve_num", "court_side", "serve_dir"),
            regex = "rho\\[(\\d+),(\\d+),(\\d+)\\]", convert = TRUE)

  t_df <- draws_raw %>%
    select(.draw, matches("^t\\[")) %>%
    pivot_longer(cols = -".draw", names_to = "variable", values_to = "t") %>%
    extract(variable, into = c("serve_num", "court_side", "serve_dir"),
            regex = "t\\[(\\d+),(\\d+),(\\d+)\\]", convert = TRUE)

  draws_processed <- mu_df %>%
    full_join(tau_df, by = c(".draw", "serve_num", "court_side", "serve_dir")) %>%
    full_join(rho_df, by = c(".draw", "serve_num", "court_side", "serve_dir")) %>%
    full_join(t_df,   by = c(".draw", "serve_num", "court_side", "serve_dir")) %>%
    mutate(
      serve_dir = case_when(
        court_side == 1 & serve_dir == 1 ~ "T",
        court_side == 1 & serve_dir == 2 ~ "Wide",
        court_side == 2 & serve_dir == 1 ~ "Wide",
        court_side == 2 & serve_dir == 2 ~ "T"
      ),
      court_side = ifelse(court_side == 1, "DeuceCourt", "AdCourt")
    ) %>%
    rename(draw = .draw)

  saveRDS(draws_processed, paste0("tennis_project/execution_error/players/", server_name, ".rds"))
  
}

fit_execution_error_rstan = function(server_name) {

  pbp_df = read_csv("tennis_project/data/pbp_df.csv") %>%
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1 | court_side == "DeuceCourt"
    ) %>%
    mutate(
      court_side = ifelse(
        court_side == "DeuceCourt", 1, 2
      )
    )

  z_obs = pbp_df %>%
    filter(x_serve_bounce > 3) %>%
    select(x_serve_bounce, y_serve_bounce) %>%
    as.matrix()

  z_ind_serve = pbp_df %>%
    filter(x_serve_bounce > 3) %>%
    pull(serve_num)

  z_ind_court = pbp_df %>%
    filter(x_serve_bounce > 3) %>%
    pull(court_side)

  N_obs = z_obs %>%
    nrow()

  N_cens = nrow(pbp_df) - N_obs

  c_ind_serve = pbp_df %>%
    filter(x_serve_bounce < 3) %>%
    pull(serve_num)

  c_ind_court = pbp_df %>%
    filter(x_serve_bounce < 3) %>%
    pull(court_side)

  # technically, this is a logical error
  df_obs = pbp_df |>
    filter(x_serve_bounce > 3)

  t_upper_base <- unname(
    tapply(
      df_obs$x_serve_bounce,
      list(df_obs$serve_num, df_obs$court_side),
      min
    )
  )

  t_upper = array(NA_real_, dim = c(2, 2, 2))

  for (d in 1:2) {
    t_upper[, , d] = t_upper_base
  }

  stan_data <- list(
    N_obs = N_obs,
    N_cens = N_cens,
    z_obs = z_obs,
    t_upper = t_upper,
    z_ind_serve = z_ind_serve,
    z_ind_court = z_ind_court,
    c_ind_serve = c_ind_serve,
    c_ind_court = c_ind_court
  )

  model <- stan_model("tennis_project/execution_error/execution_error.stan")

  fit <- sampling(
    model,
    data = stan_data,
    seed = 42,
    chains = 1,
    iter = 2000,
    warmup = 1000
  )

  draws_list <- rstan::extract(fit)
  n_draws <- dim(draws_list$mu)[1]

  mu_df <- expand.grid(
    draw = 1:n_draws,
    serve_num = 1:2,
    court_side = 1:2,
    serve_dir = 1:2,
    coord = 1:2
  ) %>%
    mutate(value = as.vector(draws_list$mu),
           coord = ifelse(coord == 1, "mu_x", "mu_y")) %>%
    pivot_wider(names_from = coord, values_from = value)

  tau_df <- expand.grid(
    draw = 1:n_draws,
    serve_num = 1:2,
    court_side = 1:2,
    serve_dir = 1:2,
    coord = 1:2
  ) %>%
    mutate(value = as.vector(draws_list$tau),
           coord = ifelse(coord == 1, "tau_x", "tau_y")) %>%
    pivot_wider(names_from = coord, values_from = value)

  rho_df <- expand.grid(
    draw = 1:n_draws,
    serve_num = 1:2,
    court_side = 1:2,
    serve_dir = 1:2
  ) %>%
    mutate(rho = as.vector(draws_list$rho))

  t_df <- expand.grid(
    draw = 1:n_draws,
    serve_num = 1:2,
    court_side = 1:2,
    serve_dir = 1:2
  ) %>%
    mutate(t = as.vector(draws_list$t))

  draws_processed <- mu_df %>%
    full_join(tau_df, by = c("draw", "serve_num", "court_side", "serve_dir")) %>%
    full_join(rho_df, by = c("draw", "serve_num", "court_side", "serve_dir")) %>%
    full_join(t_df,   by = c("draw", "serve_num", "court_side", "serve_dir")) %>%
    mutate(
      serve_dir = case_when(
        court_side == 1 & serve_dir == 1 ~ "T",
        court_side == 1 & serve_dir == 2 ~ "Wide",
        court_side == 2 & serve_dir == 1 ~ "Wide",
        court_side == 2 & serve_dir == 2 ~ "T"
      ),
      court_side = ifelse(court_side == 1, "DeuceCourt", "AdCourt")
    )

  saveRDS(draws_processed, paste0("tennis_project/execution_error/players/", server_name, ".rds"))

}

fit_execution_error_rstan(server_name)

# players <- c("A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
#              "I.SWIATEK", "C.GAUFF", "E.SVITOLINA",
#              "N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
#              "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV")
# 
# for (player in players) {
#   fit_execution_error(player)
# }




