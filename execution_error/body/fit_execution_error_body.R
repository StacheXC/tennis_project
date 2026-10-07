
library(tidyverse)
library(rstan)

pbp_df <- readRDS("../data/pbp_df.rds") %>%
  filter(
    !is.na(server_name),
    str_detect(match_id, "australian"),
    x_serve_bounce > 3 | error_type == "Net Error",
    x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
    x_serve_bounce < 9,
    abs(y_serve_bounce) < 5.48,
    y_serve_bounce > -1 | court_side == "AdCourt",
    y_serve_bounce < 1  | court_side == "DeuceCourt",
    serve_num == 1 | x_serve_bounce < 8,
    serve_num == 1 | y_serve_bounce > -0.25 | court_side == "AdCourt",
    serve_num == 1 | y_serve_bounce < 0.25 | court_side == "DeuceCourt",
    serve_num == 1 | abs(y_serve_bounce) < 4.5
  ) %>%
  mutate(
    court_side = ifelse(court_side == "DeuceCourt", 1, 2)
  )

fit_execution_error_body2 <- function(pbp_df) {

  player_levels <- sort(unique(pbp_df$server_name))
  N_player      <- length(player_levels)

  pbp_df <- pbp_df %>%
    mutate(player_idx = as.integer(factor(server_name, levels = player_levels)))

  df_obs  <- pbp_df %>% filter(x_serve_bounce > 3)
  df_cens <- pbp_df %>% filter(x_serve_bounce < 3)

  z_obs        <- df_obs %>% select(x_serve_bounce, y_serve_bounce) %>% as.matrix()
  z_ind_player <- df_obs$player_idx
  z_ind_serve  <- df_obs$serve_num
  z_ind_court  <- df_obs$court_side
  N_obs        <- nrow(z_obs)

  c_ind_player <- df_cens$player_idx
  c_ind_serve  <- df_cens$serve_num
  c_ind_court  <- df_cens$court_side
  N_cens       <- nrow(df_cens)

  # t_upper[N_player, N_serve, N_court, 3]: all three directions share the same
  # per-player min observed x (d=1 T, d=2 body, d=3 Wide)
  t_upper_base <- df_obs %>%
    group_by(player_idx, serve_num, court_side) %>%
    summarise(min_x = min(x_serve_bounce), .groups = "drop")

  t_upper <- array(3.0, dim = c(N_player, 2, 2, 3))

  for (row in seq_len(nrow(t_upper_base))) {
    p <- t_upper_base$player_idx[row]
    s <- t_upper_base$serve_num[row]
    c <- t_upper_base$court_side[row]
    t_upper[p, s, c, ] <- t_upper_base$min_x[row]
  }

  stan_data <- list(
    N_obs        = N_obs,
    N_cens       = N_cens,
    N_player     = N_player,
    N_serve      = 2L,
    N_court      = 2L,
    z_obs        = z_obs,
    z_ind_player = z_ind_player,
    z_ind_serve  = z_ind_serve,
    z_ind_court  = z_ind_court,
    t_upper      = t_upper,
    c_ind_player = c_ind_player,
    c_ind_serve  = c_ind_serve,
    c_ind_court  = c_ind_court
  )

  model <- stan_model("../execution_error/execution_error.stan")

  fit <- sampling(
    model,
    data   = stan_data,
    seed   = 42,
    chains = 1,
    iter   = 600,
    warmup = 300
  )

  draws_list <- rstan::extract(fit)
  n_draws    <- dim(draws_list$mu)[1]

  # All three directions live in the same arrays:
  #   mu, tau: [n_draws, N_player, N_serve, N_court, 3, 2]
  #   rho, t, theta: [n_draws, N_player, N_serve, N_court, 3]
  # d=1: lower-y cluster (T on Deuce, Wide on Ad)
  # d=2: body (both courts)
  # d=3: higher-y cluster (Wide on Deuce, T on Ad)

  base_grid <- expand.grid(
    draw       = 1:n_draws,
    player     = 1:N_player,
    serve_num  = 1:2,
    court_side = 1:2,
    dir_idx    = 1:3,
    KEEP.OUT.ATTRS = FALSE
  )

  # mu covers all 3 directions: [n_draws, N_player, N_serve, N_court, 3, 2]
  mu_df <- expand.grid(
    draw       = 1:n_draws,
    player     = 1:N_player,
    serve_num  = 1:2,
    court_side = 1:2,
    dir_idx    = 1:3,
    coord      = 1:2,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    mutate(
      value = as.vector(draws_list$mu),
      coord = ifelse(coord == 1, "mu_x", "mu_y")
    ) %>%
    pivot_wider(names_from = coord, values_from = value)

  # tau covers only T and Wide: [n_draws, N_player, N_serve, N_court, 2, 2]
  # Internal array indices 1 and 2 map to base_grid dir_idx 1 (lower-y) and 3 (higher-y)
  tau_tw_df <- expand.grid(
    draw       = 1:n_draws,
    player     = 1:N_player,
    serve_num  = 1:2,
    court_side = 1:2,
    dir_idx    = 1:2,
    coord      = 1:2,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    mutate(
      value   = as.vector(draws_list$tau),
      dir_idx = ifelse(dir_idx == 1, 1L, 3L),
      coord   = ifelse(coord == 1, "tau_x", "tau_y")
    ) %>%
    pivot_wider(names_from = coord, values_from = value)

  # Body tau is separate: [n_draws, N_player, N_serve, N_court]
  tau_body_df <- expand.grid(
    draw       = 1:n_draws,
    player     = 1:N_player,
    serve_num  = 1:2,
    court_side = 1:2,
    KEEP.OUT.ATTRS = FALSE
  ) %>%
    mutate(
      tau_x   = as.vector(draws_list$tau_body_x),
      tau_y   = as.vector(draws_list$tau_body_y),
      dir_idx = 2L
    )

  tau_df <- bind_rows(tau_tw_df, tau_body_df)

  draws_processed <- base_grid %>%
    mutate(
      rho   = as.vector(draws_list$rho),
      t     = as.vector(draws_list$t),
      theta = as.vector(draws_list$theta)
    ) %>%
    left_join(mu_df,  by = c("draw", "player", "serve_num", "court_side", "dir_idx")) %>%
    left_join(tau_df, by = c("draw", "player", "serve_num", "court_side", "dir_idx")) %>%
    mutate(
      serve_dir = case_when(
        dir_idx == 2                    ~ "Body",
        court_side == 1 & dir_idx == 1 ~ "T",
        court_side == 1 & dir_idx == 3 ~ "Wide",
        court_side == 2 & dir_idx == 1 ~ "Wide",
        court_side == 2 & dir_idx == 3 ~ "T"
      ),
      court_side  = ifelse(court_side == 1, "DeuceCourt", "AdCourt"),
      server_name = player_levels[player]
    ) %>%
    select(-player, -dir_idx)

  list(
    draws   = draws_processed,
    summary = summary(fit)$summary,
    sampler = rstan::get_sampler_params(fit, inc_warmup = FALSE)
  )

}

draws_pooled <- fit_execution_error_body2(pbp_df)

saveRDS(draws_pooled, "../execution_error/execution_error.rds")
