
library(tidyverse)
library(rstan)
  
pbp_df <- readRDS("tennis_project/data/pbp_df.rds") %>% 
  filter(
    !is.na(server_name),
    str_detect(match_id, "australian"),
    x_serve_bounce > 3 | error_type == "Net Error",
    x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
    x_serve_bounce < 9,
    abs(y_serve_bounce) < 5.48,
    y_serve_bounce > -1 | court_side == "AdCourt",
    y_serve_bounce < 1 | court_side == "DeuceCourt"
  ) %>%
  mutate(
    court_side = ifelse(court_side == "DeuceCourt", 1, 2)
  )

fit_execution_error = function(pbp_df) {

# Create player index
player_levels <- sort(unique(pbp_df$server_name))
N_player      <- length(player_levels)

pbp_df <- pbp_df %>%
  mutate(player_idx = as.integer(factor(server_name, levels = player_levels)))

# Split observed and censored
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

# Build t_upper[N_player, 2, 2, 2] — direction dimension is replicated
t_upper_base <- df_obs %>%
  group_by(player_idx, serve_num, court_side) %>%
  summarise(min_x = min(x_serve_bounce), .groups = "drop")

t_upper <- array(3.0, dim = c(N_player, 2, 2, 2))

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
  N_direction  = 2L,
  z_obs        = z_obs,
  z_ind_player = z_ind_player,
  z_ind_serve  = z_ind_serve,
  z_ind_court  = z_ind_court,
  t_upper      = t_upper,
  c_ind_player = c_ind_player,
  c_ind_serve  = c_ind_serve,
  c_ind_court  = c_ind_court
)

model <- stan_model("tennis_project/execution_error/old/execution_error.stan")

fit <- sampling(
  model,
  data = stan_data,
  seed = 42,
  chains = 1,
  iter = 2000,
  warmup = 1000
)

draws_list <- rstan::extract(fit)
n_draws    <- dim(draws_list$mu)[1]

# Base grid for scalar parameters (rho, t, theta)
base_grid <- expand.grid(
  draw       = 1:n_draws,
  player     = 1:N_player,
  serve_num  = 1:2,
  court_side = 1:2,
  serve_dir  = 1:2
)

# Coord grid reused for both mu and tau
coord_grid <- expand.grid(
  draw       = 1:n_draws,
  player     = 1:N_player,
  serve_num  = 1:2,
  court_side = 1:2,
  serve_dir  = 1:2,
  coord      = 1:2
)

mu_df <- coord_grid %>%
  mutate(value = as.vector(draws_list$mu),
         coord = ifelse(coord == 1, "mu_x", "mu_y")) %>%
  pivot_wider(names_from = coord, values_from = value)

tau_df <- coord_grid %>%
  mutate(value = as.vector(draws_list$tau),
         coord = ifelse(coord == 1, "tau_x", "tau_y")) %>%
  pivot_wider(names_from = coord, values_from = value)

draws_processed <- base_grid %>%
  mutate(
    rho   = as.vector(draws_list$rho),
    t     = as.vector(draws_list$t),
    theta = as.vector(draws_list$theta)
  ) %>%
  left_join(mu_df,  by = c("draw", "player", "serve_num", "court_side", "serve_dir")) %>%
  left_join(tau_df, by = c("draw", "player", "serve_num", "court_side", "serve_dir")) %>%
  mutate(
    serve_dir = case_when(
      court_side == 1 & serve_dir == 1 ~ "T",
      court_side == 1 & serve_dir == 2 ~ "Wide",
      court_side == 2 & serve_dir == 1 ~ "Wide",
      court_side == 2 & serve_dir == 2 ~ "T"
    ),
    court_side = ifelse(court_side == 1, "DeuceCourt", "AdCourt")
  )

# Replace integer player index with player name and save as single file
draws_processed <- draws_processed %>%
  mutate(server_name = player_levels[player]) %>%
  select(-player)

draws_processed

}

draws_pooled = fit_execution_error(pbp_df)

saveRDS(draws_pooled, "tennis_project/execution_error/old/execution_error.rds")
