
# Read in libraries
library(tidyverse)
library(mvtnorm)

source("../utils.R")

get_expected_value <- function(value_func, exec_err_post_mean,
                               fault_value = -1) {

  # Create full grid
  grid_df <- expand.grid(x_serve_bounce = seq(0, 11.9, by = 0.1),
                         y_serve_bounce = seq(-5.5, 5.5, by = 0.1)) |>
    mutate(x_serve_bounce = round(x_serve_bounce, 1),
           y_serve_bounce = round(y_serve_bounce, 1),
           v_hat = fault_value)

  # Extract Parameters (exec_err_post_mean already filtered to serve_num and court_side)
  court_side <- exec_err_post_mean$court_side[1]
  row_W <- exec_err_post_mean |> filter(serve_dir == "Wide")
  row_T <- exec_err_post_mean |> filter(serve_dir == "T")

  mu_W   <- c(pull(row_W, mu_x), pull(row_W, mu_y))
  mu_T   <- c(pull(row_T, mu_x), pull(row_T, mu_y))
  sig_W  <- c(pull(row_W, tau_x), pull(row_W, tau_y))
  sig_T  <- c(pull(row_T, tau_x), pull(row_T, tau_y))
  corr_W <- pull(row_W, rho)
  corr_T <- pull(row_T, rho)
  t_W    <- pull(row_W, t)
  t_T    <- pull(row_T, t)

  # Clean and truncate value_func
  value_func <- value_func |>
    mutate(x_serve_bounce = round(x_serve_bounce, 1),
           y_serve_bounce = round(y_serve_bounce, 1)) |>
    rowwise() |>
    mutate(t_interp = {
      interp <- if (court_side == "DeuceCourt") {
        interpolate_deuce(x_serve_bounce, y_serve_bounce,
                          mu_W, mu_T, sig_W, sig_T, corr_W, corr_T, t_W, t_T)
      } else {
        interpolate_ad(x_serve_bounce, y_serve_bounce,
                       mu_W, mu_T, sig_W, sig_T, corr_W, corr_T, t_W, t_T)
      }
      interp$t
    }) |>
    ungroup() |>
    mutate(v_hat = ifelse(x_serve_bounce < t_interp, fault_value, v_hat)) |>
    select(x_serve_bounce, y_serve_bounce, v_hat)

  # Merge into grid
  grid_df <- grid_df |>
    left_join(value_func, by = c("x_serve_bounce", "y_serve_bounce"), suffix = c("", ".new")) |>
    mutate(v_hat = ifelse(!is.na(v_hat.new), v_hat.new, v_hat)) |>
    select(x_serve_bounce, y_serve_bounce, v_hat)

  # Create an empty vector to store results
  ev_list <- numeric(nrow(grid_df))

  # Convert full grid to matrix once
  grid_mat <- as.matrix(grid_df[, c("x_serve_bounce", "y_serve_bounce")])
  v_vals <- grid_df$v_hat

  for (i in seq_len(nrow(grid_df))) {
    # Extract target (aiming location)
    x_serve_bounce <- grid_df$x_serve_bounce[i]
    y_serve_bounce <- grid_df$y_serve_bounce[i]

    # Interpolate parameters at this aim point
    if (court_side == "DeuceCourt") {
      interp <- interpolate_deuce(x_serve_bounce, y_serve_bounce, mu_W, mu_T, sig_W, sig_T,
                                  corr_W, corr_T, t_W, t_T)
    } else {
      interp <- interpolate_ad(x_serve_bounce, y_serve_bounce, mu_W, mu_T, sig_W, sig_T,
                               corr_W, corr_T, t_W, t_T)
    }

    # Mean and covariance matrix
    mu <- c(x_serve_bounce, y_serve_bounce)
    sig <- interp$sig
    rho <- interp$corr

    cov_mat <- matrix(c(
      sig[1]^2, rho * sig[1] * sig[2],
      rho * sig[1] * sig[2], sig[2]^2
    ), nrow = 2)

    # Compute densities over full grid
    density <- dmvnorm(x = grid_mat, mean = mu, sigma = cov_mat)

    # Compute expected value
    ev_hat <- sum(density * v_vals) / sum(density)

    # Store result
    ev_list[i] <- ev_hat
  }

  # Combine into final dataframe
  grid_df |> mutate(ev_hat = ev_list)

}

get_contours = function(server_name) {

  value_all = readRDS("../reward_surface/reward_surface_new.rds") %>% 
    filter(server_name == !!server_name)

  exec_err_post_draws <- readRDS("../execution_error/all_players.rds") %>% 
    filter(server_name == !!server_name)

  results <- list()

  for (i in 1:100) {

    cat(i, "\n")

    exec_err_post_mean = exec_err_post_draws |>
      filter(draw == i)

    for (court_side in c("DeuceCourt", "AdCourt")) {

      for (serve_num in c(2, 1)) {

        value_obj = value_all |>
          filter(court_side == !!court_side,
                 serve_num == !!serve_num) |>
          select(x_serve_bounce, y_serve_bounce, v_hat)

        if (serve_num == 1) {
          fault_value = exec_err_post_draws |>
            mutate(x_serve_bounce = round(mu_x, 1),
                   y_serve_bounce = round(mu_y, 1)) |>
            filter(serve_num == 2,
                   court_side == !!court_side,
                   draw == i) |>
            left_join(ev_df, by = c("x_serve_bounce", "y_serve_bounce")) |>
            slice_max(ev_hat) |>
            pull(ev_hat)

        } else {
          fault_value = -1
        }

        ev_df <- get_expected_value(
          value_obj,
          exec_err_post_mean |> filter(serve_num == !!serve_num, court_side == !!court_side),
          # fault_value = ifelse(serve_num == 2, -1, max(ev_df$ev_hat))
          fault_value = fault_value
        )

        ev_df <- ev_df |>
          mutate(
            serve_num = serve_num,
            court_side = court_side,
            draw = i
          )

        results[[length(results) + 1]] <- ev_df |>
          mutate(serve_dir = ifelse(abs(y_serve_bounce) > 2, "Wide", "T")) |>
          group_by(serve_num, court_side, serve_dir, draw) |>
          slice_max(ev_hat, n = 1) |>
          ungroup()

      }

    }
  }

  combined_df <- bind_rows(results)
  saveRDS(combined_df, paste0("../optimums/contours", server_name, ".rds"))

}

# get_contours("N.DJOKOVIC")

ev_df = readRDS("../optimums/optimums.rds")

optimal_targets = ev_df %>% 
  mutate(
    serve_dir = ifelse(abs(y_serve_bounce) > 2, "Wide", "T"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
    serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
  ) %>% 
  group_by(server_name, serve_num, court_side, serve_dir) |>
  slice_max(ev_hat)

players <- optimal_targets %>% 
  filter(abs(y_serve_bounce) == 2) %>% 
  pull(server_name) %>% 
  unique()

print(players)

for (player in players) {
  get_contours(player)
}

