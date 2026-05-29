# Read in libraries
library(tidyverse)
library(mvtnorm)

source("tennis_project/optimums/interpolate_sig_fig.R")

geom_halfcourt <- function()  {
  court_dat <- data.frame(
    x = c(0, 0, 11.887, 0, 0, 0, 0, 6.4),
    xend = c(11.887, 0, 11.887, 11.887, 11.887, 11.887, 6.4, 6.4),
    y = c(5.486, 5.486, 5.486, -5.486, 4.115, -4.115, 0, 4.115),
    yend = c(5.486, -5.486, -5.486, -5.486, 4.115, -4.115, 0, -4.115)
  )
  geom_segment(aes(x = x,
                   xend = xend,
                   y = y,
                   yend = yend),
               data = court_dat,
               color = "gray40")
}

get_expected_value <- function(value_func, exec_err_post_mean, 
                               serve_num = 2, 
                               court_side = "DeuceCourt", 
                               fault_value = -1) {
  
  # Create full grid
  grid_df <- expand.grid(x = seq(0, 11.9, by = 0.1), 
                         y = seq(-5.5, 5.5, by = 0.1)) |>
    mutate(x = round(x, 1), y = round(y, 1), v_hat = fault_value)
  
  # Extract Parameters
  row_W <- exec_err_post_mean |> 
    filter(serve_num == !!serve_num, court_side == !!court_side, serve_dir == "Wide")
  row_T <- exec_err_post_mean |> 
    filter(serve_num == !!serve_num, court_side == !!court_side, serve_dir == "T")
  
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
    mutate(x = round(x, 1), y = round(y, 1)) |>
    rowwise() |>
    mutate(t_interp = {
      interp <- if (court_side == "DeuceCourt") {
        interpolate_deuce(x, y, mu_W, mu_T, sig_W, sig_T, corr_W, corr_T, t_W, t_T)
      } else {
        interpolate_ad(x, y, mu_W, mu_T, sig_W, sig_T, corr_W, corr_T, t_W, t_T)
      }
      interp$t
    }) |>
    ungroup() |> 
    mutate(v_hat = ifelse(x < t_interp, fault_value, v_hat)) |>
    select(x, y, v_hat)
  
  # Merge into grid
  grid_df <- grid_df |>
    left_join(value_func, by = c("x", "y"), suffix = c("", ".new")) |>
    mutate(v_hat = ifelse(!is.na(v_hat.new), v_hat.new, v_hat)) |>
    select(x, y, v_hat)
  
  # Create an empty vector to store results
  ev_list <- numeric(nrow(grid_df))
  
  # Convert full grid to matrix once
  grid_mat <- as.matrix(grid_df[, c("x", "y")])
  v_vals <- grid_df$v_hat
  
  for (i in seq_len(nrow(grid_df))) {
    # Extract target (aiming location)
    x <- grid_df$x[i]
    y <- grid_df$y[i]
    
    # Interpolate parameters at this aim point
    if (court_side == "DeuceCourt") {
      interp <- interpolate_deuce(x, y, mu_W, mu_T, sig_W, sig_T, 
                                  corr_W, corr_T, t_W, t_T)
    }
    else {
      interp <- interpolate_ad(x, y, mu_W, mu_T, sig_W, sig_T, 
                               corr_W, corr_T, t_W, t_T)
    }
    
    # Mean and covariance matrix
    mu <- c(x, y)
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
  grid_df <- grid_df |> 
    mutate(ev_hat = ev_list)
  
  grid_df
  
}



get_optimums = function(server_name) {

  value_all = readRDS("tennis_project/reward_surface/reward_surface.rds") %>% 
    filter(server_name == !!server_name)

  # Get posterior distribution data
  exec_err_post_mean <- readRDS(paste0("tennis_project/execution_error/players/", server_name, ".rds")) %>%
    group_by(serve_num, court_side, serve_dir) %>%
    summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, t), mean), .groups = "drop")

  results <- list()

  for (court_side in c("DeuceCourt", "AdCourt")) {
    for (serve_num in c(2, 1)) {

      value_obj = value_all |>
        filter(court_side == !!court_side,
               serve_num == !!serve_num) |>
        rename(x = x_serve_bounce, y = y_serve_bounce) |>
        select(x, y, v_hat)
      
      if (serve_num == 1) {
        fault_value = exec_err_post_mean |>
          mutate(x = round(mu_x, 1),
                 y = round(mu_y, 1)) |>
          filter(serve_num == 2,
                 court_side == !!court_side) |>
          left_join(ev_df, by = c("x", "y")) |>
          slice_max(ev_hat) |>
          pull(ev_hat)

      } else {
        fault_value = -1
      }
      
      ev_df <- get_expected_value(
        value_obj,
        exec_err_post_mean,
        court_side = court_side,
        serve_num = serve_num,
        # fault_value = ifelse(serve_num == 2, -1, max(ev_df$ev_hat))
        fault_value = fault_value
      )
      
      ev_df <- ev_df |>
        mutate(
          serve_num = serve_num,
          court_side = court_side
        )
      
      results[[length(results) + 1]] <- ev_df
    }
  }
  
  combined_df <- bind_rows(results)
  saveRDS(combined_df, paste0("tennis_project/optimums/players/", server_name, ".rds"))
  
}

server_name = "N.DJOKOVIC"

get_optimums(server_name)



players <- c("N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV",
             "A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA")


for (player in players) {
  cat(player, "\n")
  get_optimums(player)
}





optimum_grapher = function(server_name) {
  
  ev_df <- readRDS(paste0("tennis_project/optimums/players/", server_name, ".rds"))
  
  ev_df <- ev_df |> 
    mutate(serve_dir = ifelse(abs(y) > 2, "Wide", "T"),
           court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
           court_side = factor(court_side, levels = c("Deuce", "Ad")),
           serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve"))
  
  optimal_aim_points <- ev_df |> 
    group_by(serve_dir, court_side, serve_num) |> 
    slice_max(ev_hat)
  
  ggplot(ev_df, aes(x = x, y = y)) +
    geom_tile(aes(fill = ev_hat)) +
    geom_halfcourt() +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Expected Aim Value",
      limits = c(-1, 1)
    ) +
    geom_point(data = optimal_aim_points, aes(x = x, y = y),
               shape = 4, size = 1, stroke = 1) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    coord_equal() +
    labs(x = "", y = "", title = server_name) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          legend.position = "bottom",
          strip.text.y.left = element_text(angle = 0),
          legend.title = element_text(size = 6, face = "bold"), 
          legend.text = element_text(size = 6),
          legend.background = element_rect(fill = "gray95", color = NA),
          plot.title = element_text(hjust = 0.5))
  
}

optimum_grapher(server_name)

