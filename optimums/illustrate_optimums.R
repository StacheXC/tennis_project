
library(tidyverse)

source("tennis_project/utils.R")

# Diagonal boundary geometry (from action_space.R)
serve_x       <- -11.887
returner_du_x <- 13.37
returner_du_y <- 3.4
slope_mid     <- returner_du_y / (returner_du_x - serve_x)

find_local_maxima <- function(ev_df, grid_res = 0.1) {
  offsets <- list(
    c(-1, -1), c(-1,  0), c(-1,  1),
    c( 0, -1),             c( 0,  1),
    c( 1, -1), c( 1,  0), c( 1,  1)
  )

  ev_df %>%
    group_by(server_name, serve_num, court_side) %>%
    group_modify(function(data, keys) {
      lookup <- setNames(
        data$ev_hat,
        paste(round(data$x_serve_bounce, 1), round(data$y_serve_bounce, 1))
      )

      is_max <- rep(TRUE, nrow(data))
      for (off in offsets) {
        nbr_key <- paste(round(data$x_serve_bounce + off[1] * grid_res, 1),
                         round(data$y_serve_bounce + off[2] * grid_res, 1))
        nbr_ev  <- lookup[nbr_key]
        nbr_ev[is.na(nbr_ev)] <- -Inf
        is_max  <- is_max & (data$ev_hat > nbr_ev)
      }

      data[is_max, ] %>% select(x_serve_bounce, y_serve_bounce, ev_hat)
    }) %>%
    ungroup()
}

illustrate_optimums <- function(server_name) {

  ev_df <- readRDS("tennis_project/optimums/optimums.rds") |>
    filter(server_name == !!server_name)

  local_maxima <- find_local_maxima(ev_df %>%
    filter(x_serve_bounce <= 6.4, abs(y_serve_bounce) <= 4.115)) %>%
    mutate(
      serve_dir = case_when(
        court_side == "DeuceCourt" &
          y_serve_bounce >  slope_mid * (x_serve_bounce - serve_x) ~ "Wide",
        court_side == "DeuceCourt" &
          y_serve_bounce <= slope_mid * (x_serve_bounce - serve_x) ~ "T",
        court_side == "AdCourt" &
          y_serve_bounce < -slope_mid * (x_serve_bounce - serve_x) ~ "Wide",
        court_side == "AdCourt" &
          y_serve_bounce >= -slope_mid * (x_serve_bounce - serve_x) ~ "T"
      )
    )

  optimal_local_maxima <- local_maxima %>%
    group_by(serve_num, court_side, serve_dir) %>%
    slice_max(ev_hat, n = 1) %>%
    ungroup()

  ev_plot <- ev_df %>%
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
      court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  optimal_local_maxima <- optimal_local_maxima %>%
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
      court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  local_maxima <- local_maxima %>%
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
      court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  boundary_lines <- tibble(
    x    = 0,
    xend = 6.4,
    y    = c( slope_mid * (0   - serve_x),  -slope_mid * (0   - serve_x)),
    yend = c( slope_mid * (6.4 - serve_x),  -slope_mid * (6.4 - serve_x)),
    court_side = factor(c("Deuce Court", "Ad Court"),
                        levels = c("Deuce Court", "Ad Court"))
  )

  ggplot(ev_plot, aes(x = x_serve_bounce, y = y_serve_bounce)) +
    geom_tile(aes(fill = ev_hat)) +
    geom_halfcourt() +
    # geom_segment(data = boundary_lines,
    #              aes(x = x, xend = xend, y = y, yend = yend),
    #              inherit.aes = FALSE,
    #              linetype = "dashed", color = "black", linewidth = 0.5) +
    # geom_point(data = local_maxima,
    #            aes(x = x_serve_bounce, y = y_serve_bounce),
    #            shape = 1, size = 1, color = "gray40") +
    geom_point(data = optimal_local_maxima,
               aes(x = x_serve_bounce, y = y_serve_bounce),
               shape = 4, size = 1, stroke = 1.2) +
    scale_fill_gradient2(
      low = "blue", mid = "white", high = "red",
      midpoint = 0, name = "Expected Aim Value", limits = c(-1, 1)
    ) +
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

illustrate_optimums(server_name)

