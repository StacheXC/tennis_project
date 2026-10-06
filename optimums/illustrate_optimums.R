
library(tidyverse)

source("tennis_project/utils.R")

illustrate_optimums <- function(server_name) {

  ev_df <- readRDS("tennis_project/optimums/optimums.rds") |>
    filter(server_name == !!server_name)

  optimal_local_maxima <- readRDS("tennis_project/optimums/targets.rds") %>% 
    filter(server_name == !!server_name)

  ev_plot <- ev_df %>%
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce\nCourt", "Ad\nCourt"),
      court_side = factor(court_side, levels = c("Deuce\nCourt", "Ad\nCourt")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  optimal_local_maxima <- optimal_local_maxima %>%
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce\nCourt", "Ad\nCourt"),
      court_side = factor(court_side, levels = c("Deuce\nCourt", "Ad\nCourt")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  ggplot(ev_plot, aes(x = x_serve_bounce, y = y_serve_bounce)) +
    geom_tile(aes(fill = ev_hat)) +
    geom_halfcourt() +
    geom_point(data = optimal_local_maxima,
               aes(x = x_serve_bounce, y = y_serve_bounce),
               shape = 4, size = 1, stroke = 1) +
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

server_name = "N.DJOKOVIC"

illustrate_optimums(server_name)
