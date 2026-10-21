
library(tidyverse)

source("tennis_project/utils.R")

illustrate_reward_surface = function(server_name) {
  
  value_all = readRDS("tennis_project/reward_surface/reward_surface.rds") %>% 
    filter(
      server_name == !!server_name,
      x_serve_bounce > 3
    ) %>% 
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce\nCourt", "Ad\nCourt"),
      court_side = factor(court_side, levels = c("Deuce\nCourt", "Ad\nCourt")),
      serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )
  
  ggplot() +
    geom_halfcourt() +
    geom_raster(data = value_all,
                aes(x = x_serve_bounce, y = y_serve_bounce, fill = v_hat)) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      name = "Expected Bounce Value",
      limits = c(-1, 1)
    ) +
    labs(x = "", y = "", title = server_name) +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          plot.title = element_text(hjust = 0.5),
          legend.position = "right",
          legend.justification = c(0.5, 0.5),
          legend.direction = "vertical",
          legend.title = element_text(size = 6, face = "bold",
                                      hjust = 0.5),
          legend.text = element_text(size = 6),
          legend.background = element_rect(fill = "gray95", color = NA))
  
}

server_name = "N.DJOKOVIC"

illustrate_reward_surface(server_name)
