
library(tidyverse)

source("tennis_project/utils.R")

illustrate_reward_surface = function(server_name, value_all) {
  
  value_all = value_all %>% 
    filter(
      server_name == !!server_name,
      x_serve_bounce > 3
    ) %>% 
    mutate(
      court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
      court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
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
          legend.position = "bottom",
          legend.justification = c(0.5, 0.5),
          legend.direction = "horizontal",
          strip.text.y.left = element_text(angle = 0),
          legend.title = element_text(size = 6, face = "bold",
                                      hjust = 0.5),
          legend.text = element_text(size = 6),
          strip.text = ggtext::element_markdown(size = 8, lineheight = 1.1),
          legend.background = element_rect(fill = "gray95", color = NA),
          plot.title = element_text(hjust = 0.5, size = 12))
  
}

server_name = "N.DJOKOVIC"

value_all = readRDS("tennis_project/reward_surface/reward_surface.rds")

illustrate_reward_surface(server_name, value_all)
