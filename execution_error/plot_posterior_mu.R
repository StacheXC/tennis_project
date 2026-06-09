
library(tidyverse)
library(ggthemes)

source("tennis_project/utils.R")

plot_posterior_mu = function(server_name, exec_err_fit) {
  
  mu_df <- exec_err_fit %>%
    filter(server_name == !!server_name) %>% 
    select(draw, serve_num, court_side, serve_dir, mu_x, mu_y) %>%
    mutate(
      spot = case_when(
        court_side == "DeuceCourt" & serve_dir == "Wide" ~ "deuce wide",
        court_side == "DeuceCourt" & serve_dir == "T"    ~ "deuce tee",
        court_side == "AdCourt"    & serve_dir == "T"    ~ "ad tee",
        court_side == "AdCourt"    & serve_dir == "Wide" ~ "ad wide"
      ),
      spot = factor(spot, levels = c("deuce wide", "deuce tee", "ad tee", "ad wide")),
      serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
      court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
      court_side = factor(court_side, levels = c("Deuce Court", "Ad Court"))
    )
  
  ggplot() +
    geom_halfcourt() +
    geom_density_2d(
      data = mu_df,
      aes(x = mu_x, y = mu_y, color = spot),
      contour_var = "ndensity",
      breaks = c(0.05)
    ) +
    geom_density_2d_filled(
      data = mu_df, 
      aes(x = mu_x, y = mu_y, fill = spot),
      contour_var = "ndensity", # normalized density 0-1
      breaks = c(0.05, 1),
      alpha = 0.2) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    labs(x = "", y = "", title = server_name) +
    coord_equal() +
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

exec_err_fit = readRDS("tennis_project/execution_error/execution_error.rds")

plot_posterior_mu(server_name, exec_err_fit)
