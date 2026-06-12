
library(tidyverse)
library(ggthemes)

source("tennis_project/utils.R")

illustrate_contours = function(server_name) {
  
  contours = readRDS(paste0("tennis_project/optimums/contours/", server_name, ".rds")) %>% 
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
      data = contours,
      aes(x = x_serve_bounce, y = y_serve_bounce, color = spot),
      contour_var = "ndensity",
      breaks = c(0.05)
    ) +
    geom_density_2d_filled(
      data = contours, 
      aes(x = x_serve_bounce, y = y_serve_bounce, fill = spot),
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

illustrate_contours(server_name)








contours = readRDS(paste0("tennis_project/optimums/contours/", server_name, ".rds")) %>% 
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
  geom_point(
    data = contours %>% filter(abs(y_serve_bounce) != 2),
    aes(x = x_serve_bounce, y = y_serve_bounce, color = spot),
    alpha = 0.75,
    size = 0.75
  ) +
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









observed_targets = readRDS("tennis_project/execution_error/execution_error.rds") %>% 
  filter(server_name == "N.DJOKOVIC") %>% 
  select(serve_num, court_side, serve_dir, mu_x, mu_y) %>% 
  rename(x_serve_bounce = mu_x, y_serve_bounce = mu_y)

optimal_targets = readRDS("tennis_project/optimums/contours.rds") %>% 
  select(x_serve_bounce, y_serve_bounce, serve_num, court_side, serve_dir)

ggplot() +
  geom_halfcourt() +
  geom_density_2d(
    data = observed_targets,
    aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir),
    contour_var = "ndensity",
    breaks = c(0.05)
  ) +
  geom_density_2d_filled(
    data = observed_targets, 
    aes(x = x_serve_bounce, y = y_serve_bounce, fill = serve_dir),
    contour_var = "ndensity", # normalized density 0-1
    breaks = c(0.05, 1),
    alpha = 0.2) +
  geom_density_2d(
    data = optimal_targets,
    aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir),
    contour_var = "ndensity",
    breaks = c(0.05)
  ) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  labs(x = "", y = "", title = "N.DJOKOVIC") +
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
