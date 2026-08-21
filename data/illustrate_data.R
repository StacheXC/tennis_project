
library(tidyverse)
library(mvtnorm)
library(ellipse)
library(ggpattern)
library(ggthemes)

source("tennis_project/utils.R")

illustrate_data = function(server_name) {
  
  returner_locs <- data.frame(
    court = c("DeuceCourt", "DeuceCourt", "AdCourt", "AdCourt"),
    serve = c(1, 2, 1, 2),
    ret_x = c(13.59, 12.91, 13.72, 12.80),
    ret_y = c(3.64, 3.48, -3.47, -3.29)
  )
  
  pbp_df = readRDS("tennis_project/data/pbp_df.rds") %>%
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1 | court_side == "DeuceCourt",
      serve_num == 1 | x_serve_bounce < 8,
      serve_num == 1 | y_serve_bounce > -0.25 | court_side == "AdCourt",
      serve_num == 1 | y_serve_bounce < 0.25 | court_side == "DeuceCourt",
      serve_num == 1 | abs(y_serve_bounce) < 4.5
    ) %>% 
    mutate(
      x_serve_bounce = ifelse(
        x_serve_bounce < 3, runif(n(), -.2, 0), x_serve_bounce
      ),
      serve_num = ifelse(
        serve_num == 1, "1st Serve", "2nd Serve"
      ),
      court_side = ifelse(
        court_side == "DeuceCourt", "Deuce\nCourt", "Ad\nCourt"
      ),
      court_side = factor(
        court_side, levels = c("Deuce\nCourt", "Ad\nCourt")
      ),
      point = point_winner_id == server_id
    ) %>% 
    mutate(
      point = point & 
        x_serve_bounce > 3 &
        x_serve_bounce < 6.4 &
        abs(y_serve_bounce) < 4.11 &
        (y_serve_bounce > 0 | court_side == "Ad\nCourt") &
        (y_serve_bounce < 0 | court_side == "Deuce\nCourt") &
        serve_speed_kph != 0 &
        rally_length != 0 &
        !is_fault
    ) %>% 
    mutate(
      Point = factor(point, levels = c(TRUE, FALSE))
    )
  
  ggplot() + 
    geom_halfcourt() +
    geom_point(data = pbp_df,
               aes(x = x_serve_bounce, y = y_serve_bounce, color = Point),
               alpha = .5,
               size = .5) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    labs(x = "", y = "", title = server_name) +
    coord_equal() +
    scale_color_manual(values = c("red", "blue")) +
    scale_shape_manual(values = c(1, 4)) +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          plot.title = element_text(hjust = 0.5),
          legend.position = "bottom")

}

server_name = "N.DJOKOVIC"

illustrate_data(server_name)


