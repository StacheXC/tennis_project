
library(tidyverse)

source("tennis_project/utils.R")

illustrate_data = function(server_name, pbp_df) {
  
  pbp_df = pbp_df %>% 
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1 | court_side == "DeuceCourt"
    ) %>% 
    mutate(
      x_serve_bounce = ifelse(
        x_serve_bounce < 3, runif(n(), -.2, 0), x_serve_bounce
      ),
      serve_num = ifelse(
        serve_num == 1, "1st Serve", "2nd Serve"
      ),
      court_side = ifelse(
        court_side == "DeuceCourt", "Deuce Court", "Ad Court"
      ),
      court_side = factor(
        court_side, levels = c("Deuce Court", "Ad Court")
      )
    )
  
  ggplot() + 
    geom_halfcourt() +
    geom_point(data = pbp_df,
               aes(x = x_serve_bounce, y = y_serve_bounce),
               alpha = .75,
               size = .75) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    labs(x = "", y = "", title = server_name) +
    coord_equal() +
    theme_minimal() +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          strip.text.y.left = element_text(angle = 0),
          plot.title = element_text(hjust = 0.5))

}

server_name = "N.DJOKOVIC"

pbp_df = readRDS("tennis_project/data/pbp_df.rds")

illustrate_data(server_name, pbp_df)
