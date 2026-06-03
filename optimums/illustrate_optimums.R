
library(tidyverse)

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

illustrate_optimums = function(server_name) {
  
  ev_df <- readRDS(paste0("tennis_project/optimums/players/", server_name, ".rds"))
  
  ev_df <- ev_df |>
    mutate(serve_dir = ifelse(abs(y_serve_bounce) > 2, "Wide", "T"),
           court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
           court_side = factor(court_side, levels = c("Deuce", "Ad")),
           serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve"))
  
  optimal_aim_points <- ev_df |>
    group_by(serve_dir, court_side, serve_num) |>
    slice_max(ev_hat)
  
  ggplot(ev_df, aes(x = x_serve_bounce, y = y_serve_bounce)) +
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
    geom_point(data = optimal_aim_points, aes(x = x_serve_bounce, y = y_serve_bounce),
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

server_name = "N.DJOKOVIC"

illustrate_optimums(server_name)


