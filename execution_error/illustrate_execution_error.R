
library(tidyverse)
library(ellipse)
library(ggpattern)
library(ggthemes)

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

illustrate_execution_error = function(server_name) {

  pbp_df = read_csv("tennis_project/data/pbp_df.csv") %>%
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1 | court_side == "DeuceCourt"
    ) %>%
    mutate(
      x_serve_bounce = ifelse(
        x_serve_bounce < 3, runif(sum(x_serve_bounce < 3), -.2, 0), x_serve_bounce
      ),
      serve_num = ifelse(
        serve_num == 1, "1st Serve", "2nd Serve"
      ),
      court_side = ifelse(
        court_side == "DeuceCourt", "Deuce", "Ad"
      ),
      court_side = factor(court_side, levels = c("Deuce", "Ad"))
    )

  exec_err_post_mean <- readRDS(paste0("tennis_project/execution_error/players/", server_name, ".rds")) %>%
    group_by(serve_num, court_side, serve_dir) %>%
    summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, t), mean), .groups = "drop")

  ellipse_df <- exec_err_post_mean %>%
    pmap_dfr(function(serve_num, court_side, serve_dir, mu_x, mu_y, tau_x, tau_y, rho, t, ...) {

      # Build covariance matrix
      Sigma <- matrix(c(
        tau_x^2, rho * tau_x * tau_y,
        rho * tau_x * tau_y, tau_y^2
      ), nrow = 2)

      mu <- c(mu_x, mu_y)

      # Get 95% contour points
      ellipse_points <- as.data.frame(ellipse(Sigma, centre = mu,
                                              level = 0.95, npoints = 200))

      # Add grouping info to each point
      ellipse_points$serve_num <- serve_num
      ellipse_points$court_side <- court_side
      ellipse_points$serve_dir <- serve_dir
      ellipse_points$t <- t

      return(ellipse_points)
    })

  ellipse_df <- ellipse_df |>
    mutate(spot = case_when(
             court_side == "DeuceCourt" & serve_dir == "T"    ~ "deuce tee",
             court_side == "DeuceCourt" & serve_dir == "Wide" ~ "deuce wide",
             court_side == "AdCourt"    & serve_dir == "T"    ~ "ad tee",
             court_side == "AdCourt"    & serve_dir == "Wide" ~ "ad wide"
           ),
           spot = factor(spot, levels = c("deuce wide",
                                          "deuce tee",
                                          "ad tee",
                                          "ad wide")),
           serve_num = ifelse(
             serve_num == 1, "1st Serve", "2nd Serve"
           ),
           court_side = ifelse(
             court_side == "DeuceCourt", "Deuce", "Ad"
           ),
           court_side = factor(court_side, levels = c("Deuce", "Ad")))

  ellipse_obs_df <- ellipse_df %>%
    filter(x > t)

  ellipse_cens_df <- ellipse_df %>%
    filter(x <= t)

  ggplot() +
    geom_halfcourt() +
    geom_polygon(
      data = ellipse_obs_df,
      aes(x = x, y = y, fill = spot),
      alpha = 0.3
    ) +
    geom_polygon_pattern(
      data = ellipse_cens_df,
      aes(x = x, y = y, fill = spot),
      pattern = "stripe",
      pattern_angle = 45,
      pattern_density = 0.4,
      pattern_spacing = 0.02,
      pattern_fill = "white",
      alpha = .5,
      pattern_colour = NA
    ) +
    geom_point(data = pbp_df,
               aes(x = x_serve_bounce, y = y_serve_bounce),
               alpha = .75,
               size = .75) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_fill_colorblind() +
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

illustrate_execution_error(server_name)

