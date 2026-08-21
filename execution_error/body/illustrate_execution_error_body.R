
library(tidyverse)
library(mvtnorm)
library(ellipse)
library(ggpattern)
library(ggthemes)

source("tennis_project/utils.R")

illustrate_execution_error_body <- function(server_name) {

  pbp_df <- readRDS("tennis_project/data/pbp_df.rds") %>%
    filter(
      server_name == !!server_name,
      str_detect(match_id, "australian"),
      x_serve_bounce > 3 | error_type == "Net Error",
      x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
      x_serve_bounce < 9,
      abs(y_serve_bounce) < 5.48,
      y_serve_bounce > -1 | court_side == "AdCourt",
      y_serve_bounce < 1  | court_side == "DeuceCourt",
      serve_num == 1 | x_serve_bounce < 8,
      serve_num == 1 | y_serve_bounce > -0.25 | court_side == "AdCourt",
      serve_num == 1 | y_serve_bounce < 0.25 | court_side == "DeuceCourt",
      serve_num == 1 | abs(y_serve_bounce) < 4.5
    ) %>%
    mutate(
      x_serve_bounce = ifelse(x_serve_bounce < 3, runif(n(), -.2, 0), x_serve_bounce)
    )

  exec_err_post_mean <- readRDS("tennis_project/execution_error/body/execution_error_body.rds")$draws %>%
    filter(server_name == !!server_name) %>%
    group_by(serve_num, court_side, serve_dir) %>%
    summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, theta, t), mean), .groups = "drop")

  serve_dirs <- unique(exec_err_post_mean$serve_dir)

  params_wide <- exec_err_post_mean %>%
    select(-t) %>%
    pivot_wider(
      names_from  = serve_dir,
      values_from = c(mu_x, mu_y, tau_x, tau_y, rho, theta)
    )

  pbp_df <- pbp_df %>%
    left_join(params_wide, by = c("serve_num", "court_side")) %>%
    group_by(court_side, serve_num) %>%
    group_modify(function(data, keys) {
      if (any(is.na(data[[paste0("mu_x_", serve_dirs[1])]]))) {
        return(data %>% mutate(serve_dir = NA_character_))
      }

      z <- as.matrix(data[, c("x_serve_bounce", "y_serve_bounce")])
      p <- data[1, ]

      dens <- sapply(serve_dirs, function(d) {
        Sigma <- matrix(
          c(p[[paste0("tau_x_", d)]]^2,
            p[[paste0("rho_", d)]] * p[[paste0("tau_x_", d)]] * p[[paste0("tau_y_", d)]],
            p[[paste0("rho_", d)]] * p[[paste0("tau_x_", d)]] * p[[paste0("tau_y_", d)]],
            p[[paste0("tau_y_", d)]]^2),
          nrow = 2
        )
        p[[paste0("theta_", d)]] *
          dmvnorm(z, mean = c(p[[paste0("mu_x_", d)]], p[[paste0("mu_y_", d)]]), sigma = Sigma)
      })

      data %>% mutate(serve_dir = serve_dirs[apply(dens, 1, which.max)])
    }) %>%
    ungroup() %>%
    select(-matches("^(mu_x|mu_y|tau_x|tau_y|rho|theta)_"))

  ellipse_df <- exec_err_post_mean %>%
    pmap_dfr(function(serve_num, court_side, serve_dir, mu_x, mu_y, tau_x, tau_y, rho, t, ...) {
      Sigma <- matrix(c(tau_x^2, rho*tau_x*tau_y, rho*tau_x*tau_y, tau_y^2), nrow = 2)
      as.data.frame(ellipse(Sigma, centre = c(mu_x, mu_y), level = 0.95, npoints = 200)) %>%
        mutate(serve_num = serve_num, court_side = court_side, serve_dir = serve_dir, t = t)
    })

  has_body    <- "Body" %in% serve_dirs
  spot_levels <- if (has_body) {
    c("deuce wide", "deuce body", "deuce tee", "ad tee", "ad body", "ad wide")
  } else {
    c("deuce wide", "deuce tee", "ad tee", "ad wide")
  }

  add_spot <- function(df, court_col = "court_side", dir_col = "serve_dir") {
    df %>%
      mutate(
        serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
        court_side = ifelse(.data[[court_col]] == "DeuceCourt", "Deuce\nCourt", "Ad\nCourt"),
        region = case_when(
          court_side == "Deuce\nCourt" & .data[[dir_col]] == "Wide" ~ "deuce wide",
          court_side == "Deuce\nCourt" & .data[[dir_col]] == "Body" ~ "deuce body",
          court_side == "Deuce\nCourt" & .data[[dir_col]] == "T"    ~ "deuce tee",
          court_side == "Ad\nCourt"    & .data[[dir_col]] == "T"    ~ "ad tee",
          court_side == "Ad\nCourt"    & .data[[dir_col]] == "Body" ~ "ad body",
          court_side == "Ad\nCourt"    & .data[[dir_col]] == "Wide" ~ "ad wide"
        ),
        region       = factor(region, levels = spot_levels),
        court_side = factor(court_side, levels = c("Deuce\nCourt", "Ad\nCourt"))
      )
  }

  pbp_df     <- add_spot(pbp_df)
  ellipse_df <- add_spot(ellipse_df)

  ellipse_obs_df  <- ellipse_df %>% filter(x > t)
  ellipse_cens_df <- ellipse_df %>% filter(x <= t)

  ggplot() +
    geom_halfcourt() +
    geom_polygon(
      data  = ellipse_obs_df,
      aes(x = x, y = y, fill = region),
      alpha = 0.3
    ) +
    geom_polygon_pattern(
      data            = ellipse_cens_df,
      aes(x = x, y = y, fill = region),
      pattern         = "stripe",
      pattern_angle   = 45,
      pattern_density = 0.4,
      pattern_spacing = 0.02,
      pattern_fill    = "white",
      alpha           = 0.5,
      pattern_colour  = NA
    ) +
    geom_point(
      data  = pbp_df,
      aes(x = x_serve_bounce, y = y_serve_bounce, color = region),
      alpha = 0.75,
      size  = 0.75
    ) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_fill_colorblind() +
    scale_color_colorblind() +
    guides(fill = "none") +
    labs(x = "", y = "", title = server_name) +
    coord_equal() +
    theme_minimal() +
    theme(
      panel.grid          = element_blank(),
      axis.text           = element_blank(),
      legend.position     = "bottom",
      strip.text.y.left   = element_text(angle = 0),
      legend.title        = element_text(size = 6, face = "bold"),
      legend.text         = element_text(size = 6),
      legend.background   = element_rect(fill = "gray95", color = NA),
      plot.title          = element_text(hjust = 0.5)
    )

}

server_name <- "N.DJOKOVIC"
illustrate_execution_error_body(server_name)
