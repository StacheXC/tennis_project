
library(tidyverse)
library(ggthemes)

geom_halfcourt <- function() {
  court_dat <- data.frame(
    x    = c(0, 0, 11.887, 0, 0, 0, 0, 6.4),
    xend = c(11.887, 0, 11.887, 11.887, 11.887, 11.887, 6.4, 6.4),
    y    = c(5.486, 5.486, 5.486, -5.486, 4.115, -4.115, 0, 4.115),
    yend = c(5.486, -5.486, -5.486, -5.486, 4.115, -4.115, 0, -4.115)
  )
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend),
               data = court_dat, color = "gray40")
}

plot_theme <- list(
  scale_color_colorblind(name = "Direction"),
  coord_equal(),
  labs(x = "", y = ""),
  theme_minimal(),
  theme(
    panel.grid        = element_blank(),
    axis.text         = element_blank(),
    legend.position   = "bottom",
    strip.text.y.left = element_text(angle = 0),
    plot.title        = element_text(hjust = 0.5)
  )
)


# Block 1: Observed targets (execution error posterior means) -----------------

exec_err_post_mean <- readRDS("tennis_project/execution_error/execution_error.rds") %>%
  group_by(server_name, serve_num, court_side, serve_dir) %>%
  summarise(across(c(mu_x, mu_y), mean), .groups = "drop") %>%
  mutate(
    serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court"))
  )

ggplot(exec_err_post_mean, aes(x = mu_x, y = mu_y, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  plot_theme


# Block 2: Optimal targets — old method (slice_max, abs(y) > 2 boundary) ------

ev_df <- readRDS("tennis_project/optimums/optimums.rds")

optimal_targets <- ev_df %>%
  mutate(
    serve_dir  = ifelse(abs(y_serve_bounce) > 2, "Wide", "T"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
    serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
  ) %>%
  group_by(server_name, serve_num, court_side, serve_dir) %>%
  slice_max(ev_hat)

ggplot(optimal_targets, aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  plot_theme


# Block 3: Optimal targets — new method (local maxima, diagonal boundary) ------

serve_x   <- -11.887
slope_mid <- 3.4 / (13.37 - serve_x)

find_local_maxima <- function(ev_df, grid_res = 0.1) {
  offsets <- list(
    c(-1, -1), c(-1,  0), c(-1,  1),
    c( 0, -1),             c( 0,  1),
    c( 1, -1), c( 1,  0), c( 1,  1)
  )
  ev_df %>%
    group_by(server_name, serve_num, court_side) %>%
    group_modify(function(data, keys) {
      lookup <- setNames(
        data$ev_hat,
        paste(round(data$x_serve_bounce, 1), round(data$y_serve_bounce, 1))
      )
      is_max <- rep(TRUE, nrow(data))
      for (off in offsets) {
        nbr_key <- paste(round(data$x_serve_bounce + off[1] * grid_res, 1),
                         round(data$y_serve_bounce + off[2] * grid_res, 1))
        nbr_ev  <- lookup[nbr_key]
        nbr_ev[is.na(nbr_ev)] <- -Inf
        is_max  <- is_max & (data$ev_hat > nbr_ev)
      }
      data[is_max, ] %>% select(x_serve_bounce, y_serve_bounce, ev_hat)
    }) %>%
    ungroup()
}

optimal_local <- find_local_maxima(ev_df %>%
  filter(
    x_serve_bounce <= 6.4,
    (court_side == "DeuceCourt" & y_serve_bounce >= 0    & y_serve_bounce <=  4.115) |
    (court_side == "AdCourt"    & y_serve_bounce >= -4.115 & y_serve_bounce <= 0)
  )) %>%
  mutate(
    serve_dir  = case_when(
      court_side == "DeuceCourt" & y_serve_bounce >   slope_mid * (x_serve_bounce - serve_x) ~ "Wide",
      court_side == "DeuceCourt" & y_serve_bounce <=  slope_mid * (x_serve_bounce - serve_x) ~ "T",
      court_side == "AdCourt"    & y_serve_bounce <  -slope_mid * (x_serve_bounce - serve_x) ~ "Wide",
      court_side == "AdCourt"    & y_serve_bounce >= -slope_mid * (x_serve_bounce - serve_x) ~ "T"
    ),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
    serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
  ) %>%
  group_by(server_name, serve_num, court_side, serve_dir) %>%
  slice_max(ev_hat, n = 1) %>%
  ungroup()

ggplot(optimal_local, aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  plot_theme
