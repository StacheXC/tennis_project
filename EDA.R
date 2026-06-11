
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

exec_err_fit = readRDS("tennis_project/execution_error/execution_error.rds")

exec_err_post_mean <- exec_err_fit %>%
  group_by(server_name, serve_num, court_side, serve_dir) %>%
  summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, t), mean), .groups = "drop")

exec_err_post_mean = exec_err_post_mean %>% 
  mutate(
    serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court"))
  )

ggplot(exec_err_post_mean, aes(x = mu_x, y = mu_y, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_color_colorblind(name = "Direction") +
  coord_equal() +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid        = element_blank(),
    axis.text         = element_blank(),
    legend.position   = "bottom",
    strip.text.y.left = element_text(angle = 0),
    plot.title        = element_text(hjust = 0.5)
  )

ev_df = readRDS("tennis_project/optimums/optimums.rds")

optimal_targets = ev_df %>% 
  mutate(
    serve_dir = ifelse(abs(y_serve_bounce) > 2, "Wide", "T"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
    court_side = factor(court_side, levels = c("Deuce Court", "Ad Court")),
    serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
  ) %>% 
  group_by(server_name, serve_num, court_side, serve_dir) |>
  slice_max(ev_hat)

ggplot(optimal_targets, aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_color_colorblind(name = "Direction") +
  coord_equal() +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid        = element_blank(),
    axis.text         = element_blank(),
    legend.position   = "bottom",
    strip.text.y.left = element_text(angle = 0),
    plot.title        = element_text(hjust = 0.5)
  )

ggplot(optimal_targets %>% 
         filter(abs(y_serve_bounce) != 2), aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
  geom_halfcourt() +
  geom_point(alpha = 0.7, size = 1.5) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_color_colorblind(name = "Direction") +
  coord_equal() +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    panel.grid        = element_blank(),
    axis.text         = element_blank(),
    legend.position   = "bottom",
    strip.text.y.left = element_text(angle = 0),
    plot.title        = element_text(hjust = 0.5)
  )

players <- optimal_targets %>% 
  filter(abs(y_serve_bounce) == 2) %>% 
  pull(server_name) %>% 
  unique()





