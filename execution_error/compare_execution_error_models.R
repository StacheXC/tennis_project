
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

# New pooled model: one combined file
mu_means_new <- readRDS("tennis_project/execution_error/execution_error.rds") |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  summarise(mu_x = mean(mu_x), mu_y = mean(mu_y), .groups = "drop") |>
  mutate(model = "pooled")

# Old per-player model: one file per player
old_files  <- list.files("tennis_project/execution_error/old/players",
                         full.names = TRUE, pattern = "\\.rds$")

mu_means_old <- map_dfr(old_files, function(f) {
  player <- tools::file_path_sans_ext(basename(f))
  readRDS(f) |>
    group_by(serve_num, court_side, serve_dir) |>
    summarise(mu_x = mean(mu_x), mu_y = mean(mu_y), .groups = "drop") |>
    mutate(server_name = player)
}) |>
  mutate(model = "independent")

format_mu <- function(df, model_label) {
  df |>
    mutate(
      model      = model_label,
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
      court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
      court_side = factor(court_side, levels = c("Deuce", "Ad"))
    )
}

mu_new <- format_mu(mu_means_new, "Pooled")
mu_old <- format_mu(mu_means_old, "Independent")

plot_mu <- function(df) {
  ggplot(df, aes(x = mu_x, y = mu_y, color = serve_dir)) +
    geom_halfcourt() +
    geom_point(alpha = 0.7, size = 1.5) +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_color_colorblind(name = "Direction") +
    coord_equal() +
    labs(x = "", y = "", title = unique(df$model)) +
    theme_minimal() +
    theme(
      panel.grid        = element_blank(),
      axis.text         = element_blank(),
      legend.position   = "bottom",
      strip.text.y.left = element_text(angle = 0),
      plot.title        = element_text(hjust = 0.5)
    )
}

plot_mu(mu_old)
plot_mu(mu_new)








# censoring bounds
exec_err_fit = readRDS("tennis_project/execution_error/execution_error.rds")

exec_err_post_mean <- exec_err_fit %>%
  group_by(server_name, serve_num, court_side, serve_dir) %>%
  summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, t), mean), .groups = "drop")

ggplot() + 
  geom_halfcourt() +
  geom_vline(data = exec_err_post_mean,
             aes(xintercept = t)) +
  facet_grid(serve_num ~ court_side)

hist(exec_err_post_mean$t)




# tau (bad)
tau_means_old <- map_dfr(old_files, function(f) {
  player <- tools::file_path_sans_ext(basename(f))
  readRDS(f) |>
    group_by(serve_num, court_side, serve_dir) |>
    summarise(tau_x = mean(tau_x), tau_y = mean(tau_y), .groups = "drop") |>
    mutate(server_name = player)
}) |>
  mutate(model = "independent")

hist(tau_means_old$tau_x)
hist(tau_means_old$tau_y)


