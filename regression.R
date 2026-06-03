
library(tidyverse)
library(mvtnorm)
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

players <- c("N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "R.FEDERER", "A.RUBLEV",
             "A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA")

# Load pbp data
pbp_df <- readRDS("tennis_project/data/pbp_df.rds") %>%
  filter(
    server_name %in% players,
    str_detect(match_id, "australian"),
    x_serve_bounce > 3 | error_type == "Net Error",
    x_serve_bounce < 9,
    abs(y_serve_bounce) < 5.48,
    y_serve_bounce > -1 | court_side == "AdCourt",
    y_serve_bounce < 1 | court_side == "DeuceCourt"
  )

# Load posterior means for all players and pivot wide so each row has
# both Wide and T parameters for a given (server, court_side, serve_num)
params_wide <- map_dfr(players, function(player) {
  readRDS(paste0("tennis_project/execution_error/players_theta/", player, ".rds")) |>
    group_by(serve_num, court_side, serve_dir) |>
    summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, theta), mean), .groups = "drop") |>
    mutate(server_name = player)
}) |>
  pivot_wider(
    names_from  = serve_dir,
    values_from = c(mu_x, mu_y, tau_x, tau_y, rho, theta)
  )

# Assign serve_dir via mixture model: argmax of theta * dmvnorm over directions
pbp_df <- pbp_df |>
  left_join(params_wide, by = c("server_name", "serve_num", "court_side")) |>
  group_by(server_name, court_side, serve_num) |>
  group_modify(function(data, keys) {

    # Drop rows with no matched parameters (players outside the 17)
    if (any(is.na(data$mu_x_Wide))) return(data |> mutate(serve_dir = NA_character_))

    z <- as.matrix(data[, c("x_serve_bounce", "y_serve_bounce")])

    p <- data[1, ]  # parameters are constant within group

    Sigma_Wide <- matrix(c(p$tau_x_Wide^2,
                            p$rho_Wide * p$tau_x_Wide * p$tau_y_Wide,
                            p$rho_Wide * p$tau_x_Wide * p$tau_y_Wide,
                            p$tau_y_Wide^2), nrow = 2)

    Sigma_T <- matrix(c(p$tau_x_T^2,
                         p$rho_T * p$tau_x_T * p$tau_y_T,
                         p$rho_T * p$tau_x_T * p$tau_y_T,
                         p$tau_y_T^2), nrow = 2)

    dens_Wide <- p$theta_Wide * dmvnorm(z, mean = c(p$mu_x_Wide, p$mu_y_Wide), sigma = Sigma_Wide)
    dens_T    <- p$theta_T    * dmvnorm(z, mean = c(p$mu_x_T,    p$mu_y_T),    sigma = Sigma_T)

    data |> mutate(serve_dir = ifelse(dens_Wide > dens_T, "Wide", "T"))
  }) |>
  ungroup() |>
  select(-matches("^(mu_x|mu_y|tau_x|tau_y|rho|theta)_"))


plot_serve_directions <- function(server_name) {

  plot_df <- pbp_df |>
    filter(server_name == !!server_name) |>
    mutate(
      x_serve_bounce = ifelse(
        !is.na(x_serve_bounce) & x_serve_bounce < 3,
        runif(sum(!is.na(x_serve_bounce) & x_serve_bounce < 3), -.2, 0),
        x_serve_bounce
      ),
      court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
      court_side = factor(court_side, levels = c("Deuce", "Ad")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  ggplot(plot_df, aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
    geom_point(alpha = 0.6, size = 0.75) +
    geom_halfcourt() +
    facet_grid(court_side ~ serve_num, switch = "y") +
    scale_color_colorblind(name = "Direction") +
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

plot_serve_directions("N.DJOKOVIC")







