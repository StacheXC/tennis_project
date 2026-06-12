
library(tidyverse)
library(mvtnorm)
library(ggthemes)

source("tennis_project/utils.R")

# Load pbp data
pbp_df <- readRDS("tennis_project/data/pbp_df.rds") %>%
  filter(
    !is.na(server_name),
    !is.na(returner_name),
    str_detect(match_id, "australian"),
    x_serve_bounce > 3 | error_type == "Net Error",
    x_serve_bounce < 3 | error_type != "Net Error" | is.na(error_type),
    x_serve_bounce < 9,
    abs(y_serve_bounce) < 5.48,
    y_serve_bounce > -1 | court_side == "AdCourt",
    y_serve_bounce < 1 | court_side == "DeuceCourt"
  )

# Load posterior means for all players and pivot wide so each row has
# both Wide and T parameters for a given (server, court_side, serve_num)
params_wide <- readRDS("tennis_project/execution_error/execution_error.rds") |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, theta), mean), .groups = "drop") |>
  pivot_wider(
    names_from  = serve_dir,
    values_from = c(mu_x, mu_y, tau_x, tau_y, rho, theta)
  )

# Assign serve_dir via mixture model: argmax of theta * dmvnorm over directions
pbp_df <- pbp_df |>
  left_join(params_wide, by = c("server_name", "serve_num", "court_side")) |>
  group_by(server_name, court_side, serve_num) |>
  group_modify(function(data, keys) {

    # Drop rows with no matched parameters
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

    data |> mutate(serve_dir_hat = ifelse(dens_Wide > dens_T, "Wide", "T"))
  }) |>
  ungroup() |>
  select(-matches("^(mu_x|mu_y|tau_x|tau_y|rho|theta|t)_"))


plot_serve_directions <- function(server_name) {

  plot_df <- pbp_df |>
    filter(server_name == !!server_name) |>
    mutate(
      x_serve_bounce = if_else(
        !is.na(x_serve_bounce) & x_serve_bounce < 3,
        runif(n(), -.2, 0),
        x_serve_bounce
      ),
      court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
      court_side = factor(court_side, levels = c("Deuce", "Ad")),
      serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve")
    )

  ggplot(plot_df, aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir_hat)) +
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



# Regression ---------------------------------------------------------------

# Posterior means per server/serve_num/court_side/serve_dir
posterior_means <- readRDS("tennis_project/execution_error/execution_error.rds") |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  summarise(mu_x = mean(mu_x), mu_y = mean(mu_y), .groups = "drop")

# Optimal aim points per server/serve_num/court_side/serve_dir
optimums_all <- readRDS("tennis_project/optimums/optimums.rds") |>
  mutate(serve_dir = ifelse(abs(y_serve_bounce) > 2, "Wide", "T")) |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  slice_max(ev_hat, n = 1) |>
  ungroup() |>
  select(server_name, serve_num, court_side, serve_dir,
         x_opt = x_serve_bounce, y_opt = y_serve_bounce)

# ggplot() + 
#   geom_halfcourt() +
#   geom_point(data = posterior_means,
#              aes(mu_x, mu_y, color = serve_dir)) +
#   facet_grid(court_side ~ serve_num)
# 
# ggplot() + 
#   geom_halfcourt() +
#   geom_point(data = optimums_all,
#              aes(x_opt, y_opt, color = serve_dir)) +
#   facet_grid(court_side ~ serve_num)

# Euclidean distance between posterior mean aim and optimal aim
distance_df <- posterior_means |>
  left_join(optimums_all, by = c("server_name", "serve_num", "court_side", "serve_dir")) |>
  mutate(dist_from_opt = sqrt((mu_x - x_opt)^2 + (mu_y - y_opt)^2)) |>
  select(server_name, serve_num, court_side, serve_dir, dist_from_opt)

# Build binomial regression data
reg_df <- pbp_df |>
  mutate(
    point = case_when(
      point_winner_id == server_id ~ TRUE,
      point_winner_id != server_id ~ FALSE,
      is.na(point_winner_id) ~ FALSE
    )
  ) |>
  group_by(server_name, returner_name, serve_num, court_side, serve_dir_hat) |>
  summarise(
    n    = n(),
    wins = sum(point, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(region_hat = paste(court_side, serve_dir_hat, sep = "_")) |>
  left_join(distance_df,
            by = c("server_name", "serve_num", "court_side",
                   "serve_dir_hat" = "serve_dir"))

# Fit logistic regression
fit <- glm(
  cbind(wins, n - wins) ~ server_name + returner_name + region_hat + serve_num + dist_from_opt +
    region_hat:dist_from_opt + serve_num:dist_from_opt,
  family = binomial,
  data   = reg_df
)

summary(fit)$coef %>% View()


# Test total effect of dist_from_opt < 0 for each region x serve_num combination

# Strip aliased (NA) coefficients — these are always player dummies, never
# dist_from_opt terms, so the corresponding L columns are zero and safe to drop
keep       <- !is.na(coef(fit))
coef_names <- names(coef(fit))[keep]
p          <- length(coef_names)

L <- matrix(0, nrow = 8, ncol = p,
            dimnames = list(
              c("AdCourt_T_s1", "AdCourt_Wide_s1", "DeuceCourt_T_s1", "DeuceCourt_Wide_s1",
                "AdCourt_T_s2", "AdCourt_Wide_s2", "DeuceCourt_T_s2", "DeuceCourt_Wide_s2"),
              coef_names
            ))

L[, "dist_from_opt"] <- 1

L["AdCourt_Wide_s1",    "region_hatAdCourt_Wide:dist_from_opt"]    <- 1
L["DeuceCourt_T_s1",    "region_hatDeuceCourt_T:dist_from_opt"]    <- 1
L["DeuceCourt_Wide_s1", "region_hatDeuceCourt_Wide:dist_from_opt"] <- 1
L["AdCourt_Wide_s2",    "region_hatAdCourt_Wide:dist_from_opt"]    <- 1
L["DeuceCourt_T_s2",    "region_hatDeuceCourt_T:dist_from_opt"]    <- 1
L["DeuceCourt_Wide_s2", "region_hatDeuceCourt_Wide:dist_from_opt"] <- 1

L[1:4, "serve_num:dist_from_opt"] <- 1
L[5:8, "serve_num:dist_from_opt"] <- 2

hypotheses <- multcomp::glht(fit, linfct = L, alternative = "two.sided",
                   coef. = function(x) coef(x)[keep],
                   vcov. = function(x) vcov(x)[keep, keep])
summary(hypotheses)
