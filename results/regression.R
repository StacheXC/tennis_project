
library(tidyverse)
library(mvtnorm)
library(ggthemes)
library(glmnet)

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

# Posterior means per server/serve_num/court_side/serve_dir
exec_err <- readRDS("tennis_project/execution_error/body/execution_error_body.rds")$draws |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  summarise(across(c(mu_x, mu_y, tau_x, tau_y, rho, theta), mean), .groups = "drop")

posterior_means <- exec_err |>
  select(server_name, serve_num, court_side, serve_dir, mu_x, mu_y)

ggplot(posterior_means %>%
         mutate(
           serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
           court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
           court_side = factor(court_side, levels = c("Deuce Court", "Ad Court"))
         ),
       aes(x = mu_x, y = mu_y, color = serve_dir)) +
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
    strip.text.y.left = element_text(angle = 0)
  )

params_wide <- exec_err |>
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



# Regression ---------------------------------------------------------------

# Optimal aim points per server/serve_num/court_side/serve_dir
optimums_all <- readRDS("tennis_project/optimums/gender/targets_gender.rds") |>
  select(server_name, serve_num, court_side, serve_dir,
         x_opt = x_serve_bounce, y_opt = y_serve_bounce)

ggplot(optimums_all %>%
         mutate(
           serve_num  = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
           court_side = ifelse(court_side == "DeuceCourt", "Deuce Court", "Ad Court"),
           court_side = factor(court_side, levels = c("Deuce Court", "Ad Court"))
         ),
       aes(x = x_opt, y = y_opt, color = serve_dir)) +
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
    strip.text.y.left = element_text(angle = 0)
  )

# Euclidean distance between posterior mean aim and optimal aim
distance_df <- posterior_means |>
  left_join(optimums_all, by = c("server_name", "serve_num", "court_side", "serve_dir")) |>
  mutate(dist_from_opt = sqrt((mu_x - x_opt)^2 + (mu_y - y_opt)^2)) |>
  select(server_name, serve_num, court_side, serve_dir, dist_from_opt)

fault_value = pbp_df %>%
  filter(serve_num == 2) %>%
  group_by(server_name) %>%
  summarize(fault_value = mean(point_winner_id == server_id))

# Build binomial regression data
reg_df <- pbp_df |>
  left_join(fault_value) %>%
  # filter(point_end_type != "Faulty Serve") %>% 
  mutate(
    point = case_when(
      point_end_type == "Faulty Serve" ~ fault_value,
      point_winner_id == server_id ~ 1,
      point_winner_id != server_id ~ 0
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
  family = quasibinomial,
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

hypotheses <- multcomp::glht(fit, linfct = L, alternative = "less",
                   coef. = function(x) coef(x)[keep],
                   vcov. = function(x) vcov(x)[keep, keep])
summary(hypotheses)


# Lasso -------------------------------------------------------------------

reg_df_complete <- reg_df |> filter(!is.na(dist_from_opt))

X <- model.matrix(
  ~ server_name + returner_name + region_hat + serve_num + dist_from_opt,
  data = reg_df_complete
)

y <- cbind(reg_df_complete$wins, reg_df_complete$n - reg_df_complete$wins)

# Penalize player effects only; leave dist_from_opt terms unpenalized
penalty <- ifelse(grepl("dist_from_opt", colnames(X)), 0, 1)
penalty[1] <- 0  # intercept

fit_lasso <- cv.glmnet(
  X, y,
  family         = "binomial",
  alpha          = 1,
  penalty.factor = penalty
)

plot(fit_lasso)

coef(fit_lasso, s = "lambda.1se")
