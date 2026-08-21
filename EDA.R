
library(tidyverse)

exec_err_fit = readRDS("tennis_project/execution_error/body/execution_error_body.rds")$draws

mu_df = exec_err_fit %>% 
  group_by(server_name, serve_num, court_side, serve_dir) %>% 
  summarize(mu_x = mean(mu_x), mu_y = mean(mu_y)) %>% 
  mutate(court_side = factor(court_side, levels = c("DeuceCourt", "AdCourt")))

tau_df = exec_err_fit %>% 
  group_by(server_name, serve_num, court_side, serve_dir) %>% 
  summarize(tau_x = mean(tau_x), tau_y = mean(tau_y)) %>% 
  mutate(court_side = factor(court_side, levels = c("DeuceCourt", "AdCourt")))

ggplot() +
  geom_halfcourt() + 
  geom_point(data = mu_df,
             aes(x = mu_x, y = mu_y)) + 
  facet_grid(court_side ~ serve_num)

ggplot() +
  geom_histogram(data = tau_df,
                 aes(x = tau_x)) +
  facet_grid(court_side ~ serve_num)

ggplot() +
  geom_histogram(data = tau_df,
                 aes(x = tau_y)) +
  facet_grid(court_side ~ serve_num)

targets = readRDS("tennis_project/optimums/targets.rds")

ggplot() +
  geom_halfcourt() +
  geom_point(data = targets,
             aes(x = x_serve_bounce, y = y_serve_bounce)) +
  facet_grid(court_side ~ serve_num)



pbp_df <- readRDS("tennis_project/data/pbp_df.rds") %>%
  filter(
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

ggplot() +
  geom_halfcourt() +
  geom_point(data = pbp_df,
             aes(x = x_serve_bounce, y = y_serve_bounce, color = serve_dir)) +
  facet_grid(court_side ~ serve_num)

pbp_df %>% 
  filter(x_serve_bounce > 3) %>% 
  group_by(server_name, serve_num, court_side, serve_dir) %>% 
  summarize(xbar = mean(x_serve_bounce), ybar = mean(y_serve_bounce)) -> plot_df

ggplot() + 
  geom_halfcourt() +
  geom_point(data = plot_df,
             aes(x = xbar, y = ybar, color = serve_dir)) +
  facet_grid(court_side ~ serve_num)



















