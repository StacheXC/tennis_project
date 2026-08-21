
# Bias plots
library(tidyverse)
library(ggthemes)

source("tennis_project/utils.R")

# Load execution error model once and summarise posterior means for all players
mu_means_all <- readRDS("tennis_project/execution_error/execution_error.rds") |>
  group_by(server_name, serve_num, court_side, serve_dir) |>
  summarise(mu_x = mean(mu_x), mu_y = mean(mu_y), .groups = "drop")

# Load optimal targets
optimums_all <- readRDS("tennis_project/optimums/targets.rds")

bias_df <- mu_means_all %>%
  left_join(
    optimums_all %>%
      rename(
        x_opt = x_serve_bounce,
        y_opt = y_serve_bounce
      ),
    by = c("server_name", "serve_num", "court_side", "serve_dir")
  ) %>%
  mutate(
    diff_x = mu_x - x_opt,
    diff_y = mu_y - y_opt
  ) |> 
  mutate(
    spot = case_when(
      court_side == "AdCourt" & serve_dir == "T" ~ "Ad\nTee",
      court_side == "AdCourt" & serve_dir == "Wide" ~ "Ad\nWide",
      court_side == "DeuceCourt" & serve_dir == "T" ~ "Deuce\nTee",
      court_side == "DeuceCourt" & serve_dir == "Wide" ~ "Deuce\nWide"
    ),
    spot = factor(spot, levels = c("Deuce\nWide", "Deuce\nTee", "Ad\nTee", "Ad\nWide")),
    serve_num = ifelse(serve_num == 1, "1st\nServe", "2nd\nServe")
  ) %>% 
  mutate(diff_x = diff_x,
         diff_y = diff_y)

# For plot
manual_shapes <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)

quad_colors <- ggthemes::colorblind_pal()(4)

# Define base quadrant geometry
quad_rects_base <- tibble(
  xmin = c(0, 0, -Inf, -Inf),
  xmax = c(Inf, Inf, 0, 0),
  ymin = c(0, -Inf, -Inf, 0),
  ymax = c(Inf, 0, 0, Inf),
  quad_id = 1:4
)

# Spot-specific label mapping (full swap of all quadrants if needed)
normal_labels <- c(
  "Deep & Wide",    # Q1
  "Deep & Narrow",  # Q2
  "Shallow & Narrow", # Q3
  "Shallow & Wide"    # Q4
)

flipped_labels <- c(
  "Deep & Narrow",  # Q1 becomes Q2
  "Deep & Wide",    # Q2 becomes Q1
  "Shallow & Wide", # Q3 becomes Q4
  "Shallow & Narrow" # Q4 becomes Q3
)

flipped_spots <- c("Deuce\nTee", "Ad\nWide")

# Build full label set per spot
gridlines_df <- expand.grid(
  xintercept = seq(-1.5, 1.5, by = 0.5),
  yintercept = seq(-1.5, 1.5, by = 0.5)
)

spot_levels <- c("Deuce\nWide", "Deuce\nTee", "Ad\nTee", "Ad\nWide")

quad_rects <- map_dfr(
  spot_levels,
  function(s) {
    labels <- if (s %in% flipped_spots) flipped_labels else normal_labels
    quad_rects_base %>%
      mutate(
        spot = s,
        fill_label = labels
      )
  }
) %>%
  mutate(
    spot = factor(spot, levels = spot_levels)
  )

male <- c("N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
          "A.ZVEREV", "J.ISNER", "R.FEDERER")

female <- c("A.BARTY", "S.WILLIAMS", "A.SABALENKA",
            "N.OSAKA", "S.KENIN", "I.SWIATEK", "C.GAUFF", "E.SVITOLINA")

plot_bias <- function() {

  ggplot() +
    geom_rect(
      data = quad_rects,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_label),
      alpha = 0.2, color = NA
    ) +
    geom_hline(data = gridlines_df, aes(yintercept = yintercept),
               color = "gray75", linewidth = 0.3) +
    geom_vline(data = gridlines_df, aes(xintercept = xintercept),
               color = "gray75", linewidth = 0.3) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
    geom_point(
      data = bias_df,
      aes(x = diff_x, y = diff_y),
      size = 1, shape = 4, alpha = 0.7
    ) +
    facet_grid(spot ~ serve_num) +
    scale_shape_manual(values = manual_shapes, name = "Player") +
    scale_fill_manual(
      values = quad_colors,
      name = "Observed error\nrelative to optimum"
    ) +
    coord_fixed() +
    labs(
      title = paste0("Strategic Bias"),
      x = expression(hat(mu)[x] - hat(mu)[x]^"OPT"),
      y = expression(hat(mu)[y] - hat(mu)[y]^"OPT")
    ) +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      strip.text.y.right = element_text(angle = 0),
      axis.title.y = element_text(angle = 0, vjust = 0.5),
      plot.title = element_text(hjust = 0.5)
    )
}

plot_bias()
