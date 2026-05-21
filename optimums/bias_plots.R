# Bias plots
library(tidyverse)
library(glue)
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



# Vector of player names
players <- c("N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV",
             "A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA")

# Initialize empty list to store data frames
mu_means_list <- list()

for (player in players) {
  # Read in Stan fit for this player
  fit_path <- paste0("tennis_project/execution_error/players/", player, ".rds")
  exec_err_fit <- readRDS(fit_path)
  
  # Extract posterior means for `mu`
  mu_df <- exec_err_fit$summary(variables = "mu") %>%
    select(variable, mean) %>%
    tidyr::extract(
      variable,
      into = c("serve_num", "court_side", "serve_dir", "coord"),
      regex = "mu\\[(\\d+),(\\d+),(\\d+),(\\d+)\\]",
      convert = TRUE
    ) %>%
    mutate(
      coord = if_else(coord == 1, "x", "y"),
      player = player
    ) %>%
    pivot_wider(
      names_from = coord,
      values_from = mean
    ) |> 
    filter(serve_dir != 3)
  
  # Append to list
  mu_means_list[[player]] <- mu_df
}

# Combine all players' data frames
mu_means_all <- bind_rows(mu_means_list) |> 
  mutate(court_side = ifelse(court_side == 1, "AdCourt", "DeuceCourt"),
         serve_dir = ifelse(serve_dir == 1, "T", "Wide"))






optimums_list <- list()

for (player in players) {
  # Build file path (now just based on player)
  file_path <- paste0("tennis_project/optimums/players/", player, ".rds")
  
  if (file.exists(file_path)) {
    # Read RDS file (assumed to contain data for all sides and serve numbers)
    opt_df <- readRDS(file_path) %>%
      mutate(player = player,
             serve_dir = ifelse(abs(y) > 2, "Wide", "T")) |> 
      group_by(serve_num, court_side, serve_dir) |> 
      slice_max(ev_hat) |> 
      ungroup()
    
    # Store in list
    optimums_list[[length(optimums_list) + 1]] <- opt_df
  } else {
    warning(glue::glue("File not found: {file_path}"))
  }
}

# Combine all into one tibble
optimums_all <- bind_rows(optimums_list)








bias_df <- mu_means_all %>%
  left_join(
    optimums_all %>% 
      rename(
        x_opt = x,
        y_opt = y
      ),
    by = c("player", "serve_num", "court_side", "serve_dir")
  ) %>%
  mutate(
    diff_x = x - x_opt,
    diff_y = y - y_opt
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
  )
  # mutate(diff_x = diff_x * 39.3701,
  #        diff_y = diff_y * 39.3701)


distance_df = bias_df %>% 
  mutate(dist_from_opt = sqrt(diff_x^2 + diff_y^2))

match_df = read_csv("tennis_project/data/catalogue_all_matches_available.csv")

player_info = read_csv("tennis_project/data/player_ids.csv")

match_df = match_df %>% 
  filter(player1 %in% players,
         player2 %in% players)

match_ids = match_df %>% pull(match_id)

winners = c()

for (match_id in match_ids) {
  current_match = read_csv(paste0("tennis_project/data/play_by_play/", match_id, "_pbp.csv"))
  winner = current_match %>% slice_tail() %>% mutate(point_winner_id = as.character(point_winner_id)) %>% left_join(player_info, by = join_by(point_winner_id == id)) %>% pull(name)
  winners = append(winners, winner)
}

match_df$winner = winners

# Step 1: create a wide player-level table
dist_wide <- distance_df %>%
  mutate(
    serve_num = gsub("\\s+", "_", serve_num),
    court_side = gsub("\\s+", "_", court_side),
    serve_dir = gsub("\\s+", "_", serve_dir),
    combo = paste(serve_num, court_side, serve_dir, sep = "_")
  ) %>%
  select(player, combo, dist_from_opt) %>%
  pivot_wider(
    names_from = combo,
    values_from = dist_from_opt
  )

# Step 2: join for player1
match_df2 <- match_df %>%
  left_join(dist_wide, by = c("player1" = "player")) %>%
  rename_with(~ paste0("p1_", .), -c(names(match_df)))

# Step 3: join for player2
match_df2 <- match_df2 %>%
  left_join(dist_wide, by = c("player2" = "player")) %>%
  rename_with(~ paste0("p2_", .), 
              starts_with("1st") | starts_with("2nd"))




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
  xintercept = seq(-70, 70, by = 10),
  yintercept = seq(-70, 70, by = 10)
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



## Deuce Court -----

ggplot() +
  geom_rect(
    data = quad_rects %>% filter(substr(spot, 1, 1) == "D"),
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
    data = bias_df %>% filter(court_side == "DeuceCourt",
                              player %in% female),
    aes(x = diff_x, y = diff_y, shape = factor(player)),
    size = 2, alpha = 0.7
  ) +
  facet_grid(spot ~ serve_num) +
  scale_shape_manual(values = manual_shapes, name = "Player") +
  scale_fill_manual(
    values = quad_colors,
    name = "Observed error\nrelative to optimum"
  ) +
  coord_fixed() +
  labs(
    title = "Strategic Bias (Subconscious) - Deuce Court",
    x = expression(hat(mu)[x] - hat(mu)[x]^"OPT"),
    y = expression(hat(mu)[y] - hat(mu)[y]^"OPT")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text.y.right = element_text(angle = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )


## Ad Court -----

ggplot() +
  geom_rect(
    data = quad_rects %>% filter(substr(spot, 1, 1) == "A"),
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill_label),
    alpha = 0.2, color = NA
  ) +
  geom_hline(data = gridlines_df, aes(yintercept = yintercept),
             color = "gray75", linewidth = 0.3) +
  geom_vline(data = gridlines_df, aes(xintercept = xintercept),
             color = "gray75", linewidth = 0.3) +
  facet_grid(spot ~ serve_num) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray55") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray55") +
  geom_point(
    data = bias_df %>% filter(court_side == "AdCourt",
                              player %in% female),
    aes(x = diff_x, y = diff_y, shape = factor(player)),
    size = 2, alpha = 0.7
  ) +
  scale_shape_manual(values = manual_shapes, name = "Player") +
  scale_fill_manual(
    values = quad_colors,
    name = "Observed error\nrelative to optimum"
  ) +
  coord_fixed() +
  labs(
    title = "Strategic Bias (Subconscious) - Ad Court",
    x = expression(hat(mu)[x] - hat(mu)[x]^"OPT"),
    y = expression(hat(mu)[y] - hat(mu)[y]^"OPT")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.text.y.right = element_text(angle = 0),
    axis.title.y = element_text(angle = 0, vjust = 0.5)
  )
