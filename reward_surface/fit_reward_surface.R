
library(tidyverse)
# library(fields)
library(gamm4)

players <- c("A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA", 
             "N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV")

pbp_df = read_csv("tennis_project/data/pbp_df.csv") %>%  
  filter(
    # server_name %in% players,
    str_detect(match_id, "australian"),
    x_serve_bounce > 3,
    x_serve_bounce <= 6.4,
    abs(y_serve_bounce) <= 4.11,
    y_serve_bounce >= 0 | court_side == "AdCourt",
    y_serve_bounce <= 0 | court_side == "DeuceCourt",
    !is_fault,
    rally_length > 0,
    serve_speed_kph > 0
  ) %>% 
  mutate(
    hand_position = case_when(
      court_side == "DeuceCourt" & server_handedness == "right-handed" ~ "Wide",
      court_side == "AdCourt" & server_handedness == "left-handed" ~ "Wide",
      TRUE ~ "T"
    ),
    hand_position = factor(hand_position),
    y_serve_bounce = if_else(
      court_side == "AdCourt", -y_serve_bounce, y_serve_bounce
    ),
    point = point_winner_id == server_id,
    weights = 1 / ((rally_length + 1) %/% 2)
  )



player_gamm = gamm4(point ~ s(x_serve_bounce,
                              y_serve_bounce,
                              serve_speed_kph,
                              by = hand_position) + 
                      hand_position,
                    random = ~ (1 | server_name) + (1 | returner),
                    family = binomial,
                    data = pbp_df
)

saveRDS(player_gamm, "tennis_project/reward_surface/player_gamm_all.rds")



speed_df = pbp_df %>%  
  group_by(server_name, serve_num) %>% 
  summarize(serve_speed_kph = mean(serve_speed_kph)) %>% 
  ungroup()

player_df <- read_csv(
  "tennis_project/data/misc/player_ids.csv"
) %>% 
  select(name, player_handedness) %>% 
  distinct() %>% 
  filter(!is.na(player_handedness)) %>% 
  rename(server_name = name, server_handedness = player_handedness)

# Make predictions
value_du = expand.grid(
  x_serve_bounce = seq(0, 6.4, by = 0.1),
  y_serve_bounce = seq(0, 4.1, by = 0.1),
  serve_num = c(1, 2),
  server_name = players
) %>% 
  left_join(player_df) %>% 
  left_join(speed_df) %>%  
  mutate(
    court_side = "DeuceCourt",
    hand_position = ifelse(
      server_handedness == "right-handed", "Wide", "T"
    )
  )

value_du <- value_du %>%
  mutate(
    eta_fixed = predict(
      player_gamm$gam,
      newdata = .,
      type = "link"
    )
  )

re_name <- ranef(player_gamm$mer)$server_name |>
  tibble::rownames_to_column("server_name") |>
  rename(re_name = `(Intercept)`)

value_du <- value_du |>
  left_join(re_name, by = "server_name") |>
  mutate(
    re_name = ifelse(is.na(re_name), 0, re_name),  # new players → population mean
    eta = eta_fixed + re_name,
    v_hat = plogis(eta) * 2 - 1
  )

value_ad = expand.grid(
  x_serve_bounce = seq(0, 6.4, by = 0.1),
  y_serve_bounce = seq(0, 4.1, by = 0.1),
  serve_num = c(1, 2),
  server_name = players
) %>% 
  left_join(player_df) %>% 
  left_join(speed_df) %>% 
  mutate(
    court_side = "AdCourt",
    hand_position = ifelse(
      server_handedness == "left-handed", "Wide", "T"
    )
  )

value_ad <- value_ad %>%
  mutate(
    eta_fixed = predict(
      player_gamm$gam,
      newdata = .,
      type = "link"
    )
  )

value_ad <- value_ad |>
  left_join(re_name, by = "server_name") |>
  mutate(
    re_name = ifelse(is.na(re_name), 0, re_name),  # new players → population mean
    eta = eta_fixed + re_name,
    v_hat = plogis(eta) * 2 - 1
  ) |> 
  mutate(y_serve_bounce = -y_serve_bounce)

value_all = bind_rows(
  value_du, value_ad
)






















