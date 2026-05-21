
library(tidyverse)
library(gamm4)
library(pROC)

players <- c("A.BARTY", "S.WILLIAMS", "A.SABALENKA", "N.OSAKA", "S.KENIN",
             "I.SWIATEK", "C.GAUFF", "E.SVITOLINA", 
             "N.DJOKOVIC", "R.NADAL", "C.ALCARAZ", "J.SINNER", "D.MEDVEDEV",
             "A.ZVEREV", "J.ISNER", "R.FEDERER", "A.RUBLEV")

pbp_df = read_csv("tennis_project/data/pbp_df.csv") %>%  
  filter(
    server_name %in% players,
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

set.seed(42)
train_ind = sample(1:nrow(pbp_df), round(nrow(pbp_df) * 0.8))
train_df = pbp_df[train_ind,]
test_df = pbp_df[-train_ind,]

# returner? weights?
player_gamm = gamm4(point ~ s(x_serve_bounce,
                              y_serve_bounce,
                              serve_speed_kph,
                              by = hand_position) + 
                      hand_position,
                    random = ~ (1 | server_name) + (1 | returner_name),
                    family = binomial,
                    data = train_df
)

# Fixed effects (linear predictor on link scale)
eta_fixed <- predict(
  player_gamm$gam,
  newdata = test_df,
  type = "link"
)

# Random effects: server
re_server_df <- ranef(player_gamm$mer)$server_name |>
  tibble::rownames_to_column("server_name") |>
  rename(re_server = `(Intercept)`)

# Random effects: returner
re_returner_df <- ranef(player_gamm$mer)$returner_name |>
  tibble::rownames_to_column("returner_name") |>
  rename(re_returner = `(Intercept)`)

# Match random effects to test data (as vectors)
re_server <- re_server_df$re_server[
  match(test_df$server_name, re_server_df$server_name)
]

re_returner <- re_returner_df$re_returner[
  match(test_df$returner_name, re_returner_df$returner_name)
]

# Replace missing REs with 0 (new/unseen players)
re_server[is.na(re_server)] <- 0
re_returner[is.na(re_returner)] <- 0

# Full linear predictor
eta <- eta_fixed + re_server + re_returner

# Predicted probabilities
v_hat <- plogis(eta)

# Log-likelihood (two equivalent ways)
print(sum(dbinom(x = test_df$point, size = 1, prob = v_hat, log = TRUE)))

print(sum(
  test_df$point * log(v_hat) +
    (1 - test_df$point) * log(1 - v_hat)
))

# ROC + AUC
my.roc <- roc(test_df$point, v_hat)
print(auc(my.roc))

# only server intercept
# loglik is -1122.168
# auc is 0.6403

# server AND returner intercept
# loglik is -1115.215
# auc is 0.6503


