
library(tidyverse)
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

df = readRDS("tennis_project/optimums/contours.rds") %>% 
  mutate(
    spot = case_when(
      court_side == "DeuceCourt" & serve_dir == "T"    ~ "deuce tee",
      court_side == "DeuceCourt" & serve_dir == "Wide" ~ "deuce wide",
      court_side == "AdCourt"    & serve_dir == "T"    ~ "ad tee",
      court_side == "AdCourt"    & serve_dir == "Wide" ~ "ad wide"
    ),
    spot = factor(spot, levels = c("deuce wide", "deuce tee", "ad tee", "ad wide")),
    serve_num = ifelse(serve_num == 1, "1st Serve", "2nd Serve"),
    court_side = ifelse(court_side == "DeuceCourt", "Deuce", "Ad"),
    court_side = factor(court_side, levels = c("Deuce", "Ad"))
  )

ggplot() +
  geom_halfcourt() +
  geom_density_2d(
    data = df,
    aes(x = x_serve_bounce, y = y_serve_bounce, color = spot),
    contour_var = "ndensity",
    breaks = c(0.05)
  ) +
  geom_density_2d_filled(
    data = df, 
    aes(x = x_serve_bounce, y = y_serve_bounce, fill = spot),
    contour_var = "ndensity", # normalized density 0-1
    breaks = c(0.05, 1),
    alpha = 0.2) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  labs(x = "", y = "", title = "N.DJOKOVIC") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        strip.text.y.left = element_text(angle = 0),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6),
        legend.background = element_rect(fill = "gray95", color = NA),
        plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_halfcourt() +
  geom_point(data = df,
             aes(x = x_serve_bounce, y = y_serve_bounce)) +
  facet_grid(court_side ~ serve_num, switch = "y") +
  scale_fill_colorblind() +
  scale_color_colorblind() +
  labs(x = "", y = "", title = "N.DJOKOVIC") +
  coord_equal() +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        strip.text.y.left = element_text(angle = 0),
        legend.title = element_text(size = 6, face = "bold"), 
        legend.text = element_text(size = 6),
        legend.background = element_rect(fill = "gray95", color = NA),
        plot.title = element_text(hjust = 0.5))

