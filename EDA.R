
distance_df

avg_distance_df = distance_df %>% 
  group_by(server_name) %>% 
  summarize(dist_from_opt = mean(dist_from_opt)) %>% 
  ungroup()

server_coefs <- coef(fit)[grepl("^server_name", names(coef(fit)))] %>%
  enframe(name = "server_name", value = "estimate") %>%
  mutate(server_name = str_remove(server_name, "^server_name"))

server_coefs <- server_coefs %>%
  add_row(server_name = levels(factor(reg_df$server_name))[1], estimate = 0) %>%
  arrange(server_name)

plot_df = left_join(avg_distance_df, server_coefs)

ggplot() +
  geom_point(data = plot_df, aes(x = dist_from_opt, estimate))

