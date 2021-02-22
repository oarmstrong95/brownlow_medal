

# Get results on new data
results <- 
  predict(ranger_final_model, new_data = new_data, type = "prob")

n_rows <- new_data %>%
  distinct(match_id) %>%
  nrow()

#votes <- 
  
results %>%
  bind_cols(new_data) %>%
  select(match_id, player_id, player_name, player_team, .pred_0:.pred_3) %>%
  mutate(expected_votes = (.pred_0 * 0) + (.pred_1 * 1) + (.pred_2 * 2) + (.pred_3 * 3)) %>%
  left_join(player_features, by = c("player_id", "player_name")) %>%
  mutate(delta = if_else(is.na(delta), 0, delta),
         updated_votes = expected_votes + delta,
         updated_votes = if_else(updated_votes < 0, 0, updated_votes)) %>%
  group_by(match_id) %>%
  slice_max(order_by = updated_votes, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(match_id, desc(updated_votes)) %>%
  mutate(predicted_votes = rep(c(3, 2, 1), n_rows))


  
  