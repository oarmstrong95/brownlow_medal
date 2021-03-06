#------------------------------------------------------------------------------
# NORMALISE DATA
#------------------------------------------------------------------------------
# Define function to normalise our numeric variables
normalise_fun <- function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

#------------------------------------------------------------------------------
# PLAYER CLUSTERING
#------------------------------------------------------------------------------
# Define function to convert stats into pca vars
get_pca_vars <- function(input_data, season_input) {
  
  # Define receipe
  pca_rec <- recipe(~., data = input_data) %>%
    # create id roles for variables not used in the model
    update_role(player_id, player_name, player_team, new_role = 'id') %>%
    # use all the remaining vars to reduce dimensionality
    step_pca(all_predictors())
  
  # Get PCA
  result <- pca_rec %>% prep() %>% juice() %>%
    mutate(season = season_input)
  
  return(result)
  
}

# Define function to do K-means clustering (Elbow analysis)
clustering_fun <- function(data) {
  
  subset_data <- data %>% select(PC1:PC5)
  
  set.seed(234)
  # Build a kmeans model
  model <- kmeans(x = subset_data, centers = 9)
  
  # Extract the cluster assignment vector from the kmeans model
  clust <- model$cluster
  
  # Generate the segmented the oes data frame
  result <- mutate(data, cluster = clust)
  
  return(result)
  
}

#------------------------------------------------------------------------------
# HYPER PARAMETER TUNING
#------------------------------------------------------------------------------
# Define function to check tuning parameters to optimize the grid search
# on the second tuning iteration
tuning_parameters_fun <- function(){
  
  graph <- ranger_tune %>%
    collect_metrics() %>%
    filter(.metric == "roc_auc") %>%
    select(mean, min_n, mtry) %>%
    pivot_longer(min_n:mtry,
                 values_to = "value",
                 names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = "AUC")
  
  return(graph)
  
}

# Define function to check accuracy through roc curve
roc_curve_fun <- function(data) {
  
  curve <- data %>%
    collect_predictions() %>%
    group_by(id) %>%
    roc_curve(brownlow_votes, .pred_0:.pred_3) %>%
    ggplot(aes(1 - specificity, sensitivity, color = id)) +
    geom_abline(lty = 2, color = "gray80", size = 1.5) +
    geom_path(show.legend = FALSE, alpha = 0.6, size = 1.2) +
    facet_wrap(~.level, ncol = 5) +
    coord_equal()
  
  return(curve)
  
}

#------------------------------------------------------------------------------
# CHECK VARIABLE IMPORTANCE
#------------------------------------------------------------------------------
var_importance <- function() {
  
  best <- ranger_tune %>%
    select_best("roc_auc")
  
  data_vip <- ranger_recipe %>% prep() %>% juice()
  
  graph <- finalize_model(ranger_spec, best) %>%
    set_engine("ranger", importance = "permutation") %>%
    fit(brownlow_votes ~ .,
        data = data_vip) %>%
    vip(geom = "point")
  
  return(graph)
  
}

#------------------------------------------------------------------------------
# MODEL METRICS
#------------------------------------------------------------------------------
# Get accuracy on test set
out_of_sample_accuracy <- function() {
  
  best <- ranger_tune %>%
    select_best("roc_auc")
  
  # Fit on entire training data
  ranger_test_check <- 
    workflow() %>% 
    add_recipe(ranger_recipe) %>% 
    add_model(finalize_model(ranger_spec, best)) %>%
    last_fit(split, metrics = metric_set(roc_auc, accuracy, sensitivity, specificity))
  
  # Check out of sample accuracy
  metrics <- ranger_test_check %>%
    collect_metrics()
  
  # Check roc graphs
  roc_curve <- ranger_test_check %>%
    roc_curve_fun()
  
  metrics <- list(metrics, roc_curve)
  
  return(metrics)
  
}

#------------------------------------------------------------------------------
# GENERATE BOXPLOT
#------------------------------------------------------------------------------
generate_boxplot <- function() {
  
  boxplot <- model_data %>%
    select(game_margin:bookmaker_prob_win) %>%
    pivot_longer(!brownlow_votes, 
                 names_to = "vars",
                 values_to = "values") %>%
    ggplot(aes(x = brownlow_votes, y = values)) + 
    geom_boxplot() +
    facet_wrap(~vars, scales = "free_y")
  
  return(boxplot)
  
}

#------------------------------------------------------------------------------
# TURN PROBABILITIES TO VOTES
#------------------------------------------------------------------------------
# Define function to turn probabilities into votes
# Get final predictions
predict_function <- function() {
  
  best <- ranger_tune %>%
    select_best("roc_auc")
  
  # Fit on entire data 
  ranger_final_model <- workflow() %>% 
    add_recipe(ranger_recipe) %>% 
    add_model(finalize_model(ranger_spec, best)) %>%
    fit(model_data)
  
  # Get the actual brownlow votes in history
  actuals <- model_data %>%
    select(season, match_id, player_id, player_name, player_team, brownlow_votes)

  # Get dimensions to allocate votes from model
  n_rows_original <- model_data %>%
    distinct(match_id) %>%
    nrow()

  # Only add a player error feature for players who have played 3 years
  players <- model_data %>%
    count(season, player_id, player_name) %>%
    group_by(player_id, player_name) %>%
    count() %>%
    filter(n == 3) %>%
    ungroup() %>%
    pull(player_id)

  # Apply model to entire data set in history
  historical_predictions <- predict(ranger_final_model, new_data = model_data, type = "prob") %>%
    bind_cols(model_data) %>%
    select(season, match_id, player_id, player_name, player_team, .pred_0:.pred_3) %>%
    mutate(expected_votes = (.pred_0 * 0) + (.pred_1 * 1) + (.pred_2 * 2) + (.pred_3 * 3)) %>%
    group_by(match_id) %>%
    slice_max(order_by = expected_votes, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(match_id, desc(expected_votes)) %>%
    mutate(predicted_votes = rep(c(3, 2, 1), n_rows_original)) %>%
    select(season, match_id, player_id, player_name, player_team, predicted_votes)

  # Find the players who are in the top 25% of predictions
  relevant_players1 <- historical_predictions %>%
    group_by(player_id, player_name) %>%
    summarize(total = sum(predicted_votes)) %>%
    ungroup() %>%
    arrange(desc(total)) %>%
    mutate(rank = percent_rank(total)) %>%
    filter(rank >= .75) %>%
    pull(player_id)

  # Find the players are who are in the top 25% of actual polling
  relevant_players2 <- model_data %>%
    mutate(brownlow_votes = as.numeric(as.character(brownlow_votes))) %>%
    group_by(player_id, player_name) %>%
    summarize(total = sum(brownlow_votes)) %>%
    ungroup() %>%
    arrange(desc(total)) %>%
    mutate(rank = percent_rank(total)) %>%
    filter(rank >= .75) %>%
    pull(player_id)

  # Dont down grade elite players
  elite_players <- model_data %>%
    mutate(brownlow_votes = as.numeric(as.character(brownlow_votes))) %>%
    group_by(player_id, player_name) %>%
    summarize(total = sum(brownlow_votes)) %>%
    ungroup() %>%
    arrange(desc(total)) %>%
    mutate(rank = percent_rank(total)) %>%
    head(9) %>%
    select(player_id, player_name) %>%
    mutate(elite_flag = 1)

  # Combine relevant players
  relevant_players <- unique(c(relevant_players1, relevant_players2))

  # Get player features var
  player_features <- historical_predictions %>%
    right_join(actuals) %>%
    filter(player_id %in% relevant_players) %>%
    arrange(season, match_id, player_name) %>%
    mutate(predicted_votes = if_else(is.na(predicted_votes), 0 , predicted_votes),
           brownlow_votes= as.numeric(as.character(brownlow_votes))) %>%
    filter(player_id %in% players) %>%
    group_by(season, player_id, player_name) %>%
    summarize(total_predicted = mean(predicted_votes),
              total_actuals = mean(brownlow_votes)) %>%
    mutate(delta = total_actuals - total_predicted) %>%
    ungroup() %>%
    mutate(weights = case_when(
      season == 2017 ~ 0.2,
      season == 2018 ~ 0.3,
      season == 2019 ~ 0.5
    )) %>%
    group_by(player_id, player_name) %>%
    summarize(delta = weighted.mean(delta, weights)) %>%
    arrange(delta) %>%
    left_join(elite_players) %>%
    mutate(drop_flag = if_else(delta < 0 & elite_flag == 1, 1, 0),
           drop_flag = if_else(is.na(drop_flag), 0, drop_flag)) %>%
    # mutate(delta = case_when(
      # player_name == "Gary Ablett" ~ 0,
      # player_name == "Travis Boak" ~ 0,
      # player_name == "Clayton Oliver" ~ 0,
      # player_name == "Jack Steele" ~ 0,
      # player_name == "Max Gawn" ~ 0,
      # player_name == "Taylor Adams" ~ delta*5,
      # player_name == "Zach Merrett" ~ delta*5,
      # player_name == "Josh Kelly" ~ delta*5,
      # player_name == "Jack Macrae" ~ delta*5,
      # TRUE ~ delta)) %>%
    filter(drop_flag != 1) %>%
    filter(delta != 0) %>%
    select(player_id, player_name, delta)

  # Get dimensions
  n_rows <- new_data %>%
    distinct(match_id) %>%
    nrow()
  
  # Get results on new data
  results <- 
    predict(ranger_final_model, new_data = new_data, type = "prob") %>%
    bind_cols(new_data) %>%
    select(match_id, player_id, player_name, player_team, .pred_0:.pred_3) %>%
    mutate(expected_votes = (.pred_0 * 0) + (.pred_1 * 1) + (.pred_2 * 2) + (.pred_3 * 3)) %>%
    left_join(player_features, by = c("player_id", "player_name")) %>%
    mutate(delta = if_else(is.na(delta), 0, delta),
           expected_votes = expected_votes + delta,
           expected_votes = if_else(expected_votes < 0, 0, expected_votes)) %>%
    group_by(match_id) %>%
    slice_max(order_by = expected_votes, n = 3, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(match_id, desc(expected_votes)) %>%
    mutate(predicted_votes = rep(c(3, 2, 1), n_rows))
  
  return(results)
  
}


#------------------------------------------------------------------------------
# CHARTS AND OUTPUTS
#------------------------------------------------------------------------------
# Define function to customise aesthics of table
gt_theme_538 <- function(data,...) {
  data %>%
    text_transform(
      locations = cells_body(vars(logo)),
      fn = function(x) {
        web_image(
          url = x,
          height = 25
        )
      }
    ) %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    )  %>% 
    # Relabel columns
    cols_label(
      logo = ""
    ) %>%
    tab_options(
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 16,
      heading.align = "left",
      ...
    ) 
}

# Define function to get top 20
totals_table <- function() {
  
  suppressMessages(
    append_data <- new_data %>%
      group_by(player_id, player_name, player_team) %>%
      summarize("Average Disposals" = mean(disposals),
                "Average Supercoach" = mean(supercoach_score))
  )
  
  suppressMessages(
    totals <- predicted_votes %>%
      group_by(player_id, player_name, player_team) %>%
      summarize(total = sum(predicted_votes)) %>%
      arrange(desc(total)) %>%
      left_join(append_data) %>%
      ungroup() %>%
      select(-player_id) %>%
      rename("Player Name" = player_name,
             "Team" = player_team, 
             "Total Predicted Votes" = total) %>%
      mutate(logo = case_when(
        Team == "Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/4/4e/Melbournefc.svg",
        Team == "Port Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/3/36/Port_Adelaide_Football_Club_logo.svg",
        Team == "Western Bulldogs" ~ "https://upload.wikimedia.org/wikipedia/en/0/09/Western_Bulldogs_logo.svg",
        Team == "Essendon" ~ "https://upload.wikimedia.org/wikipedia/en/8/8b/Essendon_FC_logo.svg",
        Team == "St Kilda" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/St_Kilda_FC_logo.svg",
        Team == "Geelong" ~ "https://upload.wikimedia.org/wikipedia/en/5/5f/Geelong_Cats_logo.svg",
        Team == "Collingwood" ~ "https://upload.wikimedia.org/wikipedia/en/a/a6/Collingwood_Football_Club_Logo_%282017%E2%80%93present%29.svg",
        Team == "Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/a/af/Sydney_Swans_Logo_2020.svg",
        Team == "Greater Western Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/0/07/GWS_Giants_logo.svg",
        Team == "Hawthorn" ~ "https://upload.wikimedia.org/wikipedia/en/6/62/Hawthorn-football-club-brand.svg",
        Team == "Carlton" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/Carlton_FC_Logo_2020.svg",
        Team == "Gold Coast" ~ "https://upload.wikimedia.org/wikipedia/en/7/7d/Gold_Coast_Suns_AFL_Logo.svg",
        Team == "West Coast" ~ "https://upload.wikimedia.org/wikipedia/en/b/b5/West_Coast_Eagles_logo_2017.svg",
        Team == "Fremantle" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
        Team == "Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
        Team == "North Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/f/fc/North_Melbourne_FC_logo.svg",
        Team == "Brisbane Lions" ~ "https://upload.wikimedia.org/wikipedia/en/c/c7/Brisbane_Lions_logo_2010.svg",
        TRUE ~ "https://upload.wikimedia.org/wikipedia/en/3/35/Richmond_Tigers_logo.svg")) %>%
      select(`Player Name`, logo, `Team`, `Average Disposals`:`Average Supercoach`, `Total Predicted Votes`) %>%
      head(15)
  )
  
  table <- totals %>%
    gt() %>%
    tab_spanner(
      label = "Season Averages",
      columns = 4:5
    ) %>%
    tab_header(
      title = md("2020 Brownlow Medal Predictions"),
      subtitle = md("The top 15 players are shown - their predicted votes have been added up for for each game to give a total across the season")
    ) %>%
    fmt_percent(columns = 4:5,
                decimals = 0
    ) %>%
    data_color(
      columns = vars(`Total Predicted Votes`),
      colors = scales::col_numeric(
        palette = c("white", "#3fc1c9"),
        domain = NULL
      )
    ) %>%
    tab_source_note(
      source_note = md("**Data**: fitzroy | **Table**: @oarmstrong95")
    ) %>%
    # tab_footnote(
    #   footnote = "Note: percentile average per game.", 
    #   locations = cells_column_labels(columns = 4:5)
    # ) %>%
    gt_theme_538()
  
  return(table)
  
}

generate_game_pics <- function(date, header, data) {
  
  table <- data %>%
    gt() %>%
    text_transform(
      locations = cells_body(vars(logo)),
      fn = function(x) {
        web_image(
          url = x,
          height = 20
        )
      }
    ) %>%
    # Relabel columns
    cols_label(
      logo = ""
    ) %>%
    data_color(
      columns = vars(`Predicted Votes`),
      colors = scales::col_numeric(
        palette = c("white", "#80df7c"),
        domain = NULL
      )
    ) %>%
    tab_header(title = md(header),
               subtitle = html(date)) %>%
    opt_align_table_header("left") %>%
    opt_all_caps()  %>%
    opt_table_font(
      font = list(
        google_font("Chivo"),
        default_fonts()
      )
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "bottom", color = "transparent", weight = px(2)
      ),
      locations = cells_body(
        columns = TRUE,
        # This is a relatively sneaky way of changing the bottom border
        # Regardless of data size
        rows = nrow(data$`_data`)
      )
    ) %>%
    tab_options(
      heading.subtitle.font.size = 10,
      column_labels.background.color = "white",
      table.border.top.width = px(3),
      table.border.top.color = "transparent",
      table.border.bottom.color = "transparent",
      table.border.bottom.width = px(3),
      column_labels.border.top.width = px(3),
      column_labels.border.top.color = "transparent",
      column_labels.border.bottom.width = px(3),
      column_labels.border.bottom.color = "black",
      data_row.padding = px(3),
      source_notes.font.size = 12,
      table.font.size = 12,
      heading.align = "left"
    )
  
  return(table)
  
}

game_by_game_table <- function() {
  
  game_label <- new_data %>%
    select(match_id, match_date, match_round, match_home_team, match_away_team) %>%
    distinct_all() %>%
    mutate(teams = paste0(match_home_team, " vs. ", match_away_team)) %>%
    select(-match_home_team, -match_away_team) %>%
    mutate(match_date = paste(wday(match_date, label = TRUE), day(match_date), month(match_date, label = TRUE), year(match_date)))
  
  result <- predicted_votes %>%
    left_join(game_label, by = "match_id") %>%
    select(match_round, match_date, teams, player_team, "Player Name" = player_name, "Predicted Votes" = predicted_votes) %>%
    mutate(logo = case_when(
      player_team == "Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/4/4e/Melbournefc.svg",
      player_team == "Port Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/3/36/Port_Adelaide_Football_Club_logo.svg",
      player_team == "Western Bulldogs" ~ "https://upload.wikimedia.org/wikipedia/en/0/09/Western_Bulldogs_logo.svg",
      player_team == "Essendon" ~ "https://upload.wikimedia.org/wikipedia/en/8/8b/Essendon_FC_logo.svg",
      player_team == "St Kilda" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/St_Kilda_FC_logo.svg",
      player_team == "Geelong" ~ "https://upload.wikimedia.org/wikipedia/en/5/5f/Geelong_Cats_logo.svg",
      player_team == "Collingwood" ~ "https://upload.wikimedia.org/wikipedia/en/a/a6/Collingwood_Football_Club_Logo_%282017%E2%80%93present%29.svg",
      player_team == "Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/a/af/Sydney_Swans_Logo_2020.svg",
      player_team == "Greater Western Sydney" ~ "https://upload.wikimedia.org/wikipedia/en/0/07/GWS_Giants_logo.svg",
      player_team == "Hawthorn" ~ "https://upload.wikimedia.org/wikipedia/en/6/62/Hawthorn-football-club-brand.svg",
      player_team == "Carlton" ~ "https://upload.wikimedia.org/wikipedia/en/5/58/Carlton_FC_Logo_2020.svg",
      player_team == "Gold Coast" ~ "https://upload.wikimedia.org/wikipedia/en/7/7d/Gold_Coast_Suns_AFL_Logo.svg",
      player_team == "West Coast" ~ "https://upload.wikimedia.org/wikipedia/en/b/b5/West_Coast_Eagles_logo_2017.svg",
      player_team == "Fremantle" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
      player_team == "Adelaide" ~ "https://upload.wikimedia.org/wikipedia/en/c/ca/Fremantle_FC_logo.svg",
      player_team == "North Melbourne" ~ "https://upload.wikimedia.org/wikipedia/en/f/fc/North_Melbourne_FC_logo.svg",
      player_team == "Brisbane Lions" ~ "https://upload.wikimedia.org/wikipedia/en/c/c7/Brisbane_Lions_logo_2010.svg",
      TRUE ~ "https://upload.wikimedia.org/wikipedia/en/3/35/Richmond_Tigers_logo.svg")) %>%
    select(match_round, match_date, teams, logo, `Player Name`, `Predicted Votes`) %>%
    group_by(match_round, match_date, teams) %>%
    nest() %>%
    mutate(table = pmap(list(match_date, teams, data), generate_game_pics))
  
  return(result)
  
}

