#------------------------------------------------------------------------------
# DEFINE FUNCTION
#------------------------------------------------------------------------------
get_data <- function(MIN_YEAR, MAX_YEAR, type = c("redownload, import")){
  
  # Error handles
  if(missing(MIN_YEAR)) {
    
    MIN_YEAR <- 2017
    
  }
  
  if(missing(MAX_YEAR)) {
    
    MAX_YEAR <- 2020
    
  }
  
  # # If condition to either re run a download or import static file
  # if(type == "import") {
  #   
  #   data <- read.csv()
  #   
  #   return(data)
  #   
  # }
  
  #------------------------------------------------------------------------------
  # IMPORT DATA
  #------------------------------------------------------------------------------
  # Download match data
  match_data <- 
    fitzRoy::get_fryzigg_stats(start = MIN_YEAR - 1, end = MAX_YEAR) %>%
    as_tibble() %>%
    mutate(
      # create a season variable
      season = as.character(year(match_date)),
      # create a player name as a concat of first and last name
      player_name = paste(player_first_name, player_last_name),
      # create a player game outcome variable
      game_outcome = if_else(match_winner == player_team, "Win", "Loss"),
      game_margin = if_else(player_team == match_home_team, match_margin,
                            -match_margin))
  
  # Import supercoach data for 2020
  SC_2020 <- suppressWarnings(
    read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                    "01 Data Sets/2020_SC.csv"), 
             col_types = cols(X1 = col_skip()))
  )
  
  # Import supercoach data for 2020
  AF_2020 <- suppressWarnings(
    read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                    "01 Data Sets/2020_AF.csv"),
             col_types = cols(X1 = col_skip()))
  )
  
  # Import coaches votes
  coaches_votes <- suppressWarnings(
    read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                    "01 Data Sets/COACHES_VOTES.csv"), 
             col_types = cols(X1 = col_skip()))
  )
  
  #------------------------------------------------------------------------------
  # CLEAN DATA
  #------------------------------------------------------------------------------
  # filter out any games that has missing variables
  filter_out <- match_data %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    filter(is.na(effective_disposals)) %>%
    distinct(match_id) %>%
    pull()
  
  # Clean imported data
  match_data_clean <- suppressWarnings(
    match_data %>%
      select(
        # select game identifiers
        season, match_round, match_id, match_date, match_home_team, 
        match_away_team, game_outcome, game_margin,
        # select player identifiers
        player_id, player_name, player_team,
        # select player stats
        kicks:spoils
      ) %>%
      # cast variables to correct data formats
      mutate(season = as.integer(season),
             match_round = as.integer(match_round),
             match_home_team = as.factor(match_home_team),
             match_away_team = as.factor(match_away_team),
             player_id = as.integer(player_id),
             match_round = as.integer(match_round),
             brownlow_votes = as.factor(brownlow_votes)
      ) %>%
      # only keep rounds in the home and away season
      filter(!is.na(match_round)) %>%
      left_join(SC_2020,
                by = c("season", "match_round", "player_name", "player_team")) %>%
      mutate(supercoach_score = if_else(is.na(supercoach_score), 
                                        as.integer(supercoach),
                                        supercoach_score)) %>%
      select(-supercoach) %>%
      left_join(AF_2020,
                by = c("season", "match_round", "player_name", "player_team")) %>%
      mutate(afl_fantasy_score = if_else(is.na(afl_fantasy_score), 
                                         as.integer(aflfant),
                                         afl_fantasy_score)) %>%
      select(-aflfant, -web_scraped_team) %>%
      filter(!(match_id %in% filter_out)) %>%
      drop_na()
  )
  
  # Check for missing data
  check1 <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    miss_var_summary() %>%
    dplyr::slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check1 == 0)
  
  #------------------------------------------------------------------------------
  # ADD CAREER AVERAGE BROWNLOW VOTES
  #------------------------------------------------------------------------------
  # Get unique players in data
  players_unique <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    distinct(player_id) %>%
    pull()
  
  # Create a complete df so that each player has all the years
  # Balanced df is important for when creating the rolling average
  df <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    distinct(player_id, player_name)
  
  # Get the seasons for the average brownlow votes
  # Exclude the most recent season as we do not have observations for it
  seasons <- match_data_clean %>%
    filter(season != MAX_YEAR) %>%
    distinct(season) %>%
    pull()
  
  # Get dimensions for tibble
  dimension1 <- df %>% nrow()
  dimension2 <- seasons %>% length()
  joinYear <- tibble(season = rep(seasons, dimension1))
  
  # Build data frame
  # This includes all the players across 2017 and 2020
  # But includes data from 2016 to 2019
  complete_df <- do.call("rbind", 
                         replicate(dimension2, df, simplify = FALSE)) %>%
    arrange(player_id, player_name) %>%
    bind_cols(joinYear) %>%
    mutate(mean_votes_pg = 0)
  
  # Create rolling average for avg brownlow votes feature
  roll_avg_votes_pg <- suppressMessages(
    match_data_clean %>%
      # filter for players between 2017 and 2020
      filter(player_id %in% players_unique) %>%
      # calc avg votes per game across 2016 and 2019
      group_by(player_id, player_name, season) %>%
      summarize(mean_votes_pg = 
                  mean(as.numeric(as.character(brownlow_votes)))) %>%
      ungroup() %>%
      # add all the years for each player to get a rolling average
      full_join(complete_df) %>%
      group_by(player_id, player_name, season) %>%
      summarize(mean_votes_pg = sum(mean_votes_pg, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(season = season + 1) %>%
      group_by(player_id, player_name) %>%
      mutate(roll_avg = order_by(season, cummean(mean_votes_pg))) %>%
      ungroup() %>%
      filter(between(season, MIN_YEAR, MAX_YEAR))
  )
  
  # Build final df
  match_data_df <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    left_join(roll_avg_votes_pg,
              by = c("season", "player_id", "player_name")) %>%
    left_join(coaches_votes,
              by = c("season", "match_round", "player_name", "player_team")) %>%
    rename(coaches_votes = vote) %>%
    mutate(coaches_votes = replace(coaches_votes, is.na(coaches_votes), 0)) %>%
    distinct_all()
  
  # Check for missing
  check2 <- match_data_df %>%
    miss_var_summary() %>%
    dplyr::slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check2 == 0)
  
  #------------------------------------------------------------------------------
  # FINALISE DATA FOR MODEL
  #------------------------------------------------------------------------------
  # Create a final df for the modelling
  match_data_pre_clust <- match_data_df %>%
    select(season, match_round, match_id, match_date, match_home_team,
           match_away_team, game_outcome, game_margin,
           player_id, player_name, player_team, 
           roll_avg, coaches_votes, brownlow_votes,
           everything()) %>%
    group_by(season, match_round, match_id) %>%
    mutate_at(vars(kicks:spoils, mean_votes_pg, roll_avg), normalise_fun) %>%
    ungroup() %>%
    mutate_at(vars(kicks:spoils), as.numeric) %>%
    mutate_if(is.numeric, ~replace(., is.nan(.), 0)) #%>%
  # # normalise by season
  # group_by(season) %>%
  # mutate(game_margin = normalise_fun(game_margin)) %>%
  # ungroup()
  
  # Check for missing
  check3 <- match_data_pre_clust %>%
    miss_var_summary() %>%
    dplyr::slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check3 == 0)
  
  #------------------------------------------------------------------------------
  # APPEND IN PLAYER CLUSTER
  #------------------------------------------------------------------------------
  # Get clusters by season
  player_positions <- 
    match_data_pre_clust %>%
    select(-c(match_round, match_date, match_home_team,
              match_away_team, game_outcome, game_margin,
              match_id, brownlow_votes, coaches_votes,
              mean_votes_pg, roll_avg)) %>%
    group_by(season, player_id, player_name, player_team) %>%
    summarize_if(is.double, mean) %>%
    group_by(season) %>%
    nest() %>%
    mutate(pca = map(data, get_pca_vars, season),
           clusters = map(pca, clustering_fun)) %>%
    ungroup() %>%
    select(clusters) %>%
    unnest(cols = c(clusters)) %>%
    select(season, player_id, player_name, player_team, cluster)
  
  # Import mapping file
  cluster_mapping <- 
    read_excel("Z:/My Documents/GitHub/brownlow_medal/01 Data Sets/cluster_mapping.xlsx") %>%
    clean_names() %>%
    mutate(cluster = as.integer(cluster)) %>%
    rename(season = year)
  
  #------------------------------------------------------------------------------
  # GET MATCH PROBS
  #------------------------------------------------------------------------------
  suppressWarnings(
    game_probabilties <- 
      read_csv("Z:/My Documents/GitHub/brownlow_medal/01 Data Sets/game_probabilties.csv", 
               col_types = cols(X1 = col_skip())) %>%
      mutate(match_date = as.character(match_date)) %>%
      # remove errors in the file
      filter(!(match_date == "2017-07-01" & player_team == "Greater Western Sydney" & bookmaker_prob_win == .28)) %>%
      filter(!(match_date == "2017-07-08" & player_team == "Hawthorn" & bookmaker_prob_win == .38)) %>%
      filter(!(match_date == "2017-07-30" & player_team == "Collingwood" & bookmaker_prob_win == .37)) %>%
      filter(!(match_date == "2018-04-21" & player_team == "St Kilda" & bookmaker_prob_win == 0.290)) %>%
      filter(!(match_date == "2020-08-12" & player_team == "Gold Coast" & bookmaker_prob_win == 0.43)) %>%
      filter(!(match_date == "2020-06-11" & player_team == "Collingwood" & bookmaker_prob_win == 0.39))  
  )
  
  #------------------------------------------------------------------------------
  # FINAL DATASET
  #------------------------------------------------------------------------------
  # Clean and get final total data
  match_data_final <- player_positions %>%
    # append in cluster
    left_join(cluster_mapping, by = c("season", "cluster")) %>%
    arrange(season, cluster) %>%
    select(season, player_id, player_name, player_team , cluster = final_id) %>%
    right_join(match_data_pre_clust,
               by = c("season", "player_id", "player_name", "player_team")) %>%
    arrange(season, match_round, match_id, player_team, player_id) %>%
    # append in probabilities
    left_join(game_probabilties,
              by = c("season", "player_team", "match_round", "match_date")) %>%
    mutate(upset_expected = case_when(
      game_outcome == "Win" & bookmaker_prob_win <= 0.5 & bookmaker_prob_win > 0.25 ~ "Upset_Win",
      game_outcome == "Win" & bookmaker_prob_win <= 0.25 ~ "Big_upset_Win",
      game_outcome == "Loss" & bookmaker_prob_win >= 0.5 ~ "Upset_Loss",
      game_outcome == "Win" & bookmaker_prob_win > 0.5 ~ "Expected_Win",
      game_outcome == "Loss" & bookmaker_prob_win < 0.5 ~ "Expected_Loss")) %>%
    mutate(real_impact_proxy = case_when(
      supercoach_score >= .70 & supercoach_score < .85 & coaches_votes < 5 ~ "Moderate impact",
      supercoach_score >= .85 & supercoach_score < .9 & coaches_votes < 5 ~ "Some impact",
      supercoach_score >= .9 & supercoach_score < .95 & coaches_votes < 5 ~ "Pseudo impact",
      supercoach_score >= .95 & supercoach_score < .99 & coaches_votes < 5 ~ "Low impact",
      supercoach_score >= .99 & coaches_votes < 3 ~ "No impact",
      TRUE ~  "Impact Rewarded")) %>%
    select(-c(contest_def_losses, time_on_ground_percentage,
              behinds, bounces, free_kicks_against,
              free_kicks_for, tackles_inside_fifty,
              disposal_efficiency_percentage, 
              intercept_marks, contested_marks,
              contest_off_one_on_ones, bookmaker_prob_win)) %>%
    select(!contains("ruck")) %>%
    select(!contains("hitout"))
  
  #------------------------------------------------------------------------------
  # SPLIT FOR MODEL DATA vs NEW DATA
  #------------------------------------------------------------------------------
  # Split for new data
  new_data <- match_data_final %>%
    filter(season == 2020) %>%
    select(-brownlow_votes)
  
  # Split for model data
  model_data <- match_data_final %>%
    filter(season != 2020)
  
  output <- list(model_data, new_data)
  
  return(output)
  
}







