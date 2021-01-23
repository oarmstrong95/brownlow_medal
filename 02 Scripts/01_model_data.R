#------------------------------------------------------------------------------
# DEFINE FUNCTION
#------------------------------------------------------------------------------
get_data <- function(MIN_YEAR, MAX_YEAR){
  
  #------------------------------------------------------------------------------
  # IMPORT DATA
  #------------------------------------------------------------------------------
  # Dowload match data
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
  SC_2020 <- read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                             "01 Data Sets/2020_SC.csv"), 
                      col_types = cols(X1 = col_skip()))
  
  # Import supercoach data for 2020
  AF_2020 <- read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                             "01 Data Sets/2020_AF.csv"),
                      col_types = cols(X1 = col_skip()))
  
  # Import coaches votes
  coaches_votes <- read_csv(paste0("Z:/My Documents/GitHub/brownlow_medal/",
                                   "01 Data Sets/COACHES_VOTES.csv"), 
                            col_types = cols(X1 = col_skip()))
  
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
  match_data_clean <- match_data %>%
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
    left_join(SC_2020) %>%
    mutate(supercoach_score = if_else(is.na(supercoach_score), 
                                      as.integer(supercoach),
                                      supercoach_score)) %>%
    select(-supercoach) %>%
    left_join(AF_2020) %>%
    mutate(afl_fantasy_score = if_else(is.na(afl_fantasy_score), 
                                       as.integer(aflfant),
                                       afl_fantasy_score)) %>%
    select(-aflfant, -web_scraped_team) %>%
    filter(!(match_id %in% filter_out)) %>%
    drop_na()
  
  # Check for missing data
  check1 <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    miss_var_summary() %>%
    slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check1 == 0)
  
  #------------------------------------------------------------------------------
  # FEATURE CREATION
  #------------------------------------------------------------------------------
  # 1. Average Brownlow Votes Per Game Across Data Set
  
  # Get unique players in latest model year
  players_unique <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    distinct(player_id) %>%
    pull()
  
  # Create a complete df so that each player has all the years
  # Balanced df is important for when creating the rolling average
  df <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    distinct(player_id, player_name)
  
  seasons <- match_data_clean %>%
    distinct(season) %>%
    pull()
  
  # Get dimensions for tibble
  dimension1 <- df %>% nrow()
  dimension2 <- seasons %>% length()
  
  joinYear <- tibble(season = rep(seasons, dimension1))
  
  # Build data frame
  do.call("rbind", replicate(dimension2, df, simplify = FALSE)) %>%
    arrange(player_id, player_name) %>%
    bind_cols(joinYear) %>%
    mutate(mean_votes_pg = 0)
  
  # Create rolling average for avg brownlow votes feature
  roll_avg_votes_pg <- match_data_clean %>%
    filter(player_id %in% players_unique) %>%
    group_by(player_id, player_name, season) %>%
    summarize(mean_votes_pg = 
                mean(as.numeric(as.character(brownlow_votes)))) %>%
    ungroup() %>%
    full_join(complete_df) %>%
    group_by(player_id, player_name, season) %>%
    summarize(mean_votes_pg = sum(mean_votes_pg, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(season = season + 1) %>%
    group_by(player_id, player_name) %>%
    mutate(roll_avg = order_by(season, cummean(mean_votes_pg))) %>%
    ungroup() %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    select(-mean_votes_pg)
  
  #------------------------------------------------------------------------------
  # BUILD FINAL MATCH DATA 
  #------------------------------------------------------------------------------
  # Build final df
  match_data_final <- match_data_clean %>%
    filter(between(season, MIN_YEAR, MAX_YEAR)) %>%
    left_join(roll_avg_votes_pg) %>%
    left_join(coaches_votes) %>%
    rename(coaches_votes = vote) %>%
    mutate(coaches_votes = replace(coaches_votes, is.na(coaches_votes), 0))
  
  # Check for missing
  check2 <- match_data_final %>%
    miss_var_summary() %>%
    slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check2 == 0)
  
  #------------------------------------------------------------------------------
  # FINALISE DATA FOR MODEL
  #------------------------------------------------------------------------------
  # Create a final df for the modelling
  model_data <- match_data_final %>%
    select(season, match_round, match_id, match_date, match_home_team,
           match_away_team, game_outcome, game_margin,
           player_id, player_name, player_team, 
           roll_avg, coaches_votes, brownlow_votes,
           everything()) %>%
    group_by(season, match_round, match_id) %>%
    mutate_at(vars(kicks:spoils), normalise_fun) %>%
    ungroup() %>%
    mutate_at(vars(kicks:spoils), as.numeric) %>%
    mutate_if(is.numeric, ~replace(., is.nan(.), 0)) %>%
    select(!contains("ruck")) %>%
    select(!contains("hitout")) %>%
    select(-c(contest_def_losses, time_on_ground_percentage,
              behinds, bounces, free_kicks_against,
              free_kicks_for, tackles_inside_fifty,
              disposal_efficiency_percentage,
              intercepts, intercept_marks, contested_marks,
              contest_off_one_on_ones))
  
  # Check for missing
  check3 <- model_data %>%
    miss_var_summary() %>%
    slice(1) %>%
    select(n_miss) %>%
    pull()
  
  stopifnot(check3 == 0)
  
  return(model_data)
  
}







