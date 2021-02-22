#------------------------------------------------------------------------------
# GLOBAL VARIABLES
#------------------------------------------------------------------------------
# Load packages
require(rvest)
require(httr)

# Create an empty df to webscrape
return_df_2017 <- tibble(match = NA,
                    date = NA,
                    type = NA,
                    prob = NA,
                    team_won = NA)

return_df_2018 <- tibble(match = NA,
                    date = NA,
                    type = NA,
                    prob = NA,
                    team_won = NA)

return_df_2019 <- tibble(match = NA,
                    date = NA,
                    type = NA,
                    prob = NA,
                    team_won = NA)

return_df_2020 <- tibble(match = NA,
                    date = NA,
                    type = NA,
                    prob = NA,
                    team_won = NA)
#------------------------------------------------------------------------------
# 2017 LOOP
#------------------------------------------------------------------------------
# Define loop to get probabilities of team winning according
# to the book makers
for (i in 1:336) {
  
  url <- paste0("https://squiggle.com.au/game/?gid=", i)
  
  table <- read_html(url) %>%
    html_table()
  
  match_id <- read_html(url) %>%
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  df <- table[[1]] %>%
    as_tibble(validate = F)
  
  winner <- read_html(url) %>%
    html_nodes(xpath = "//p") %>%
    html_text()
  
  names <- c("type", "tip", "prob", "margin", "result", "mae", "bits")
  
  colnames(df) <- names
  
  clean_df <- df %>%
    filter(type == "Punters") %>%
    select(type, prob, result) %>%
    mutate(match = match_id[1],
           date = match_id[2]) %>%
    select(match, date, type, prob, result) %>%
    mutate(team_won = winner[[1]])
  
  return_df_2017 <- return_df_2017 %>%
    bind_rows(clean_df)
  
}

# Clean 2017
probabilities_2017 <- return_df_2017 %>%
  drop_na() %>%
  rename(round = date) %>%
  separate(match, into = c("teams", "day"), sep = "\n") %>%
  separate(day, into = "match_date", sep = " ") %>%
  separate(round, into = c("junk", "match_round"), sep = " ") %>%
  select(-junk) %>%
  mutate(match_round = as.integer(str_remove_all(match_round, ","))) %>%
  filter(match_round <= 23) %>%
  mutate(season = str_sub(match_date, start = 1, end = 4),
         prob = as.numeric(str_remove_all(prob, "%"))/100) %>%
  mutate(team_won = str_remove_all(team_won, "Match complete"),
         index_1 = str_locate(team_won, "won")[,1],
         team_won = str_trim(str_sub(team_won, start = 1, end = index_1 - 2))) %>%
  separate(teams, into = c("team1", "team2"), sep = "v") %>%
  mutate_at(vars(team1, team2), str_trim) %>%
  mutate(team_lost = case_when(
    team1 == team_won ~ team2,
    TRUE ~ team1)) %>%
  select(season, match_date, match_round, team_won, team_lost, prob, result) %>%
  pivot_longer(team_won:team_lost,
               names_to = "outcome",
               values_to = "player_team") %>%
  mutate(final_prob = case_when(
    result == "???" & outcome == "team_won" ~ prob,
    result == "???" & outcome == "team_lost" ~ 1 - prob,
    result == "???" & outcome == "team_won" ~ 1 - prob,
    result == "???" & outcome == "team_lost" ~ prob)) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, match_date, match_round, player_team, bookmaker_prob_win = final_prob) %>%
  # add 3 games which are drawn
  add_row(season = 2017, match_date = "2017-07-01", match_round = 15, player_team = "Geelong", bookmaker_prob_win = .33) %>%
  add_row(season = 2017, match_date = "2017-07-01", match_round = 15, player_team = "Greater Western Sydney", bookmaker_prob_win = .67) %>%
  add_row(season = 2017, match_date = "2017-07-08", match_round = 16, player_team = "Greater Western Sydney", bookmaker_prob_win = .61) %>%
  add_row(season = 2017, match_date = "2017-07-08", match_round = 16, player_team = "Hawthorn", bookmaker_prob_win = .39) %>%
  add_row(season = 2017, match_date = "2017-07-30", match_round = 19, player_team = "Collingwood", bookmaker_prob_win = .34) %>%
  add_row(season = 2017, match_date = "2017-07-30", match_round = 19, player_team = "Adelaide", bookmaker_prob_win = .66) %>%
  drop_na()

#------------------------------------------------------------------------------
# 2018 LOOP
#------------------------------------------------------------------------------
for (i in 372:689) {
  
  url <- paste0("https://squiggle.com.au/game/?gid=", i)
  
  table <- read_html(url) %>%
    html_table()
  
  match_id <- read_html(url) %>%
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  df <- table[[1]] %>%
    as_tibble(validate = F)
  
  winner <- read_html(url) %>%
    html_nodes(xpath = "//p") %>%
    html_text()
  
  names <- c("type", "tip", "prob", "margin", "result", "mae", "bits")
  
  colnames(df) <- names
  
  clean_df <- df %>%
    filter(type == "Punters") %>%
    select(type, prob, result) %>%
    mutate(match = match_id[1],
           date = match_id[2]) %>%
    select(match, date, type, prob, result) %>%
    mutate(team_won = winner[[1]])
  
  return_df_2018 <- return_df_2018 %>%
    bind_rows(clean_df)
  
}

# Clean 2018
probabilities_2018 <- return_df_2018 %>%
  drop_na() %>%
  rename(round = date) %>%
  separate(match, into = c("teams", "day"), sep = "\n") %>%
  separate(day, into = "match_date", sep = " ") %>%
  separate(round, into = c("junk", "match_round"), sep = " ") %>%
  select(-junk) %>%
  mutate(match_round = as.integer(str_remove_all(match_round, ","))) %>%
  filter(match_round <= 23) %>%
  mutate(season = str_sub(match_date, start = 1, end = 4),
         prob = as.numeric(str_remove_all(prob, "%"))/100) %>%
  mutate(team_won = str_remove_all(team_won, "Match complete"),
         index_1 = str_locate(team_won, "won")[,1],
         team_won = str_trim(str_sub(team_won, start = 1, end = index_1 - 2))) %>%
  separate(teams, into = c("team1", "team2"), sep = "v") %>%
  mutate_at(vars(team1, team2), str_trim) %>%
  mutate(team_lost = case_when(
    team1 == team_won ~ team2,
    TRUE ~ team1)) %>%
  select(season, match_date, match_round, team_won, team_lost, prob, result) %>%
  pivot_longer(team_won:team_lost,
               names_to = "outcome",
               values_to = "player_team") %>%
  mutate(final_prob = case_when(
    result == "???" & outcome == "team_won" ~ prob,
    result == "???" & outcome == "team_lost" ~ 1 - prob,
    result == "???" & outcome == "team_won" ~ 1 - prob,
    result == "???" & outcome == "team_lost" ~ prob)) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, match_date, match_round, player_team, bookmaker_prob_win = final_prob) %>%
  add_row(season = 2018, match_date = "2018-04-21", match_round = 5, player_team = "St Kilda", bookmaker_prob_win = .33) %>%
  add_row(season = 2018, match_date = "2018-04-21", match_round = 5, player_team = "Greater Western Sydney", bookmaker_prob_win = .67) %>%
  drop_na()

#------------------------------------------------------------------------------
# 2019 LOOP
#------------------------------------------------------------------------------
for (i in 3049:3386) {
  
  url <- paste0("https://squiggle.com.au/game/?gid=", i)
  
  table <- read_html(url) %>%
    html_table()
  
  match_id <- read_html(url) %>%
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  df <- table[[1]] %>%
    as_tibble(validate = F)
  
  winner <- read_html(url) %>%
    html_nodes(xpath = "//p") %>%
    html_text()
  
  names <- c("type", "tip", "prob", "margin", "result", "mae", "bits")
  
  colnames(df) <- names
  
  clean_df <- df %>%
    filter(type == "Punters") %>%
    select(type, prob, result) %>%
    mutate(match = match_id[1],
           date = match_id[2]) %>%
    select(match, date, type, prob, result) %>%
    mutate(team_won = winner[[1]])
  
  return_df_2019 <- return_df_2019 %>%
    bind_rows(clean_df)
  
}

# Clean 2019
probabilities_2019 <- return_df_2019 %>%
  drop_na() %>%
  rename(round = date) %>%
  separate(match, into = c("teams", "day"), sep = "\n") %>%
  separate(day, into = "match_date", sep = " ") %>%
  separate(round, into = c("junk", "match_round"), sep = " ") %>%
  select(-junk) %>%
  mutate(match_round = as.integer(str_remove_all(match_round, ","))) %>%
  filter(match_round <= 23) %>%
  mutate(season = str_sub(match_date, start = 1, end = 4),
         prob = as.numeric(str_remove_all(prob, "%"))/100) %>%
  mutate(team_won = str_remove_all(team_won, "Match complete"),
         index_1 = str_locate(team_won, "won")[,1],
         team_won = str_trim(str_sub(team_won, start = 1, end = index_1 - 2))) %>%
  separate(teams, into = c("team1", "team2"), sep = "v") %>%
  mutate_at(vars(team1, team2), str_trim) %>%
  mutate(team_lost = case_when(
    team1 == team_won ~ team2,
    TRUE ~ team1)) %>%
  select(season, match_date, match_round, team_won, team_lost, prob, result) %>%
  pivot_longer(team_won:team_lost,
               names_to = "outcome",
               values_to = "player_team") %>%
  mutate(final_prob = case_when(
    result == "???" & outcome == "team_won" ~ prob,
    result == "???" & outcome == "team_lost" ~ 1 - prob,
    result == "???" & outcome == "team_won" ~ 1 - prob,
    result == "???" & outcome == "team_lost" ~ prob)) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, match_date, match_round, player_team, bookmaker_prob_win = final_prob)

#------------------------------------------------------------------------------
# 2020 LOOP
#------------------------------------------------------------------------------
for (i in 4396:5599) {
  
  url <- paste0("https://squiggle.com.au/game/?gid=", i)
  
  table <- read_html(url) %>%
    html_table()
  
  match_id <- read_html(url) %>%
    html_nodes(xpath = "//h5") %>%
    html_text()
  
  df <- table[[1]] %>%
    as_tibble(validate = F)
  
  winner <- read_html(url) %>%
    html_nodes(xpath = "//p") %>%
    html_text()
  
  names <- c("type", "tip", "prob", "margin", "result", "mae", "bits")
  
  colnames(df) <- names
  
  clean_df <- df %>%
    filter(type == "Punters") %>%
    select(type, prob, result) %>%
    mutate(match = match_id[1],
           date = match_id[2]) %>%
    select(match, date, type, prob, result) %>%
    mutate(team_won = winner[[1]])
  
  return_df_2020 <- return_df_2020 %>%
    bind_rows(clean_df)
  
}

# Clean 2020
probabilities_2020 <- return_df_2020 %>%
  drop_na() %>%
  rename(round = date) %>%
  separate(match, into = c("teams", "day"), sep = "\n") %>%
  separate(day, into = "match_date", sep = " ") %>%
  separate(round, into = c("junk", "match_round"), sep = " ") %>%
  select(-junk) %>%
  mutate(match_round = as.integer(str_remove_all(match_round, ","))) %>%
  filter(match_round <= 23) %>%
  mutate(season = str_sub(match_date, start = 1, end = 4),
         prob = as.numeric(str_remove_all(prob, "%"))/100) %>%
  mutate(team_won = str_remove_all(team_won, "Match complete"),
         index_1 = str_locate(team_won, "won")[,1],
         team_won = str_trim(str_sub(team_won, start = 1, end = index_1 - 2))) %>%
  separate(teams, into = c("team1", "team2"), sep = "v") %>%
  mutate_at(vars(team1, team2), str_trim) %>%
  mutate(team_lost = case_when(
    team1 == team_won ~ team2,
    TRUE ~ team1)) %>%
  select(season, match_date, match_round, team_won, team_lost, prob, result) %>%
  pivot_longer(team_won:team_lost,
               names_to = "outcome",
               values_to = "player_team") %>%
  mutate(final_prob = case_when(
    result == "???" & outcome == "team_won" ~ prob,
    result == "???" & outcome == "team_lost" ~ 1 - prob,
    result == "???" & outcome == "team_won" ~ 1 - prob,
    result == "???" & outcome == "team_lost" ~ prob)) %>%
  mutate(season = as.numeric(season)) %>%
  select(season, match_date, match_round, player_team, bookmaker_prob_win = final_prob) %>%
  add_row(season = 2020, match_date = "2020-06-11", match_round = 2, player_team = "Richmond", bookmaker_prob_win = .55) %>%
  add_row(season = 2020, match_date = "2020-06-11", match_round = 2, player_team = "Collingwood", bookmaker_prob_win = .45) %>%
  add_row(season = 2020, match_date = "2020-08-12", match_round = 11, player_team = "Essendon", bookmaker_prob_win = .50) %>%
  add_row(season = 2020, match_date = "2020-08-12", match_round = 11, player_team = "Gold Coast", bookmaker_prob_win = .50) %>%
  add_row(season = 2020, match_date = "2020-09-20", match_round = 18, player_team = "Sydney", bookmaker_prob_win = .22) %>%
  add_row(season = 2020, match_date = "2020-09-20", match_round = 18, player_team = "Geelong", bookmaker_prob_win = .78) %>%
  drop_na()

#------------------------------------------------------------------------------
# CHECK AND EXPORT
#------------------------------------------------------------------------------
setwd("Z:/My Documents/GitHub/brownlow_medal/01 Data Sets")
probabilities_2017 %>%
  bind_rows(probabilities_2018) %>%
  bind_rows(probabilities_2019) %>%
  bind_rows(probabilities_2020) %>%
  write.csv("game_probabilties.csv")
