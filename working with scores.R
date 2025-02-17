
rm(list = ls())
library(tidyverse)

data_as_is <- read.csv('scores.csv')

group1 <- c('Vandals', 'Ajax', 'Dunesday', 'Nighthawks', 'Brits')
group2 <- c('Clann', 'Grumpys', 'Swamp Dragons', 'Tsunami', 'Du Nord')

produce_table <- function(data, G, session_n, season){
  full_join(
    x = 
      data %>% 
        filter(session %in% session_n & season == season) %>% 
        group_by(home) %>% 
        summarise(games_home = n(), 
                  wins_h = sum(ifelse(scorehome > scoreaway, 1, 0)),
                  ties_h = sum(ifelse(scorehome == scoreaway, 1, 0)),
                  losses_h = sum(ifelse(scorehome < scoreaway, 1, 0)), 
                  
                  goals_for_home = sum(scorehome), 
                  goals_against_home = sum(scoreaway)
                  ) %>% 
        ungroup() %>% 
        mutate(pts_home = 3 * wins_h + 1 * ties_h) %>% 
        rename(team = home) %>% 
        select(team, games_home, pts_home, goals_for_home, goals_against_home, 
               wins_h, ties_h, losses_h)
    , 
    y = 
      data %>% 
        filter(session == session_n, season == season) %>% 
        group_by(away) %>% 
        summarise(games_away = n(), 
                  wins_a = sum(ifelse(scorehome < scoreaway, 1, 0)),
                  ties_a = sum(ifelse(scorehome == scoreaway, 1, 0)),
                  losses_a = sum(ifelse(scorehome > scoreaway, 1, 0)), 
                  
                  goals_for_away = sum(scoreaway ), 
                  goals_against_away = sum(scorehome)
                  ) %>% 
        ungroup() %>% 
        mutate(pts_away = 3 * wins_a + 1 * ties_a) %>% 
        rename(team = away) %>% 
        select(team, games_away, pts_away, goals_for_away, goals_against_away, 
               wins_a, ties_a, losses_a)
    , 
    
    by = 'team'
  ) -> complete_record
  
  cols_with_missing <- apply(complete_record, 2, function(x){length(which(is.na(x)))})
  cols_with_missing <- cols_with_missing[cols_with_missing > 0]
  
  combined_table <- 
    complete_record %>% 
      mutate_at(vars(names(cols_with_missing)), ~ replace_na(., 0)) %>% 
      mutate(
        games = games_home + games_away, 
        wins = wins_h + wins_a, 
        ties = ties_h + ties_a, 
        losses = losses_h + losses_a, 
        total_pts = pts_home + pts_away, 
        goals_for = goals_for_home + goals_for_away, 
        goals_against = goals_against_home + goals_against_away,
        diff = goals_for - goals_against
      ) %>% 
      select(team, wins, ties, losses, total_pts, goals_for, goals_against, diff) %>% 
      mutate(group = ifelse(team %in% group1, 'G1', 'G2') )
  
  ##################
  
  combined_table %>% filter(group == G) %>% 
    # select(-group) %>% 
    arrange(-total_pts, -diff)
}

produce_table(data = data_as_is, G = 'G1', session_n = 1, season = "2024/2025")
produce_table(data = data_as_is, G = 'G2', session_n = 1, season = "2024/2025")

produce_table(data = data_as_is, 
              G = 'G1', 
              session_n = 2, 
              season = "2024/2025"
              )
produce_table(data = data_as_is, G = 'G2', session_n = 2, season = "2024/2025")


