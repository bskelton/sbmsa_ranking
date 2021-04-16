games <- games_raw %>%
  distinct() %>%
  clean_names() %>%
  mutate(
    home_team = ifelse(is.na(home_team), home, home_team ),
    away_team = ifelse(is.na(away_team), away, away_team )
  ) %>%
  select(-c('home', 'away')) %>%
  mutate(
    date = parse_date(paste0(date,"/2021"), '%a %m/%d/%Y')
  ) 


# ELO Calculations
e = elo.run(score(home_score, away_score) ~ home_team + away_team, 
            data = games, 
            subset = home_score != away_score, 
            k = 90,
            skip = 2)
summary(e)
elo_data <- as.data.frame(e)

# add predictions
games <- games %>%
  # rowwise %>%
  mutate(
    p = predict(e, newdata = data.frame(home_team = home_team, away_team = away_team))
  )

standings <- standings_raw %>%
  mutate(
    GB = ifelse(GB == '--', 0, as.integer(GB))
  ) %>%
  clean_names()
 
home_game_log <- games %>%
  # filter(!is.na(home_score)) %>%
  select(date,
         time,
         team = home_team,
         team_score = home_score,
         opponent=away_team,
         opponent_score = away_score
        ) %>%
  mutate(
    home_away = "Home",
    result = ifelse(team_score == opponent_score, "T", ifelse(team_score > opponent_score, "W", "L")),
    season = ifelse(date < '2021-03-28', 'Preseason', 'Regular')
  )

vis_game_log <- games %>%
  # filter(!is.na(home_score)) %>%
  select(date,
         time,
         team = away_team,
         team_score = away_score,
         opponent=home_team,
         opponent_score = home_score
  ) %>%
  mutate(
    home_away = "Away",
    result = ifelse(team_score == opponent_score, "T", ifelse(team_score > opponent_score, "W", "L")),
    season = ifelse(date < '2021-03-28', 'Preseason', 'Regular')
  )

team_game_log <- rbind(home_game_log, vis_game_log)

team_game_log = team_game_log %>%
  left_join(
    select(standings, team, "opp_pct" = pct),
    by = c("opponent" = "team")
  )
 
 
team_game_stats <- team_game_log %>%
   filter(!is.na(team_score)) %>%
   group_by(team) %>% 
   summarise(
     runs_for = sum(team_score), 
     runs_against = sum(opponent_score), 
     runs_net = runs_for - runs_against,
     games_played = n(),
     games_played_preseason = sum(ifelse(season == 'Preseason', 1, 0)),
     games_played_regular = sum(ifelse(season != 'Preseason', 1, 0)),
     runs_for_preseson = sum(team_score[season == 'Preseason']), 
     runs_against_preseson = sum(opponent_score[season == 'Preseason']), 
     runs_net_preseson = runs_for_preseson - runs_against_preseson,
     runs_for_regular = sum(team_score[season != 'Preseason']), 
     runs_against_regular = sum(opponent_score[season != 'Preseason']), 
     runs_net_regular = runs_for_regular - runs_against_regular,
     avg_runs_for_preseson = mean(team_score[season == 'Preseason']), 
     avg_runs_against_preseson = mean(opponent_score[season == 'Preseason']), 
     avg_runs_net_preseson = avg_runs_for_preseson - avg_runs_against_preseson,
     avg_runs_for_regular = round(mean(team_score[season != 'Preseason']), 2),
     avg_runs_against_regular = round(mean(opponent_score[season != 'Preseason']), 2),
     avg_runs_net_regular = avg_runs_for_regular - avg_runs_against_regular,     
    ) 
 
standings <- standings %>% 
  rowwise %>% 
  mutate(
    sos = calc_sos(team)$pct
 )

elo_ranking = tibble::rownames_to_column(as.data.frame(final.elos(e)), "team") %>%
  rename(elo_rating = 'final.elos(e)') %>%
  mutate(rank = dense_rank(desc(elo_rating)))

standings <- standings %>%
  left_join(
    team_game_stats
  ) %>%
  left_join(
    elo_ranking
  ) %>%
  mutate(
    team = as.factor(team),
    division = as.factor(division)
  )
