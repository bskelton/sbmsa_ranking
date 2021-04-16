print_standings <- function(data) {
  data %>%
  arrange(desc(pct)) %>%
  select(
    Team = team,
    Wins = w, 
    Losses = l,
    Ties = t,
    Games = gp,
    "Win %"= pct,
    "Division" = division,
    "RF" = runs_for_regular,
    "RA" = runs_against_regular,
    "Net" = runs_net_regular,
    "aRF" = avg_runs_for_regular, 
    "aRA" = avg_runs_against_regular, 
    "aNet" = avg_runs_net_regular,
    "SOS" = sos,
    'Elo Rating' = elo_rating,
    'Elo Rank' = rank
    )
}

