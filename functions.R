download_files <- function() {
  download.file("https://sbmsa.org/schedule/309979/pee-wee-bregman", "./data/bregman.html", mode = "wb")
  download.file("https://sbmsa.org/schedule/309978/pee-wee-altuve", "./data/altuve.html", mode = "wb")
  download.file("https://sbmsa.org/schedule/309980/pee-wee-correa", "./data/correa.html", mode = "wb")
}

calc_win_percentage <- function(w, l, t) {
  return (w + (0.5 * t))/( w + l + t)
}

avg_win_pct <- function(teams) {
  standings %>%
    filter(
      team %in% teams
    ) %>%
    summarise(pct = mean(pct))
}

played_games <- function(teams) {
  team_game_log %>%
    filter(
      team %in% teams,
      !is.na(result)
    )
}

calc_sos <- function(team) {
  opponents = played_games(team)$opponent
  
  op_sos <- avg_win_pct(played_games(team)$opponent)
  
  oop_sos <- avg_win_pct(played_games(opponents)$opponent)
  
  ((2 * op_sos) + oop_sos)/3
}