files <- list.files(path="./data/", pattern="*.html", full.names=FALSE, recursive=FALSE)

rm(standings_raw)
rm(games_raw)

for (i in 1:length(files)) {
  filename = files[[i]]
  
  division_standings = as.data.frame(read_html(paste0("./data/", filename)) %>% 
                         html_node('#ContentPlaceHolder1_StandingsResultsControl_StandingsPanel .rgMasterTable') %>% 
                         html_table())
  
  division_standings$division = str_extract(filename, "([a-z]+)")

  if (exists("standings_raw")) {
    standings_raw = rbind(standings_raw, division_standings)
  } else {
    standings_raw = division_standings
  }
  
  division_games = as.data.frame(read_html(paste0("./data/", filename)) %>% 
                              html_node('#ContentPlaceHolder1_StandingsResultsControl_SchedulePanel .rgMasterTable') %>%
                              html_table(fill=T)) %>%
                              filter(str_detect(Date, "^(Mon|Tue|Wed|Thu|Fri|Sat|Sun)")) %>%
                              mutate(
                                Home = str_replace_all(Home, "\\s+", " "),
                                Away = str_replace_all(Away, "\\s+", " "),
                              ) %>% 
                              extract(Home, c('home_team', 'home_score'), "^(.+)\\s(\\d+)", remove = F, convert =T) %>% 
                              extract(Away, c('away_team', 'away_score'), "^(.+)\\s(\\d+)", remove = F, convert =T)

  if (exists("games_raw")) {
    games_raw = rbind(games_raw, division_games)
  } else {
    games_raw = division_games
  }
  
}