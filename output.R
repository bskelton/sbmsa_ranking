file = 'PeeWeeOutput.xlsx'
wb <- createWorkbook()

addWorksheet(wb = wb, sheetName = "Standings", gridLines = FALSE)
writeData(wb = wb, sheet = 1, x = standings)

addWorksheet(wb = wb, sheetName = "Game Log", gridLines = FALSE)
writeData(wb = wb, sheet = 2, x = team_game_log)

saveWorkbook(wb, file, overwrite = TRUE)