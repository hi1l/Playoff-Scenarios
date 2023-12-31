#rm(list = ls())
#source("~/Playoff-Scenarios_testing/Playoffs.R")

calculate_min_max_seeds <- function(playoff_standings) {
  seeds <- data.frame(team_abbr = character(), max_seed = integer(), min_seed = integer(), conf = character(), is_tied_at_max = logical(), is_tied_at_min = logical())
  
  for (conf in names(playoff_standings)) {
    standings <- playoff_standings[[conf]]
    
    for (team in standings$team_abbr) {
      # Max potential seed
      temp_standings_max <- standings
      temp_standings_max[temp_standings_max$team_abbr == team, "wins"] <- temp_standings_max[temp_standings_max$team_abbr == team, "wins"] + temp_standings_max[temp_standings_max$team_abbr == team, "remaining_games"]
      sorted_max <- temp_standings_max %>% arrange(desc(wins), desc(pct))
      max_seed <- which(sorted_max$team_abbr == team)
      is_tie_at_max <- sum(sorted_max$wins == sorted_max$wins[max_seed]) > 1
      
      # Min potential seed
      temp_standings_min <- standings
      temp_standings_min[temp_standings_min$team_abbr != team, "wins"] <- temp_standings_min[temp_standings_min$team_abbr != team, "wins"] + temp_standings_min[temp_standings_min$team_abbr != team, "remaining_games"]
      sorted_min <- temp_standings_min %>% arrange(desc(wins), desc(pct))
      min_seed <- which(sorted_min$team_abbr == team)
      is_tie_at_min <- sum(sorted_min$wins == sorted_min$wins[min_seed]) > 1
      
      seeds <- rbind(seeds, data.frame(team_abbr = team, max_seed = max_seed, min_seed = min_seed, conf = conf, is_tied_at_max = is_tie_at_max, is_tied_at_min = is_tie_at_min))
    }
  }
  
  return(seeds)
}

# Calculate maximum and minimum potential seeds for each team in the playoff_standings
potential_seeds <- calculate_min_max_seeds(playoff_standings)
potential_seeds <- split(potential_seeds,   potential_seeds$conf)
# Print the results
print(potential_seeds)





