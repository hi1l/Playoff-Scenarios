library(tidyverse)
library(ggrepel)

calc_sos_sov <- function(nflScheduleData, nflTeamsInfo, teamSeasonData, team_game_data) {
  # Loop through each team in team_game_data
  for (team_abbr in names(teamSeasonData)) {
    # Extract the team's game data
    team_games <- team_game_data[[team_abbr]]
    
    # Initialize variables for SoS and SoV
    sos <- 0
    sov <- 0
    sov_w <- 0
    sov_l <- 0
    
    # Loop through each game of the team
    for (i in 1:nrow(team_games)) {
      # Retrieve opponent and result for the current game
      opp <- team_games$opponent[i]
      result <- team_games$result[i]
      
      # Check if result is NA and skip to the next iteration if true
      if (is.na(result)) {
        next
      }
      
      # Add the opponent's total wins to SoS
      sos <- sos + teamSeasonData[[opp]]$wins
      #total <- total + 1
      # For wins, calculate SoV components
      if (result == "Win") {
        sov_w <- sov_w + teamSeasonData[[opp]]$wins
        sov_l <- sov_l + teamSeasonData[[opp]]$losses
      }
    }
    # Calculate SoV
    sov <-  sov_w / (sov_l+sov_w)
    
    # Add SoS and SoV to the corresponding team's summary in teamSeasonData
    teamSeasonData[[team_abbr]]$SoS <- sos
    teamSeasonData[[team_abbr]]$SoV <- sov
  }
  return(teamSeasonData)
}
calcTeamStats <- function(nflScheduleData, nflTeamsInfo, team_game_data) {
  
  # Initialize a list to store the summary for each team
  teamSeasonData <- list()
  
  # Loop through each team and calculate the statistics
  for (team_abbr in names(team_game_data)) {
    # Extract the team's DataFrame
    team_df <- team_game_data[[team_abbr]]
    
    # Calculate wins, losses, and conference wins
    wins <- sum(team_df$result == "Win", na.rm = TRUE)
    losses <- sum(team_df$result == "Loss", na.rm = TRUE)
    ties <- sum(team_df$result == "Tie", na.rm = TRUE)
    # Adjust wins and losses for ties
    wins <- wins + 0.5 * ties
    losses <- losses + 0.5 * ties
    
    conf_wins <- sum(team_df$result == "Win" & team_df$conference_game, na.rm = TRUE)
    div_wins <- sum(team_df$result == "Win" & team_df$div_game, na.rm = TRUE)
    conf_losses <- sum(team_df$result == "Loss" & team_df$conference_game, na.rm = TRUE)
    div_losses <- sum(team_df$result == "Loss" & team_df$div_game, na.rm = TRUE)
    conf_ties <- sum(team_df$result == "Tie" & team_df$conference_game, na.rm = TRUE)
    div_ties <- sum(team_df$result == "Tie" & team_df$div_game, na.rm = TRUE)
    
    conf_wins <- conf_wins + 0.5 * conf_ties
    conf_losses <- conf_losses + 0.5 * conf_ties
    conf_wins <- conf_wins + 0.5 * conf_ties
    conf_losses <- conf_losses + 0.5 * conf_ties
    
    games_left <- sum(team_df$gameday > Sys.Date())
    div_games_left <- sum(team_df$gameday > Sys.Date() & team_df$div_game)
    conf_games_left <- sum(team_df$gameday > Sys.Date() & team_df$conference_game)
    
    # Create a summary DataFrame for the team
    team_summary <- data.frame(team_abbr = team_abbr,
                               conf = nflTeamsInfo$team_conf[nflTeamsInfo$team_abbr == team_abbr],
                               div = nflTeamsInfo$team_division[nflTeamsInfo$team_abbr == team_abbr],
                               wins = wins, 
                               losses = losses,
                               pct = percent(wins / (losses + wins)),
                               record = paste(wins, losses, sep = "-"),
                               conf_wins = conf_wins,
                               conf_pct = percent(conf_wins / (conf_wins+conf_losses)),
                               div_wins = div_wins,
                               games_left = games_left,
                               div_games_left = div_games_left,
                               conf_games_left = conf_games_left)
    
    # Add the summary to the list
    teamSeasonData[[team_abbr]] <- team_summary
  }
  
  teamSeasonData <- calc_sos_sov(nflScheduleData, nflTeamsInfo, teamSeasonData, team_game_data)
  
  # Combine all summaries into a single DataFrame
  teamSeasonData <- do.call(rbind, teamSeasonData)
  
  teamSeasonData <- teamSeasonData %>%
    mutate(div_rank = NA_integer_,  # Add division ranking
           div_tiebreaker_rank = NA_integer_,  # Add empty tiebreaker rank
           div_tiebreaker_flag = NA_character_,
           wc_rank = NA_integer_,  # Add division ranking
           wc_tiebreaker_rank = NA_integer_,  # Add empty tiebreaker rank
           wc_tiebreaker_flag = NA_character_)   # Add empty tiebreaker flag)   # Add empty tiebreaker flag
  
  return(teamSeasonData)
}
