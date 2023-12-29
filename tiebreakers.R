library(tidyverse)
library(ggrepel)

h2h_sweep <- function(teams, team_game_data) {
  
  teams_df <- h2h_wins(teams, team_game_data)
  if (max(teams_df$h2h_wins) == length(teams_df)-1) {
    # Keep only teams with the maximum number of head-to-head wins
    tm_sweep <- teams_df[teams_df$h2h_wins == max_h2h, ]
    
    return(tm_sweep)
  } else {
    return(teams)
  }
}

h2h_wins <- function(teams, team_game_data) {
  # Initialize a vector to store the head-to-head results
  results <- numeric(nrow(teams))
  
  # Loop through each team
  for (i in 1:nrow(teams)) {
    team_abbr <- teams$team_abbr[i]
    team_df <- team_game_data[[team_abbr]]
    
    # Filter games where the opponent is in the teams list
    relevant_games <- team_df[team_df$opponent %in% teams$team_abbr, ]
    
    # Count wins against these opponents
    h2h_wins <- sum(relevant_games$result == "Win", na.rm = TRUE)
    h2h_losses <- sum(relevant_games$result == "Loss", na.rm = TRUE)
    h2h_ties <- sum(relevant_games$result == "Tie", na.rm = TRUE)
    h2h_pct <- percent((h2h_wins + 0.5 * h2h_ties) / (h2h_wins + h2h_losses + h2h_ties))
    
    # Store the result
    results[i] <- h2h_wins
  }
  # Add results to the teams dataframe
  teams$h2h_wins <- results
  return(teams)
}

head2head <- function(teams, team_game_data) {
  
  teams <- h2h_wins(teams, team_game_data)
  
  
  # Find the maximum number of head-to-head wins
  max_h2h <- max(teams$h2h_wins)
  
  # Keep only teams with the maximum number of head-to-head wins
  highest_teams_df <- teams[teams$h2h_wins == max_h2h, ]
  
  return(highest_teams_df)
}

div_record <- function(teams, team_game_data) {
  max_div <- max(teams$div_wins)
  results <- teams[teams$div_wins == max_div,]
  return(results)
}

common_opponents <- function(teams, team_game_data) {
  opponents <- list()
  for (i in 1:nrow(teams)) {
    team_abbr <- teams$team_abbr[i]
    team_df <- team_game_data[[team_abbr]]
    team_df <- team_df[!is.na(team_df$result), ]
    opponents[[team_abbr]] <- unique(team_df$opponent)
  }
  return(Reduce(intersect, opponents))
}

common_games <- function(teams, team_game_data) {
  
  common_opps <- common_opponents(teams, team_game_data)
  if (n_distinct(teams$div) != 1) {
    if (length(common_opps) < 4) {
      return(teams)
    }
  }
  # Evaluate performance against common opponents
  results <- data.frame(team = character(), record = double())
  tmdf = teams
  for (i in 1:nrow(teams)) {
    team <- teams$team_abbr[i]
    team_df <- team_game_data[[team]]
    relevant_games <- team_df[team_df$opponent %in% common_opps, ]
    wins <- sum(relevant_games$result == "Win", na.rm = TRUE)
    losses <- sum(relevant_games$result == "Loss", na.rm = TRUE)
    ties <- sum(relevant_games$result == "Tie", na.rm = TRUE)
    record <- (wins + .5*ties) / (losses+wins+ties)
    #print(team)
    #print(record)
    tmdf$cg_rec[i] <- record
    #print(tmdf[i])
    #results[i] <-  data.frame( team, record))
  }
  #max_record <- max(results$record)
  max_record <- max(tmdf$cg_rec)
  tmdf <- tmdf[tmdf$cg_rec==max_record, ]
  # cg <- teams[teams$team_abbr %in% tmdf$team, ]
  return(tmdf)
}

conf_record <- function(teams, team_game_data) {
  max_conf <- max(teams$conf_pct)
  results <- teams[teams$conf_pct == max_conf, ]
  return(results)
}

s_o_v <- function(teams, team_game_data) {
  max_sov <- max(teams$SoV)
  results <- teams[teams$SoV == max_sov, ]
  return(results)
}

s_o_s <- function(teams, team_game_data) {
  max_sos <- max(teams$SoS)
  results <- teams[teams$SoS == max_sos, ]
  return(results)
}

div_check <- function(teams, team_game_data) {
  #print(teams)
  if (n_distinct(teams$div) == length(teams$div)) {
    return(teams)
  } else {
    
    
    div_split <- split(teams, teams$div)
    top_teams <- list()
    for (div in names(div_split)) {
      ranked <- div_split[[div]] %>%
        arrange((div_rank))
      #print(ranked[1,])
      top_team <- ranked[1,]
      #print(top_teams[[div]])
      
      
      top_teams <- rbind(top_teams, top_team)
      #print(top_teams)
    }
    return(teams)
  }
}

get_tiebreakers <- function(sort_type) {
  if (sort_type == "div") {
    tiebreakers <- list(
      head2head,      # Compare teams based on head-to-head results
      div_record,     # Flag for division records
      conf_record,    # Compare teams based on conference records
      common_games,   # Compare teams based on performance in common games
      s_o_v,          # Compare teams based on strength of victory
      s_o_s           # Compare teams based on strength of schedule
    )
    flags <- c(
      "head2head",      # Flag for head-to-head results
      "div_record",     # Flag for division records
      "common_games",   # Flag for common games
      "conf_record",    # Flag for conference records
      "s_o_v",          # Flag for strength of victory
      "s_o_s"           # Flag for strength of schedule
    )} else {
      tiebreakers <- list(
        div_check,
        head2head,      # Compare teams based on head-to-head results
        conf_record,    # Compare teams based on conference records
        common_games,   # Compare teams based on performance in common games
        s_o_v,          # Compare teams based on strength of victory
        s_o_s           # Compare teams based on strength of schedule
      )
      flags <- c(
        "div_check",
        "head2head",      # Flag for head-to-head results
        "conf_record",    # Flag for conference records
        "common_games",   # Flag for common games
        "s_o_v",          # Flag for strength of victory
        "s_o_s"           # Flag for strength of schedule
      )}
  
  return(list(tiebreakers = tiebreakers, flags = flags))
}

tiebreaker <- function(teams, team_game_data, sort_type) {
  tiebreaker_order <- get_tiebreakers(sort_type)
  
  unique_divs <- unique(teams$div)
  num_teams <- nrow(teams)
  
  for (i in seq_along(tiebreaker_order$tiebreakers)) {
    tiebreak <- tiebreaker_order$tiebreakers[[i]]
    # if (sort_type == "wc") {
    #   x <- 4
    # }
    teams <- tiebreak(teams, team_game_data)
    
    # If the number of teams is reduced, exit the loop
    if (nrow(teams) < num_teams) {
      teams$tiebreaker_flag <- tiebreaker_order$flags[i]  # Set the flag
      break
    }
  }
  
  # Check if the tie is resolved to 1 team
  if (nrow(teams) == 1) {
    # Return the resulting teams if tie is resolved
    return(teams)
  } else {
    # If still more than 1 team, restart the tiebreaking process recursively
    return(tiebreaker(teams, team_game_data, sort_type))
  }
}