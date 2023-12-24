library(nflfastR)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(scales)

main <- function() {
  # Load schedules and team data
  games <- nflreadr::load_schedules(2023)
  NFLteams <- load_teams()
  
  team_dataframes <- setNames(
    lapply(NFLteams$team_abbr, function(team_abbr) create_team_df(team_abbr, games, NFLteams)),
    NFLteams$team_abbr
  )
  # Initialize a list to store the summary for each team
  team_summaries <- list()
  
  # Loop through each team and calculate the statistics
  for (team_abbr in names(team_dataframes)) {
    # Extract the team's DataFrame
    team_df <- team_dataframes[[team_abbr]]
    
    # Calculate wins, losses, and conference wins
    wins <- sum(team_df$result == "Win", na.rm = TRUE)
    losses <- sum(team_df$result == "Loss", na.rm = TRUE)
    ties <- sum(team_df$result == "Tie", na.rm = TRUE)
    # Adjust wins and losses for ties
    wins <- wins + 0.5 * ties
    losses <- losses + 0.5 * ties
    
    conf_wins <- sum(team_df$result == "Win" & team_df$is_conference_game, na.rm = TRUE)
    div_wins <- sum(team_df$result == "Win" & team_df$is_div_game, na.rm = TRUE)
    
    # Create a summary DataFrame for the team
    team_summary <- data.frame(team_abbr = team_abbr,
                               conf = NFLteams$team_conf[NFLteams$team_abbr == team_abbr],
                               div = NFLteams$team_division[NFLteams$team_abbr == team_abbr],
                               wins = wins, 
                               losses = losses,
                               pct = percent(wins / (losses + wins)),
                               conf_wins = conf_wins,
                               div_wins = div_wins)
    
    # Add the summary to the list
    team_summaries[[team_abbr]] <- team_summary
  }
  # Loop through each team in team_dataframes
  for (team_abbr in names(team_dataframes)) {
    # Extract the team's game data
    team_games <- team_dataframes[[team_abbr]]
    
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
      sos <- sos + team_summaries[[opp]]$wins
      #total <- total + 1
      # For wins, calculate SoV components
      if (result == "Win") {
        sov_w <- sov_w + team_summaries[[opp]]$wins
        sov_l <- sov_l + team_summaries[[opp]]$losses
      }
    }
    # Calculate SoV
    sov <-  sov_w / (sov_l+sov_w)
    
    # Add SoS and SoV to the corresponding team's summary in team_summaries
    team_summaries[[team_abbr]]$SoS <- sos
    team_summaries[[team_abbr]]$SoV <- sov
  }
  
  
  # Combine all summaries into a single DataFrame
  team_summaries <- do.call(rbind, team_summaries)
  
  standings <- team_summaries %>%
    group_by(div) %>%
    arrange(desc(pct), .by_group = TRUE) %>%
    ungroup()
  # Split the data into a list of dataframes for each division
  divisions_data <- split(team_summaries, team_summaries$div)
  
  # Sort each dataframe in the list by number of wins
  divisions_standings <- lapply(divisions_data, function(df) {
    df %>% 
      arrange(desc(pct))%>%
      mutate(div_rank = row_number(),  # Add division ranking
             tiebreaker_rank = NA_integer_,  # Add empty tiebreaker rank
             tiebreaker_flag = NA_character_)   # Add empty tiebreaker flag
  })
  
  for (division in names(divisions_standings)) {
    # division_df <- divisions_standings[[division]]  # Access the dataframe, not just the name
    #sorted_division_df <- division_sort(division_df, team_dataframes)  # Now pass the dataframe
    divisions_standings[[division]] <- division_sort(divisions_standings[[division]], team_dataframes) #sorted_division_df
  }
}

h2h_sweep <- function(teams, team_dataframes) {
  
  teams_df <- h2h_wins(teams, team_dataframes)
  if (max(teams_df$h2h_wins) == length(teams_df)-1) {
    # Keep only teams with the maximum number of head-to-head wins
    tm_sweep <- teams_df[teams_df$h2h_wins == max_h2h, ]
    
    return(tm_sweep)
  } else {
    return(teams)
  }
}

h2h_wins <- function(teams, team_dataframes) {
  # Initialize a vector to store the head-to-head results
  results <- numeric(nrow(teams))
  
  # Loop through each team
  for (i in 1:nrow(teams)) {
    team_abbr <- teams$team_abbr[i]
    team_df <- team_dataframes[[team_abbr]]
    
    # Filter games where the opponent is in the teams list
    relevant_games <- team_df[team_df$opponent %in% teams$team_abbr, ]
    
    # Count wins against these opponents
    h2h_wins <- sum(relevant_games$result == "Win", na.rm = TRUE)
    
    # Store the result
    results[i] <- h2h_wins
  }
  # Add results to the teams dataframe
  teams$h2h_wins <- results
  return(teams)
}

head2head <- function(teams, team_dataframes) {
  
  teams <- h2h_wins(teams, team_dataframes)
  
  
  # Find the maximum number of head-to-head wins
  max_h2h <- max(teams$h2h_wins)
  
  # Keep only teams with the maximum number of head-to-head wins
  highest_teams_df <- teams[teams$h2h_wins == max_h2h, ]
  
  return(highest_teams_df)
}

div_record <- function(teams, team_dataframes) {
  max_div <- max(teams$div_wins)
  #print(paste("max_div:", max_div))
  results <- teams[teams$div_wins == max_div,]
  #print("Teams:")
  #print(teams)
  #print("Results:")
  #print(results)
  return(results)
}

common_opponents <- function(teams, team_dataframes) {
  opponents <- list()
  for (i in 1:nrow(teams)) {
    team_abbr <- teams$team_abbr[i]
    team_df <- team_dataframes[[team_abbr]]
    opponents[[team_abbr]] <- unique(team_df$opponent)
  }
  return(Reduce(intersect, opponents))
}

common_games <- function(teams, team_dataframes) {
  common_opps <- common_opponents(teams, team_dataframes)
  
  # Evaluate performance against common opponents
  results <- data.frame(team_abbr = character(), record = double())
  for (i in 1:nrow(teams)) {
    team <- teams$team_abbr[i]
    team_df <- team_dataframes[[team_abbr]]
    relevant_games <- team_df[team_df$opponent %in% common_opps, ]
    wins <- sum(relevant_games$result == "Win", na.rm = TRUE)
    losses <- sum(relevant_games$result == "Loss", na.rm = TRUE)
    ties <- sum(relevant_games$result == "Tie", na.rm = TRUE)
    record <- (wins + .5*ties) / (losses+wins+ties)
    results <- rbind(results, data.frame(team = team, record))
  }
  max_record <- max(results$record)
  results <- results[results$record==max_record]
  cg <- teams[teams$team_abbr %in% results$team, ]
  return(cg)
}

conf_record <- function(teams, team_dataframes) {
  max_conf <- max(teams$conf_wins)
  results <- teams[teams$conf_wins == max_conf]
  return(results)
}

s_o_v <- function(teams, team_dataframes) {
  max_sov <- max(teams$SoV)
  results <- teams[teams$SoV == max_sov]
  return(results)
}

s_o_s <- function(teams, team_dataframes) {
  max_sos <- max(teams$SoS)
  results <- teams[teams$SoS == max_sos]
  return(results)
}

div_tiebreaker <- function(teams, team_dataframes) {
  divtiebreakers <- list(
    head2head,      # Compare teams based on head-to-head results
    div_record,     # Compare teams based on division records
    common_games,   # Compare teams based on performance in common games
    conf_record,    # Compare teams based on conference records
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
  )
  
  
  # Store the original number of teams
  num_teams <- nrow(teams)
  
  # Iterate through each tiebreaker
  for (i in seq_along(divtiebreakers)) {
    tiebreaker <- divtiebreakers[[i]]
    teams <- tiebreaker(teams, team_dataframes)
    
    # If the number of teams is reduced, exit the loop
    if (nrow(teams) < num_teams) {
      teams$tiebreaker_flag <- flags[i]  # Set the flag
      break
    }
  }
  
  # Check if the tie is resolved to fewer than 3 teams
  if (nrow(teams) == 1) {
    # Return the resulting teams if tie is resolved
    #teams$tiebreaker_flag <- as.character(quote(tiebreaker))
    return(teams)
  } else {
    # If still more than 1 team, restart the tiebreaking process recursively
    return(div_tiebreaker(teams, team_dataframes))
  }
}

wc_tiebreaker <- function(teams, team_dataframes) {
  # List of tiebreaker functions
  wctiebreakers <- list(
    head2head,      # Compare teams based on head-to-head results
    conf_record,    # Compare teams based on conference records
    common_games,   # Compare teams based on performance in common games
    s_o_v,          # Compare teams based on strength of victory
    s_o_s           # Compare teams based on strength of schedule
  )
  
  unique_divs <- unique(teams$div)
  
  if (length(unique_divs) == 1) {
    return(div_tiebreaker(teams, team_dataframes))
  } else {
    div_results <- list()
    for (div in unique_divs) {
      teams_in_div <- teams[teams$div == div, ]
      
      # Only apply tiebreaker if more than one team in the division
      if (nrow(teams_in_div) > 1) {
        div_results[[div]] <- div_tiebreaker(teams_in_div, team_dataframes)
      } else {
        div_results[[div]] <- teams_in_div
      }
    }
    teams <- do.call(rbind, div_results)  # Combine results from different divisions
  }
  
  num_teams <- nrow(teams)
  
  # Iterate through each tiebreaker
  for (tiebreaker in wctiebreakers) {
    if (tiebreaker == head2head && num_teams > 2) {
      teams <- h2h_sweep(teams, team_dataframes) 
    } else if (tiebreaker == common_games && length(common_opponents(teams, team_dataframes)) < 4) {
      next  # Skip to the next tiebreaker
    } else {
      teams <- tiebreaker(teams, team_dataframes)  # Apply the tiebreaker function
    }
    
    # If the number of teams is reduced, exit the loop
    if (nrow(teams) < num_teams) {
      break
    }
  }
  
  # Check if the tie is resolved to 1 team
  if (nrow(teams) == 1) {
    # Return the resulting teams if tie is resolved
    return(teams)
  } else if (length(unique(teams$div)) == 1) {
    return(div_tiebreaker(teams, team_dataframes))
  } else {
    # If still more than 1 team, restart the tiebreaking process recursively
    return(wc_tiebreaker(teams, team_dataframes))
  }
}

create_team_df <- function(team_name, games, NFLteams) {
  # Joining the teams DataFrame with the games DataFrame to get the team and opponent conference
  games_with_conf <- games %>%
    left_join(NFLteams, by = c("home_team" = "team_abbr")) %>%
    rename(home_team_conf = team_conf, home_team_div = team_division) %>%
    left_join(NFLteams, by = c("away_team" = "team_abbr")) %>%
    rename(away_team_conf = team_conf, away_team_div = team_division)
  
  
  # Filter and mutate to create the desired DataFrame
  team_games <- games_with_conf %>%
    filter(home_team == team_name | away_team == team_name) %>%
    mutate(
      is_home_game = home_team == team_name,
      team_score = ifelse(is_home_game, home_score, away_score),
      opponent_score = ifelse(is_home_game, away_score, home_score),
      opponent = ifelse(is_home_game, away_team, home_team),
      result = ifelse(team_score > opponent_score, "Win",
                      ifelse(team_score<opponent_score,"Loss",
                             ifelse(team_score==opponent_score,"Tie",NA))),
      team_conf = ifelse(is_home_game, home_team_conf, away_team_conf),
      opponent_conf = ifelse(is_home_game, away_team_conf, home_team_conf),
      is_conference_game = team_conf == opponent_conf,
      team_div = ifelse(is_home_game, home_team_div, away_team_div),
      opponent_div = ifelse(is_home_game, away_team_div, home_team_div),
      is_div_game = team_div == opponent_div
    ) %>%
    select(week, opponent, result, is_conference_game, is_div_game)
  
  return(team_games)
}

division_sort <- function(division, team_dataframes) {
  unique_wins <- unique(division$wins)
  if (length(unique_wins) == length(division$wins)) {
    return(division)
  }
  repeat {
    tiebreaker_applied <- FALSE
  
    for (wins in unique_wins) {
      tied_teams <- division[division$wins == wins & is.na(division$tiebreaker_rank), ]
      rank = 1
      while (nrow(tied_teams)>1) {
      
        winning_team <- div_tiebreaker(tied_teams, team_dataframes)
        
        # Update the tiebreaker_rank for the winning team
        #current_max_rank <- max(division$tiebreaker_rank, na.rm = TRUE)
        #next_rank <- ifelse(is.na(current_max_rank), 1, current_max_rank + 1)
        division[division$team_abbr %in% winning_team$team_abbr, "tiebreaker_rank"] <- rank
        division[division$team_abbr %in% winning_team$team_abbr, "tiebreaker_flag"] <- winning_team$tiebreaker_flag
        tied_teams <- division[division$wins == wins & is.na(division$tiebreaker_rank), ]
        tiebreaker_applied <- TRUE
        rank = rank+1
        if (nrow(tied_teams) ==1) {
          division[division$team_abbr == tied_teams$team_abbr, "tiebreaker_rank"] <- rank
        }
      }
    }
    
    division <- division %>%
      arrange(desc(pct), tiebreaker_rank, .by_group = TRUE) %>%
      mutate(div_rank = row_number())
    
    if (!tiebreaker_applied) {
      break
    }
  }
  
  return(division)
}

main()




