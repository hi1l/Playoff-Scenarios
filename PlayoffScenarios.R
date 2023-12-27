library(nflfastR)
library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(scales)

playoff_seeds <- function(div_standings, team_game_data) {
  div_winners <- division_winners(div_standings, team_game_data)
  conf_div_winners <- conf_seed(div_winners)

  in_the_hunt <- get_non_div_winners(div_standings, team_game_data)
  wc_seeds <- wc_seed(in_the_hunt)
  final_conf_standings <- final_standings(conf_div_winners, wc_seeds)

  
  return(final_conf_standings)
}



division_winners <- function(div_standings, team_game_data) {
  div_winners <- lapply(div_standings, function(df) {
    df[df$div_rank == 1, ]
  })
  div_winners <- do.call(rbind, div_winners)
  return(div_winners)
}

conf_seed <- function(teams) {
  conf_teams <- split_by_conf(teams)
  #print(conf_teams)
  for(conf in names(conf_teams)) {
    conf_df <- conf_teams[[conf]]
    conf_df <- team_sort(conf_df, team_game_data)
    conf_teams[[conf]] <- conf_df %>%
      arrange(desc(pct), wc_tiebreaker_rank) %>%
      mutate(wc_rank = (row_number()))
  }
  return(conf_teams)
}

wc_seed <- function(teams) {
  ranked <- conf_seed(teams)
  for (conf in names(ranked)) {
    conf_df <- ranked[[conf]]
    ranked[[conf]] <- ranked[[conf]] %>% mutate(wc_rank = (wc_rank + n_distinct(conf_df$div)))
  }
  return(ranked)
}

final_standings <- function(conf_div_winners, wc_seeds) {
  standings <- list()
  
  for(conf in names(conf_div_winners)) {
    
    combined <- rbind(conf_div_winners[[conf]], wc_seeds[[conf]])
    
    standings[[conf]] <- arrange(combined, wc_rank)
  }
  
  return(standings)
}




get_non_div_winners <- function(div_standings, team_game_data) {
  in_the_hunt <- lapply(div_standings, function(df) {
    df[df$div_rank != 1, ]
  })
 in_the_hunt <- do.call(rbind, in_the_hunt)
 return(in_the_hunt)
}

split_by_conf <- function(teams) {
  afc_teams <- teams[teams$conf == "AFC",]
  nfc_teams <- teams[teams$conf == "NFC",]
  return(list(AFC = afc_teams, NFC = nfc_teams))
}

division_standings <- function(divisions, team_game_data) {
  
  for (division in names(divisions)) {
    div <- divisions[[division]]
    div <- team_sort(div, team_game_data) #sorted_division_df
    
    div <- div %>%
      arrange(desc(pct), div_tiebreaker_rank, .by_group = TRUE) %>%
      mutate(div_rank = row_number())
    divisions[[division]] <- div
  }
  
  return(divisions)
}



load_nfl_data <- function() {
  nflScheduleData <- nflreadr::load_schedules(2023)
  nflTeamsInfo <- load_teams()
  nflDatas <- list(schedule = nflScheduleData, teams = nflTeamsInfo)
  return(nflDatas)
}
byTeamGameData <- function(nflScheduleData, nflTeamsInfo) {
  team_game_data <- setNames(
    lapply(nflTeamsInfo$team_abbr, function(team_abbr) create_team_df(team_abbr, nflScheduleData, nflTeamsInfo)),
    nflTeamsInfo$team_abbr
  )
  return(team_game_data)
}
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

div_tiebreaker <- function(teams, team_game_data) {
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
  
  teams <- iterate_tiebreakers(teams, divtiebreakers, team_game_data, flags)
  #teams <- remaining_teams$teams
  #teams$div_tiebreaker_flag <- remaining_teams$flag
  
  # Check if the tie is resolved to fewer than 3 teams
  if (nrow(teams) == 1) {
    # Return the resulting teams if tie is resolved
    return(teams)
  } else {
    # If still more than 1 team, restart the tiebreaking process recursively
    return(div_tiebreaker(teams, team_game_data))
  }
}

iterate_tiebreakers <- function(teams, tiebreakers, team_game_data, flags) {
  # Store the original number of teams
  num_teams <- nrow(teams)
  
  # Iterate through each tiebreaker
  for (i in seq_along(tiebreakers)) {
    tiebreaker <- tiebreakers[[i]]
    teams <- tiebreaker(teams, team_game_data)
    
    # If the number of teams is reduced, exit the loop
    if (nrow(teams) < num_teams) {
      teams$tiebreaker_flag <- flags[i]  # Set the flag
      #return(list(teams = teams, flag = flags[i]))
      return(teams)
    }
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
}}

team_sort <- function(teams, team_game_data) {
  
  if (n_distinct(teams$record) == nrow(teams)) {
    teams <- teams %>% arrange(desc(pct))
    return(teams)
  } 

  unique_wins <- unique(teams$record)
  repeat {
    tiebreaker_applied <- FALSE
  
    for (record in unique_wins) {
      if (n_distinct(teams[teams$record == record, ]) == 1) {
        next
      } else if (n_distinct(teams$div) != 1) {
        sort_type <- "wc"
      } else {
        sort_type <- "div"
      }
      rank_col <- paste(sort_type,"tiebreaker_rank",sep = "_")
      flag_col <- paste(sort_type,"tiebreaker_flag",sep = "_")
      tied_teams <- teams[teams$record == record & is.na(teams[[rank_col]]), ]
      
      rank = 1
      while (nrow(tied_teams)>1) {
      
        tie_winner <- tiebreaker(tied_teams, team_game_data, sort_type)
        teams[teams$team_abbr %in% tie_winner$team_abbr, rank_col] <- rank
        teams[teams$team_abbr %in% tie_winner$team_abbr, flag_col] <- tie_winner$tiebreaker_flag
        tied_teams <- teams[teams$record == record & is.na(teams[[rank_col]]), ] #teams dataframe includes teams not in the tie
        tiebreaker_applied <- TRUE
        rank = rank+1
        if (nrow(tied_teams) ==1) {
          teams[teams$team_abbr == tied_teams$team_abbr, rank_col] <- rank
        }
      }
    }
    if (!tiebreaker_applied) {
      break
    }
  }
  
  return(teams)
}
create_team_df <- function(team_name, nflScheduleData, nflTeamsInfo) {
  # Joining the teams DataFrame with the nflScheduleData DataFrame to get the team and opponent conference
  games_with_conf <- nflScheduleData %>%
    left_join(nflTeamsInfo, by = c("home_team" = "team_abbr")) %>%
    rename(home_team_conf = team_conf, home_team_div = team_division) %>%
    left_join(nflTeamsInfo, by = c("away_team" = "team_abbr")) %>%
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
      conference_game = team_conf == opponent_conf,
    ) %>%
    select(week, opponent, result, conference_game, div_game, gameday)
  
  return(team_games)
}

get_divs <- function(teamSeasonData) {
  # Split the data into a list of dataframes for each division
  return(split(teamSeasonData, teamSeasonData$div))
}



#main()
# Load schedules and team data
nflData <- load_nfl_data()
# Get game by game data for each team
team_game_data <- byTeamGameData(nflData$schedule, nflData$teams)
# Get record and other stats for each team
teamSeasonData <- calcTeamStats(nflData$schedule, nflData$teams, team_game_data)
# get division standings
divs <- get_divs(teamSeasonData)
div_standings <- division_standings(divs, team_game_data)
# Get playoff seeds
playoff_standings <- playoff_seeds(div_standings, team_game_data)


