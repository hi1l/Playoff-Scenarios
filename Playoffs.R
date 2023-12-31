


library(tidyverse)
library(ggrepel)
library(dplyr)
library(nflplotR)
library(scales)
library(nflreadr)
library(nflfastR)
library(purrr)



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
  conf_teams <- split(teams, teams$conf)
  for(conf in names(conf_teams)) {
    conf_df <- conf_teams[[conf]]
    conf_df <- team_sort(conf_df, team_game_data)
    conf_teams[[conf]] <- conf_df %>%
      arrange(desc(pct), wc_tiebreaker_rank) %>%
      mutate(wc_rank = (row_number()))
  }
  return(conf_teams)
}


division_standings <- function(teams, team_game_data) {
  #divisions <- split(teams, teams$div)
  x<-1
  divisions <- teams
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

get_standings <- function(teams, team_game_data) {#, split_col) {
  #split_teams <- split(teams, teams[[split_col]])
  x<-1
  for (i in names(teams)) {
    group <- teams[[i]]
    group <- team_sort(group, team_game_data)
    
    group <- arrange(group, desc(pct), wc_rank, div_rank)
    teams[[i]] <- group
  }
  return(teams)
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

get_divs <- function(teamSeasonData) {
  # Split the data into a list of dataframes for each division
  return(split(teamSeasonData, teamSeasonData$div))
}

get_non_div_winners <- function(div_standings, team_game_data) {
  in_the_hunt <- lapply(div_standings, function(df) {
    df[df$div_rank != 1, ]
  })
  in_the_hunt <- do.call(rbind, in_the_hunt)
  return(in_the_hunt)
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
  x<-0
  team_wins <- sapply(teams$team_abbr, function(team_abbr) {
    
    team_df <- team_game_data[[team_abbr]]
    
    relevant_games <- team_df[team_df$opponent %in% teams$team_abbr, ]
    
    # Check if there are no relevant games
    if (nrow(relevant_games) == 0) {
      (0)
    } else {
      # Count wins against these opponents
      (sum(relevant_games$result == "Win", na.rm = TRUE))
    }
  })
  
  # Add results to the teams dataframe
  teams$h2h_wins <- team_wins
  return(teams)
}

head2head <- function(teams, team_game_data) {
  teams <- h2h_wins(teams, team_game_data)
  max_h2h <- max(teams$h2h_wins)
  highest_teams_df <- teams[teams$h2h_wins == max_h2h, ]
  return(highest_teams_df)
}


div_record <- function(teams, team_game_data) {
  max_div <- max(teams$div_wins)
  results <- teams[teams$div_wins == max_div,]
  return(results)
}


# Function to find common opponents among a group of teams
common_opponents <- function(teams) {
  # Check if the 'past_opps' column is present in the teams data frame
  if ("past_opps" %in% names(teams)) {
    # Use Reduce and intersect to find the common elements (opponents) across all teams' 'past_opps' lists
    return(Reduce(intersect, teams$past_opps))
  } else {
    # If 'past_opps' column is not found, stop the function and display an error message
    stop("Error: 'past_opps' column not found in the teams data frame.")
  }
}




# Function to compare teams based on performance in common games
common_games <- function(teams, team_game_data) {
  # Retrieve the list of common opponents
  common_opps <- common_opponents(teams)
  
  # Check if common opponents list is empty or has less than 4 teams
  if (is.null(common_opps) || length(common_opps) < 4) {
    # Return teams as is if not enough common opponents
    return(teams)
  }
  
  # Calculate the performance record against common opponents
  team_records <- lapply(teams$team_abbr, function(team_abbr) {
    team_df <- team_game_data[[team_abbr]]
    common_games_df <- team_df[team_df$opponent %in% common_opps, ]
    
    # Calculate wins, losses, and ties
    wins <- sum(common_games_df$result == "Win", na.rm = TRUE)
    losses <- sum(common_games_df$result == "Loss", na.rm = TRUE)
    ties <- sum(common_games_df$result == "Tie", na.rm = TRUE)
    total_games <- wins + losses + ties
    
    # Compute win percentage
    record_pct <- ifelse(total_games > 0, (wins + 0.5 * ties) / total_games, NA)
    
    # Create a data frame for each team's record percentage
    data.frame(team_abbr = team_abbr, record_pct = record_pct)
  })
  
  # Combine the records into one data frame
  team_records_df <- do.call(rbind, team_records)
  
  # Find the highest record percentage
  max_record <- max(team_records_df$record_pct, na.rm = TRUE)
  
  # Select teams with the highest record percentage
  teams_with_max_record <- team_records_df[team_records_df$record_pct == max_record, ]
  
  # Return the subset of teams with the maximum record
  return(teams[teams$team_abbr %in% teams_with_max_record$team_abbr, ])
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



# Function to load NFL data
load_nfl_data <- function() {
  nflScheduleData <- nflreadr::load_schedules(2023)
  nflTeamsInfo <- load_teams()
  nflData <- list(schedule = nflScheduleData, teams = nflTeamsInfo)
  return(nflData)
}



#Function to create game data for each team
byTeamGameData <- function(nflScheduleData, nflTeamsInfo) {
  if (!("team_abbr" %in% names(nflTeamsInfo))) {
    stop("Error: 'team_abbr' column not found in team info data")
  }
  
  # Perform join operations once
  games_with_conf <- nflScheduleData %>%
    left_join(nflTeamsInfo, by = c("home_team" = "team_abbr")) %>%
    rename(home_team_conf = team_conf, home_team_div = team_division) %>%
    left_join(nflTeamsInfo, by = c("away_team" = "team_abbr")) %>%
    rename(away_team_conf = team_conf, away_team_div = team_division)
  
  # Use lapply to process each team
  team_game_data <- setNames(
    lapply(nflTeamsInfo$team_abbr, function(team_abbr) {
      create_team_df(team_abbr, games_with_conf)
    }),
    nflTeamsInfo$team_abbr
  )
  
  return(team_game_data)
}

# Adjusted function to filter and mutate the pre-joined data
create_team_df <- function(team_name, games_with_conf) {
  team_games <- games_with_conf %>%
    filter(home_team == team_name | away_team == team_name) %>%
    mutate(
      is_home_game = home_team == team_name,
      team_score = ifelse(is_home_game, home_score, away_score),
      opponent_score = ifelse(is_home_game, away_score, home_score),
      opponent = ifelse(is_home_game, away_team, home_team),
      result = case_when(
        team_score > opponent_score ~ "Win",
        team_score < opponent_score ~ "Loss",
        team_score == opponent_score ~ "Tie",
        TRUE ~ NA_character_
      ),
      conference_game = home_team_conf == away_team_conf,
      div_game = home_team_div == away_team_div
    ) %>%
    select(game_id, week, opponent, result, conference_game, div_game, gameday)
  
  return(team_games)
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

team_sort <- function(teams, team_game_data) {
  
  if (n_distinct(teams$pct) == nrow(teams)) {
    teams <- teams %>% 
      arrange(desc(pct)) %>%
      mutate(rank = row_number())
    return(teams)
  } 
  
  unique_wins <- unique(teams$pct)
  
  if (n_distinct(teams$div) != 1) {
    sort_type <- "wc"
    rank_adj <- 4
  } else {
    sort_type <- "div"
    rank_adj <- 0
  }
  rank_col <- paste(sort_type,"tiebreaker_rank",sep = "_")
  other_rank <- paste(sort_type,"rank",sep = "_")
  flag_col <- paste(sort_type,"tiebreaker_flag",sep = "_")
  
  
  repeat {
    tiebreaker_applied <- FALSE
    
    for (pct in unique_wins) {
      if (n_distinct(teams[teams$pct == pct, ]) == 1) {
        next
      } 
      
      
      tied_teams <- teams[teams$pct == pct & is.na(teams[[rank_col]]), ]
      rank = 1
      while (nrow(tied_teams)>1) {
        
        tie_winner <- tiebreaker(tied_teams, team_game_data, sort_type)
        teams[teams$team_abbr %in% tie_winner$team_abbr, rank_col] <- rank
        teams[teams$team_abbr %in% tie_winner$team_abbr, flag_col] <- tie_winner$tiebreaker_flag
        tied_teams <- teams[teams$pct == pct & is.na(teams[[rank_col]]), ] #teams dataframe includes teams not in the tie
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
  
  teams <- teams %>%
    arrange(desc(pct),rank_col) %>%
    mutate(rank = row_number())
  
  return(teams)
}

# Function to calculate team records based on a given DataFrame and filter expression
calculate_team_records <- function(team_df, filter_expr = NULL) {
  if (!is.null(filter_expr)) {
    # Apply filter expression if provided
    team_df <- team_df %>% filter(!!rlang::parse_expr(filter_expr))
  }
  
  # Calculating wins, losses, and win percentage
  wins <- sum(team_df$result == "Win", na.rm = TRUE)
  losses <- sum(team_df$result == "Loss", na.rm = TRUE)
  pct <- percent(wins / (wins + losses))
  
  return(list(wins = wins, losses = losses, pct = pct))
}

# Main function to calculate team statistics
calcTeamStats <- function(nflScheduleData, nflTeamsInfo, team_game_data) {
  # Processing each team
  teamSeasonData <- map_dfr(names(team_game_data), function(team_abbr) {
    team_df <- team_game_data[[team_abbr]]
    
    # Calculating overall, conference, and division records
    overall <- calculate_team_records(team_df)
    conf_rec <- calculate_team_records(team_df, "conference_game")
    div_rec <- calculate_team_records(team_df, "div_game")
    
    # Extracting past, future, and opponents for SoV
    past_opponents <- team_df$opponent[team_df$gameday < Sys.Date()]
    sov_opponents <- team_df$opponent[team_df$gameday < Sys.Date() & team_df$result == "Win"]
    future_opponents <- team_df$opponent[team_df$gameday >= Sys.Date()]
    
    # Calculating remaining games
    remaining_games <- sum(team_df$gameday >= Sys.Date())
    remaining_conf_games <- sum(team_df$gameday >= Sys.Date() & team_df$conference_game)
    remaining_div_games <- sum(team_df$gameday >= Sys.Date() & team_df$div_game)
    remaining_games_id <- team_df$game_id[team_df$gameday < Sys.Date()]
    
    # Creating the team summary table
    tibble(
      team_abbr = team_abbr,
      conf = nflTeamsInfo$team_conf[nflTeamsInfo$team_abbr == team_abbr],
      div = nflTeamsInfo$team_division[nflTeamsInfo$team_abbr == team_abbr],
      wins = overall$wins, losses = overall$losses, pct = overall$pct,
      conf_wins = conf_rec$wins, conf_losses = conf_rec$losses, conf_pct = conf_rec$pct,
      div_wins = div_rec$wins, div_losses = div_rec$losses, div_pct = div_rec$pct,
      #opps = opponents, 
      past_opps = list(past_opponents), future_opps = list(future_opponents), sov_opps = list(sov_opponents),
      remaining_games, remaining_conf_games, remaining_div_games, remaining_games_id = list(remaining_games_id)
    )
  })
  
  # Calculate SoS and SoV
  teamSeasonData <- calc_sos_sov(teamSeasonData, team_game_data)
  
  # Add empty tiebreaker columns
  teamSeasonData <- teamSeasonData %>%
    mutate(div_rank = NA_integer_, div_tiebreaker_rank = NA_integer_,
           div_tiebreaker_flag = NA_character_, wc_rank = NA_integer_,
           wc_tiebreaker_rank = NA_integer_, wc_tiebreaker_flag = NA_character_)
  
  return(teamSeasonData)
}

# Function to calculate SoS and SoV for each team
calc_sos_sov <- function(teamSeasonData, team_game_data) {
  # Create a summary table for wins and losses
  team_summaries <- teamSeasonData %>% 
    select(team_abbr, wins, losses) %>% 
    pivot_longer(cols = c(wins, losses), names_to = "result_type", values_to = "count")
  
  # Calculate SoS and SoV for each team
  teamSeasonData <- teamSeasonData %>% 
    mutate(
      SoS = map_dbl(past_opps, ~sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "wins", "count"], na.rm = TRUE)),
      SoV = map_dbl(sov_opps, ~{
        sov_wins <- sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "wins", "count"], na.rm = TRUE)
        sov_losses <- sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "losses", "count"], na.rm = TRUE)
        ifelse(sov_wins + sov_losses > 0, sov_wins / (sov_wins + sov_losses), 0)
      })
    )
  
  return(teamSeasonData)
}





# #main()
# # Load schedules and team data
# load_nfl_data_time <- system.time(
#   nflData <- load_nfl_data()
# )
# print(paste("load_nfl_data function took", load_nfl_data_time["elapsed"], "seconds"))
# 
# # Get game by game data for each team
# byTeamGameData_time <- system.time(
#   team_game_data <- byTeamGameData(nflData$schedule, nflData$teams)
# )
# print(paste("byTeamGameData function took", byTeamGameData_time["elapsed"], "seconds"))
# # Get record and other stats for each team
# teamSeasonData_time <- system.time(
#   teamSeasonData <- calcTeamStats(nflData$schedule, nflData$teams, team_game_data)
# )
# print(paste("teamSeasonData function took", teamSeasonData_time["elapsed"], "seconds"))
# 
# # get division standings
# #divs <- get_divs(teamSeasonData)
# divStandings_time <- system.time(
#   div_standings <- division_standings(split(teamSeasonData,teamSeasonData$div), team_game_data)
# )
# print(paste("div_Standings function took", divStandings_time["elapsed"], "seconds"))
# #div_standings <- get_standings(teamSeasonData, team_game_data, "div")
# 
# # Get playoff seeds
# playoff_time <- system.time(
#   playoff_standings <- playoff_seeds(div_standings, team_game_data)
# )
# print(paste("load_nfl_data function took", load_nfl_data_time["elapsed"], "seconds"))
# print(paste("byTeamGameData function took", byTeamGameData_time["elapsed"], "seconds"))
# print(paste("teamSeasonData function took", teamSeasonData_time["elapsed"], "seconds"))
# print(paste("div_Standings function took", divStandings_time["elapsed"], "seconds"))
# print(paste("playoff function took", playoff_time["elapsed"], "seconds"))


# Load schedules and team data
nflData <- load_nfl_data()

# Get game by game data for each team
team_game_data <- byTeamGameData(nflData$schedule, nflData$teams)

# Get record and other stats for each team
teamSeasonData <- calcTeamStats(nflData$schedule, nflData$teams, team_game_data)

# Get division standings
div_standings <- division_standings(split(teamSeasonData, teamSeasonData$div), team_game_data)

# Get playoff seeds
playoff_standings <- playoff_seeds(div_standings, team_game_data)


