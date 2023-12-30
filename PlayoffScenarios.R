rm(list = ls())

library(tidyverse)
library(ggrepel)
library(dplyr)
library(nflplotR)
library(scales)
library(nflreadr)
library(nflfastR)

source("~/Playoff-Scenarios_testing/get_season_data.R")
source("~/Playoff-Scenarios_testing/preprocess_game_data.R")
source("~/Playoff-Scenarios_testing/tiebreakers.R")

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
  
  if (n_distinct(teams$record) == nrow(teams)) {
    teams <- teams %>% 
      arrange(desc(pct)) %>%
      mutate(rank = row_number())
    return(teams)
  } 

  unique_wins <- unique(teams$record)
  
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
  
    for (record in unique_wins) {
      if (n_distinct(teams[teams$record == record, ]) == 1) {
        next
      } 
      
      
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
  
  teams <- teams %>%
    arrange(desc(pct),rank_col) %>%
    mutate(rank = row_number())
  
  return(teams)
}


library(tidyverse)
library(ggrepel)

library(dplyr)
library(purrr)


calc_sos_sov2 <- function(nflScheduleData, nflTeamsInfo, teamSeasonData, team_game_data) {
  teamSeasonData <- teamSeasonData %>%
    rowwise() %>%
    mutate(
      SoS = sum(sapply(unlist(past_opps), function(opp) {
        win_count <- teamSeasonData %>% filter(team_abbr == opp) %>% pull(wins)
        if (length(win_count) > 0) win_count else 0
      })),
      
      SoV = {
        sov_wins <- sum(sapply(unlist(sov_opps), function(opp) {
          win_count <- teamSeasonData %>% filter(team_abbr == opp) %>% pull(wins)
          if (length(win_count) > 0) win_count else 0
        }))
        
        sov_losses <- sum(sapply(unlist(sov_opps), function(opp) {
          loss_count <- teamSeasonData %>% filter(team_abbr == opp) %>% pull(losses)
          if (length(loss_count) > 0) loss_count else 0
        }))
        
        ifelse(sov_wins + sov_losses > 0, sov_wins / (sov_wins + sov_losses), 0)
      }
    ) %>%
    ungroup()
  
  return(teamSeasonData)
}



library(rlang)
# calculate_team_records <- function(team_df, filter_expr = NULL) {
#   if (!is.null(filter_expr)) {
#     filter_expr <- rlang::parse_expr(filter_expr)
#     team_df <- team_df %>% filter(!!filter_expr)
#   }
#   
#   wins <- sum(team_df$result == "Win", na.rm = TRUE)
#   losses <- sum(team_df$result == "Loss", na.rm = TRUE)
#   pct <- percent(wins / (wins + losses))
#   
#   return(list(wins = wins, losses = losses, pct = pct))
# }
# 
# calcTeamStats <- function(nflScheduleData, nflTeamsInfo, team_game_data) {
#   
#   teamSeasonData <- map(names(team_game_data), function(team_abbr) {
#     team_df <- team_game_data[[team_abbr]]
#     
#     # overall <- calculate_win_loss_ties(team_df)
#     # conf_rec <- calculate_win_loss_ties(team_df %>% filter(conference_game))
#     # div_rec <- calculate_win_loss_ties(team_df %>% filter(div_game))
#     
#     overall <- calculate_team_records(team_df)
#     conf_rec <- calculate_team_records(team_df, "conference_game")
#     div_rec <- calculate_team_records(team_df, "div_game")
#     
#     past_opponents <- (team_df$opponent[team_df$gameday < Sys.Date()])
#     sov_opponents <- (team_df$opponent[team_df$gameday < Sys.Date() & team_df$result == "Win"])
#     #print(sov_opponents)
#     future_opponents <- (team_df$opponent[team_df$gameday >= Sys.Date()])
#     opponents <- (team_df$opponent)  
#     
#     # Count of future games
#     remaining_games <- sum(team_df$gameday >= Sys.Date())
#     remaining_conf_games <- sum(team_df$gameday >= Sys.Date() & team_df$conference_game)
#     remaining_div_games <- sum(team_df$gameday >= Sys.Date() & team_df$div_game)
#     
#     team_summary <- tibble(
#       team_abbr = team_abbr,
#       conf = nflTeamsInfo$team_conf[nflTeamsInfo$team_abbr == team_abbr],
#       div = nflTeamsInfo$team_division[nflTeamsInfo$team_abbr == team_abbr],
#       wins = overall[["wins"]], 
#       losses = overall[["losses"]],
#       pct = percent(overall[["wins"]] / (overall[["wins"]] + overall[["losses"]])),
#       conf_wins = conf_rec[["wins"]],
#       conf_losses = conf_rec[["losses"]],
#       conf_pct = percent(conf_rec[["wins"]] / (conf_rec[["wins"]] + conf_rec[["losses"]])),
#       div_wins = div_rec[["wins"]],
#       div_losses = div_rec[["losses"]],
#       div_pct = percent(div_rec[["wins"]] / (div_rec[["wins"]] + div_rec[["losses"]])),
#       opps = list(opponents),
#       past_opps = list(past_opponents),
#       future_opps = list(future_opponents),
#       sov_opps = list(sov_opponents),
#       remaining_games = remaining_games,
#       remaining_conf_games = remaining_conf_games,
#       remaining_div_games = remaining_div_games
#     )
#     x<-0
#     return(team_summary)
#   })
#   teamSeasonData <- setNames(teamSeasonData, names(team_game_data))
#   teamSeasonData <- bind_rows(teamSeasonData)  # Combine all team summaries into a single data frame
#   #return(teamSeasonData)
# 
# 
# 
#   
#   # teamSeasonData <- calc_sos_sov(nflScheduleData, nflTeamsInfo, (teamSeasonData), team_game_data)
#   teamSeasonData <- calc_sos_sov( (teamSeasonData), team_game_data)
#   # Combine all summaries into a single DataFrame
#   teamSeasonData <- teamSeasonData %>%
#     mutate(div_rank = NA_integer_,  # Add division ranking
#            div_tiebreaker_rank = NA_integer_,  # Add empty tiebreaker rank
#            div_tiebreaker_flag = NA_character_,
#            wc_rank = NA_integer_,  # Add division ranking
#            wc_tiebreaker_rank = NA_integer_,  # Add empty tiebreaker rank
#            wc_tiebreaker_flag = NA_character_)   # Add empty tiebreaker flag)   # Add empty tiebreaker flag
#   
#   return(teamSeasonData)
# }
# 
# calc_sos_sov <- function(teamSeasonData, team_game_data) {
#   # Create a summary table for wins and losses
#   team_summaries <- teamSeasonData %>% 
#     select(team_abbr, wins, losses) %>% 
#     pivot_longer(cols = c(wins, losses), names_to = "result_type", values_to = "count")
#   
#   # Calculate SoS and SoV for each team
#   teamSeasonData <- teamSeasonData %>% 
#     mutate(
#       SoS = map_dbl(past_opps, ~sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "wins", "count"], na.rm = TRUE)),
#       SoV = map_dbl(sov_opps, ~{
#         sov_wins <- sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "wins", "count"], na.rm = TRUE)
#         sov_losses <- sum(team_summaries[team_summaries$team_abbr %in% .x & team_summaries$result_type == "losses", "count"], na.rm = TRUE)
#         ifelse(sov_wins + sov_losses > 0, sov_wins / (sov_wins + sov_losses), 0)
#       })
#     )
#   
#   return(teamSeasonData)
# }

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
      remaining_games, remaining_conf_games, remaining_div_games
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



#main()
# Load schedules and team data
load_nfl_data_time <- system.time(
  nflData <- load_nfl_data()
)
print(paste("load_nfl_data function took", load_nfl_data_time["elapsed"], "seconds"))

# Get game by game data for each team
byTeamGameData_time <- system.time(
  team_game_data <- byTeamGameData(nflData$schedule, nflData$teams)
)
print(paste("byTeamGameData function took", byTeamGameData_time["elapsed"], "seconds"))
# Get record and other stats for each team
teamSeasonData_time <- system.time(
  teamSeasonData <- calcTeamStats(nflData$schedule, nflData$teams, team_game_data)
)
print(paste("teamSeasonData function took", teamSeasonData_time["elapsed"], "seconds"))

# get division standings
#divs <- get_divs(teamSeasonData)
divStandings_time <- system.time(
  div_standings <- division_standings(split(teamSeasonData,teamSeasonData$div), team_game_data)
)
print(paste("div_Standings function took", divStandings_time["elapsed"], "seconds"))
#div_standings <- get_standings(teamSeasonData, team_game_data, "div")

# Get playoff seeds
playoff_time <- system.time(
  playoff_standings <- playoff_seeds(div_standings, team_game_data)
)
print(paste("load_nfl_data function took", load_nfl_data_time["elapsed"], "seconds"))
print(paste("byTeamGameData function took", byTeamGameData_time["elapsed"], "seconds"))
print(paste("teamSeasonData function took", teamSeasonData_time["elapsed"], "seconds"))
print(paste("div_Standings function took", divStandings_time["elapsed"], "seconds"))
print(paste("playoff function took", playoff_time["elapsed"], "seconds"))


