library(tidyverse)
library(ggrepel)

library(nflplotR)
library(scales)

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






#main()
# Load schedules and team data
nflData <- load_nfl_data()
# Get game by game data for each team
team_game_data <- byTeamGameData(nflData$schedule, nflData$teams)
# Get record and other stats for each team
teamSeasonData <- calcTeamStats(nflData$schedule, nflData$teams, team_game_data)
# get division standings
#divs <- get_divs(teamSeasonData)
div_standings <- division_standings(split(teamSeasonData,teamSeasonData$div), team_game_data)
#div_standings <- get_standings(teamSeasonData, team_game_data, "div")

# Get playoff seeds
playoff_standings <- playoff_seeds(div_standings, team_game_data)


