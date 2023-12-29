# library(nflreadr)
# library(nflfastR)
# 
# load_nfl_data <- function() {
#   nflScheduleData <- nflreadr::load_schedules(2023)
#   nflTeamsInfo <- load_teams()
#   nflDatas <- list(schedule = nflScheduleData, teams = nflTeamsInfo)
#   return(nflDatas)
# }
# 
# byTeamGameData <- function(nflScheduleData, nflTeamsInfo) {
#   team_game_data <- setNames(
#     lapply(nflTeamsInfo$team_abbr, function(team_abbr) create_team_df(team_abbr, nflScheduleData, nflTeamsInfo)),
#     nflTeamsInfo$team_abbr
#   )
#   return(team_game_data)
# }
# create_team_df <- function(team_name, nflScheduleData, nflTeamsInfo) {
#   # Joining the teams DataFrame with the nflScheduleData DataFrame to get the team and opponent conference
#   games_with_conf <- nflScheduleData %>%
#     left_join(nflTeamsInfo, by = c("home_team" = "team_abbr")) %>%
#     rename(home_team_conf = team_conf, home_team_div = team_division) %>%
#     left_join(nflTeamsInfo, by = c("away_team" = "team_abbr")) %>%
#     rename(away_team_conf = team_conf, away_team_div = team_division)
#   
#   # Filter and mutate to create the desired DataFrame
#   team_games <- games_with_conf %>%
#     filter(home_team == team_name | away_team == team_name) %>%
#     mutate(
#       is_home_game = home_team == team_name,
#       team_score = ifelse(is_home_game, home_score, away_score),
#       opponent_score = ifelse(is_home_game, away_score, home_score),
#       opponent = ifelse(is_home_game, away_team, home_team),
#       result = ifelse(team_score > opponent_score, "Win",
#                       ifelse(team_score<opponent_score,"Loss",
#                              ifelse(team_score==opponent_score,"Tie",NA))),
#       team_conf = ifelse(is_home_game, home_team_conf, away_team_conf),
#       opponent_conf = ifelse(is_home_game, away_team_conf, home_team_conf),
#       conference_game = team_conf == opponent_conf,
#     ) %>%
#     select(week, opponent, result, conference_game, div_game, gameday)
#   
#   return(team_games)
# }


library(nflreadr)
library(nflfastR)
library(dplyr)

# Function to load NFL data
load_nfl_data <- function() {
  nflScheduleData <- nflreadr::load_schedules(2023)
  nflTeamsInfo <- load_teams()
  nflData <- list(schedule = nflScheduleData, teams = nflTeamsInfo)
  return(nflData)
}

# Function to create game data for each team
byTeamGameData <- function(nflScheduleData, nflTeamsInfo) {
  if (!("team_abbr" %in% names(nflTeamsInfo))) {
    stop("Error: 'team_abbr' column not found in team info data")
  }
  
  team_game_data <- setNames(
    lapply(nflTeamsInfo$team_abbr, function(team_abbr) create_team_df(team_abbr, nflScheduleData, nflTeamsInfo)),
    nflTeamsInfo$team_abbr
  )
  return(team_game_data)
}

# Function to create a DataFrame for each team
create_team_df <- function(team_name, nflScheduleData, nflTeamsInfo) {
  # Validating required columns in nflScheduleData
  required_columns <- c("home_team", "away_team", "home_score", "away_score", "gameday")
  if (!all(required_columns %in% names(nflScheduleData))) {
    stop("Error: Required columns missing in schedule data")
  }
  
  games_with_conf <- nflScheduleData %>%
    left_join(nflTeamsInfo, by = c("home_team" = "team_abbr")) %>%
    rename(home_team_conf = team_conf, home_team_div = team_division) %>%
    left_join(nflTeamsInfo, by = c("away_team" = "team_abbr")) %>%
    rename(away_team_conf = team_conf, away_team_div = team_division)
  
  # Filtering and mutating to create the team's game DataFrame
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
      team_conf = ifelse(is_home_game, home_team_conf, away_team_conf),
      opponent_conf = ifelse(is_home_game, away_team_conf, home_team_conf),
      conference_game = team_conf == opponent_conf,
      div_game = home_team_div == away_team_div
    ) %>%
    select(week, opponent, result, conference_game, div_game, gameday)
  
  return(team_games)
}

x <- load_nfl_data()
y <- byTeamGameData(nflData$schedule, nflData$teams)
