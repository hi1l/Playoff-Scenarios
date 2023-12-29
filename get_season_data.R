library(nflreadr)
library(nflfastR)

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