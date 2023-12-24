# Playoff-Scenarios

# NFL Standings Calculator

## Overview
This project provides a comprehensive solution for calculating the current NFL standings, including possible playoff seeding scenarios. It handles various complexities such as divisional ties and mixed division situations.

## Features
- Calculates current standings in each NFL division.
- Handles tiebreaker scenarios both within divisions and across different divisions.
- Considers various tiebreaker criteria such as head-to-head results, conference records, and strength of victory.
- Dynamically handles games that haven't occurred yet based on current date.

## Usage
To use this project, you need to have R installed with the following packages: `nflfastR`, `tidyverse`, `nflreadr`, `nflplotR`, and `scales`.

1. **Data Loading**: Load NFL schedules and team data.
2. **Standings Calculation**: Calculate standings based on the current season's data.
3. **Tiebreaker Application**: Apply tiebreaker logic for teams with identical records.

## Functions
- `get_tiebreakers(sort_type)`: Retrieves tiebreaker functions based on the sort type.
- `tiebreaker(teams, team_game_data, sort_type)`: Applies tiebreaker logic to a set of teams.
- `division_standings(divisions, team_game_data)`: Calculates standings for each division.
- Additional functions for specific tiebreaking scenarios and data manipulations.

## Contributing
Contributions to this project are welcome. Please ensure that any pull requests or issues adhere to the project's standards and are thoroughly tested.

---

You can expand each section with more details specific to your project. For example, under "Usage," you could include sample code or commands. Under "Contributing," you could add guidelines for contributors.
