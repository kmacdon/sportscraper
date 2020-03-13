load("data/team_test_data.RData")
load("data/team_table_data.RData")

if(at_home()){
  expect_equivalent(sportscraper::team_data("Boston Bruins", "NHL"), 
                    team_test_data[["Boston Bruins"]])
} else {
  expect_equivalent(sportscraper:::nhl_team("Boston Bruins", team_table_data[["Boston Bruins"]]), 
                    team_test_data[["Boston Bruins"]])
  expect_equivalent(sportscraper:::nfl_team("Chicago Bears", team_table_data[["Chicago Bears"]]), 
                    team_test_data[["Chicago Bears"]])
}

expect_error(team_data("Chicago Cubs", "SDF"))