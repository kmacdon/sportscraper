load("data/team_test_data.RData")

if(at_home()){
  expect_equivalent(sportscraper::team_data("Chicago Cubs", "MLB"), 
                    team_test_data[["Chicago Cubs"]])
  
  expect_equivalent(sportscraper::team_data("Boston Bruins", "NHL"), 
                    team_test_data[["Boston Bruins"]])
} else {
  expect_equivalent(sportscraper:::nhl_team("Boston Bruins", xml2::read_html("data/BostonBruins.html")), 
                    team_test_data[["Boston Bruins"]])
}
