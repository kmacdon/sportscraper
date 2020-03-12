load("data/player_test_data.RData")
load("data/player_table_data.RData")

if(at_home()){
  # Test downloading if building locally
  expect_equivalent(sportscraper::player_data("Kobe Bryant", "NBA", advanced=FALSE), 
                    player_test_data[["Kobe Bryant"]])
  expect_equivalent(sportscraper::player_data("Kobe Bryant", "NBA", advanced=TRUE), 
                    player_test_data[["Kobe Bryant adv"]])
  
  expect_equivalent(sportscraper::player_data("Joe Montana", "NFL"), 
                    player_test_data[["Joe Montana"]])
  
  expect_equivalent(sportscraper::player_data("Bobby Or", "NHL"), 
                    player_test_data[["Bobby Or"]])
  
  expect_equivalent(sportscraper::player_data("Derek Jeter", "MLB", advanced=FALSE), 
                    player_test_data[["Derek Jeter"]])
  expect_equivalent(sportscraper::player_data("Derek Jeter", "MLB", advanced=TRUE), 
                    player_test_data[["Derek Jeter adv"]])
  
  expect_equivalent(sportscraper::player_data("Allen Iverson", "CBB", advanced=FALSE), 
                    player_test_data[["Allen Iverson"]])
  
  expect_equivalent(sportscraper::player_data("Allen Iverson", "CBB", advanced=TRUE), 
                    player_test_data[["Allen Iverson adv"]])
} else {
  expect_equivalent(sportscraper:::nba_player("Kobe Bryant", player_table_data[["Kobe Bryant"]], advanced = FALSE),
                    player_test_data[["Kobe Bryant"]])
  expect_equivalent(sportscraper:::nba_player("Kobe Bryant", player_table_data[["Kobe Bryant"]], advanced = TRUE),
                    player_test_data[["Kobe Bryant adv"]])

  expect_equivalent(sportscraper:::nfl_player("Joe Montana", player_table_data[["Joe Montana"]]), 
                    player_test_data[["Joe Montana"]])
  
  expect_equivalent(sportscraper:::nhl_player("Bobby Or", player_table_data[["Bobby Or"]]), 
                    player_test_data[["Bobby Or"]])
  
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", player_table_data[["Derek Jeter"]], advanced=FALSE), 
                    player_test_data[["Derek Jeter"]])
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", player_table_data[["Derek Jeter"]], advanced=TRUE), 
                    player_test_data[["Derek Jeter adv"]])
  
  expect_equivalent(sportscraper:::cbb_player("Allen Iverson", player_table_data[["Allen Iverson"]], advanced=FALSE), 
                    player_test_data[["Allen Iverson"]])
  
  expect_equivalent(sportscraper:::cbb_player("Allen Iverson", player_table_data[["Allen Iverson"]], advanced=TRUE), 
                    player_test_data[["Allen Iverson adv"]])
}

expect_error(player_data("Kobe Bryant", "NS"))
