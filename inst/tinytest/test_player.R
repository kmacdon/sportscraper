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
  expect_equivalent(sportscraper::player_data("Jim Brown", "NFL"), 
                    player_test_data[["Jim Brown"]])
  expect_equivalent(sportscraper::player_data("Randy Moss", "NFL"), 
                    player_test_data[["Randy Moss"]])
  expect_equivalent(sportscraper::player_data("Ray Lewis", "NFL"), 
                    player_test_data[["Ray Lewis"]])
  
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
  expect_equivalent(sportscraper:::nfl_player("Jim Brown", player_table_data[["Jim Brown"]]), 
                    player_test_data[["Jim Brown"]])
  expect_equivalent(sportscraper:::nfl_player("Randy Moss", player_table_data[["Randy Moss"]]), 
                    player_test_data[["Randy Moss"]])
  expect_equivalent(sportscraper:::nfl_player("Ray Lewis", player_table_data[["Ray Lewis"]]), 
                    player_test_data[["Ray Lewis"]])
  
  expect_equivalent(sportscraper:::nhl_player("Bobby Or", player_table_data[["Bobby Or"]]), 
                    player_test_data[["Bobby Or"]])
  
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", player_table_data[["Derek Jeter"]], advanced=FALSE), 
                    player_test_data[["Derek Jeter"]])
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", player_table_data[["Derek Jeter"]], advanced=TRUE), 
                    player_test_data[["Derek Jeter adv"]])
  
  expect_equivalent(sportscraper:::cbb_player("Allen Iverson", player_table_data[["Allen Iverson"]], advanced=FALSE), 
                    player_test_data[["Allen Iverson"]])
  expect_equivalent(sportscraper:::cbb_player("Kevin Durant", player_table_data[["Kevin Durant"]], advanced=FALSE), 
                    player_test_data[["Kevin Durant"]])
  expect_equivalent(sportscraper:::cbb_player("Allen Iverson", player_table_data[["Allen Iverson"]], advanced=TRUE), 
                    player_test_data[["Allen Iverson adv"]])
}

expect_error(player_data("Kobe Bryant", "NS"))
