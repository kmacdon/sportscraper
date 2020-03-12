load("data/player_test_data.RData")

if(at_home()){
  # Test downloading if building locally
  expect_equivalent(sportscraper::player_stats("Kobe Bryant", "NBA", advanced=FALSE), 
                    player_test_data[["Kobe Bryant"]])
  expect_equivalent(sportscraper::player_stats("Kobe Bryant", "NBA", advanced=TRUE), 
                    player_test_data[["Kobe Bryant_adv"]])
  
  expect_equivalent(sportscraper::player_stats("Joe Montana", "NFL"), 
                    player_test_data[["Joe Montana"]])
  
  expect_equivalent(sportscraper::player_stats("Bobby Or", "NHL"), 
                    player_test_data[["Bobby Or"]])
  
  expect_equivalent(sportscraper::player_stats("Derek Jeter", "MLB", advanced=FALSE), 
                    player_test_data[["Derek Jeter"]])
  expect_equivalent(sportscraper::player_stats("Derek Jeter", "MLB", advanced=TRUE), 
                    player_test_data[["Derek Jeter_adv"]])
  
  expect_equivalent(sportscraper::player_stats("Austin Carr", "CBB", advanced=FALSE), 
                    player_test_data[["Austin Carr"]])
  
  expect_equivalent(sportscraper::player_stats("Allen Iverson", "CBB", advanced=TRUE), 
                    player_test_data[["Allen Iverson_adv"]])
} else {
  expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/KobeBryant.html"), advanced = FALSE),
                    player_test_data[["Kobe Bryant"]])
  expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/KobeBryant.html"), advanced = TRUE),
                    player_test_data[["Kobe Bryant_adv"]])

  expect_equivalent(sportscraper:::nfl_player("Joe Montana", xml2::read_html("data/JoeMontana.html")), 
                    player_test_data[["Joe Montana"]])
  
  expect_equivalent(sportscraper:::nhl_player("Bobby Or", xml2::read_html("data/BobbyOr.html")), 
                    player_test_data[["Bobby Or"]])
  
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/DerekJeter.html"), advanced=FALSE), 
                    player_test_data[["Derek Jeter"]])
  expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/DerekJeter.html"), advanced=TRUE), 
                    player_test_data[["Derek Jeter_adv"]])
  
  expect_equivalent(sportscraper:::cbb_player("Austin Carr", xml2::read_html("data/AustinCarr.html"), advanced=FALSE), 
                    player_test_data[["Austin Carr"]])
  
  expect_equivalent(sportscraper:::cbb_player("Allen Iverson", xml2::read_html("data/AllenIverson.html"), advanced=TRUE), 
                    player_test_data[["Allen Iverson_adv"]])
}

