load("data/kb.RData")
load("data/jm.RData")
load("data/bo.RData")
load("data/dj.RData")
load("data/ac.RData")

expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/kb.html"), advanced=FALSE), 
                       kb)

expect_equivalent(sportscraper:::nfl_player("Joe Montana", xml2::read_html("data/jm.html")), 
                  jm)

expect_equivalent(sportscraper:::nhl_player("Bobby Or", xml2::read_html("data/bo.html")), 
                  bo)

expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/dj.html"), advanced=FALSE), 
                  dj)

expect_equivalent(sportscraper:::cbb_player("Austin Carr", xml2::read_html("data/ac.html"), advanced=FALSE), 
                  ac)