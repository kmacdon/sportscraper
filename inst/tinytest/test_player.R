load("data/player/kb.RData")
load("data/player/kb_adv.RData")
load("data/player/jm.RData")
load("data/player/bo.RData")
load("data/player/dj.RData")
load("data/player/dj_adv.RData")
load("data/player/ac.RData")
load("data/player/ai_adv.RData")

expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/player/kb.html"), advanced=FALSE), 
                       kb)

expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/player/kb_adv.html"), advanced=TRUE), 
                  kb_adv)

expect_equivalent(sportscraper:::nfl_player("Joe Montana", xml2::read_html("data/player/jm.html")), 
                  jm)

expect_equivalent(sportscraper:::nhl_player("Bobby Or", xml2::read_html("data/player/bo.html")), 
                  bo)

expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/player/dj.html"), advanced=FALSE), 
                  dj)

expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/player/dj_adv.html"), advanced=TRUE), 
                  dj_adv)

expect_equivalent(sportscraper:::cbb_player("Austin Carr", xml2::read_html("data/player/ac.html"), advanced=FALSE), 
                  ac)

expect_equivalent(sportscraper:::cbb_player("Allen Iverson", xml2::read_html("data/player/ai_adv.html"), advanced=TRUE), 
                  ai_adv)
