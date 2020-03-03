load("data/kb.RData")
load("data/kb_adv.RData")
load("data/jm.RData")
load("data/bo.RData")
load("data/dj.RData")
load("data/ac.RData")
load("data/ai_adv.RData")

expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/kb.html"), advanced=FALSE), 
                       kb)

expect_equivalent(sportscraper:::nba_player("Kobe Bryant", xml2::read_html("data/kb_adv.html"), advanced=TRUE), 
                  kb_adv)

expect_equivalent(sportscraper:::nfl_player("Joe Montana", xml2::read_html("data/jm.html")), 
                  jm)

expect_equivalent(sportscraper:::nhl_player("Bobby Or", xml2::read_html("data/bo.html")), 
                  bo)

expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/dj.html"), advanced=FALSE), 
                  dj)

expect_equivalent(sportscraper:::mlb_player("Derek Jeter", xml2::read_html("data/dj_adv.html"), advanced=TRUE), 
                  dj_adv)

expect_equivalent(sportscraper:::cbb_player("Austin Carr", xml2::read_html("data/ac.html"), advanced=FALSE), 
                  ac)

expect_equivalent(sportscraper:::cbb_player("Allen Iverson", xml2::read_html("data/ai_adv.html"), advanced=TRUE), 
                  ai_adv)
