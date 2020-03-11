load("data/player/kb.RData")
load("data/player/kb_adv.RData")
load("data/player/jm.RData")
load("data/player/bo.RData")
load("data/player/dj.RData")
load("data/player/dj_adv.RData")
load("data/player/ac.RData")
load("data/player/ai_adv.RData")

expect_equivalent(sportscraper::player_stats("Kobe Bryant", "NBA", advanced=FALSE), 
                       kb)

expect_equivalent(sportscraper::player_stats("Kobe Bryant", "NBA", advanced=TRUE), 
                  kb_adv)

expect_equivalent(sportscraper::player_stats("Joe Montana", "NFL"), 
                  jm)

expect_equivalent(sportscraper::player_stats("Bobby Or", "NHL"), 
                  bo)

expect_equivalent(sportscraper::player_stats("Derek Jeter", "MLB", advanced=FALSE), 
                  dj)

expect_equivalent(sportscraper::player_stats("Derek Jeter", "MLB", advanced=TRUE), 
                  dj_adv)

expect_equivalent(sportscraper::player_stats("Austin Carr", "CBB", advanced=FALSE), 
                  ac)

expect_equivalent(sportscraper::player_stats("Allen Iverson", "CBB", advanced=TRUE), 
                  ai_adv)
