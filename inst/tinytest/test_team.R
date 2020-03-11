load("data/team/cb.RData")
load("data/team/bb.RData")

expect_equivalent(sportscraper::team_stats("Chicago Bears", "NFL"), 
                  cb)

expect_equivalent(sportscraper::team_stats("Boston Bruins", "NHL"), 
                  bb)