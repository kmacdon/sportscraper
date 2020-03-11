load("data/team/cb.Rdata")
load("data/team/bb.Rdata")

expect_equivalent(sportscraper::team_stats("Chicago Bears", "NFL"), 
                  cb)

expect_equivalent(sportscraper::team_stats("Boston Bruins", "NHL"), 
                  bb)