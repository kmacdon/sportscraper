load("data/team/cb.Rdata")
load("data/team/lak.Rdata")

expect_equivalent(sportscraper:::nfl_team("Chicago Bears", xml2::read_html("data/team/cb.html")), 
                  cb)

expect_equivalent(sportscraper:::nhl_team("Los Angeles Kings", xml2::read_html("data/team/lak.html")), 
                  lak)