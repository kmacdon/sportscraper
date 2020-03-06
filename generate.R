
player <- "Joe Montana"
league <- "NFL"
filename1 <- paste0(tolower(paste0(unlist(stringr::str_extract_all(player, "[A-Z]*")), collapse="")), "_adv.html")
filename2 <- paste0(tolower(paste0(unlist(stringr::str_extract_all(player, "[A-Z]*")), collapse="")), "_adv.RData")
page <- sportscraper:::access_page(player, league)
jm <- sportscraper:::nfl_player(player, page)

xml2::write_html(page, file = paste0("inst/tinytest/data/player/", filename1))
save(jm, file=paste0("inst/tinytest/data/player/", filename2))

team <- "Chicago Cubs"
league <- "MLB"
filename1 <- paste0(tolower(paste0(unlist(stringr::str_extract_all(team, "[A-Z]*")), collapse="")), ".html")
filename2 <- paste0(tolower(paste0(unlist(stringr::str_extract_all(team, "[A-Z]*")), collapse="")), ".RData")
page <- sportscraper:::access_team_page(team, league)
cc <- sportscraper:::mlb_team(team, page, defensive = FALSE)

xml2::write_html(page, file = paste0("inst/tinytest/data/team/", filename1))
save(cc, file=paste0("inst/tinytest/data/team/", filename2))
