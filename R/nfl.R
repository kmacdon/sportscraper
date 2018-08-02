
#' Scrape Team Season Stats
#'
#' Scrape the stats for a specific range of seasons for a team
#' @param team A string containing the full name of the team
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @result A tibble containing seasonal data for the team
#' @export
team_stats <- function(team, league, years){
  if(league == "NBA"){
    df <- nba_team(team)
  }
  df
}

nba_team <- function(team){
  url <- "https://www.basketball-reference.com/"
  df <- list()
  s <- access_page(url, team)
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)
  df <- df[-1, 1:15]
  df <- df[, -9]
  s <- jump_to(s, "stats_per_game_totals.html")
  df2 <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)

  names(df2)[c(16:22, 25)] <- c("FG%","3PM","3PA","3P%","2PM","2PA","2P%","FT%")
  df2 <- df2[, -c(2:7, 11, 12)]
  df <- df %>%
    inner_join(., df2, by = "Season")
  df
}
