
#' Scrape Team Season Stats
#'
#' Scrape the seasonal statistics for a specific team.
#' @param team A string containing the full name of the team
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @param defensive Whether to return defensive stats or offensive
#' @return A tibble containing seasonal data for the team. Both offensive
#'     and defensive return the same set of advanced statistics for the
#'     season and then the rest are their respective statistics.
#' @export
team_stats <- function(team, league, defensive = F){
  if(league == "NBA"){
    df <- nba_team(team, defensive)
  } else if (league == "NHL"){
    df <- nhl_team(team)
  } else if (league == "MLB"){
    df <- mlb_team(team, defensive)
  } else {
    stop("Error: league not recognized")
  }
  df
}

nba_team <- function(team, defensive){
  url <- "https://www.basketball-reference.com/"
  s <- access_page(url, team)
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)
  #remove unnecessary columns and rows
  df <- df[-1, 1:15]
  df <- df[, -9]

  if (defensive){
    s <- jump_to(s, "opp_stats_per_game_totals.html")
  } else {
    s <- jump_to(s, "stats_per_game_totals.html")
  }

  df2 <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)
  if (defensive){
    df2 <- df2[, -(2:8)]
    names(df2)[c(5:11, 14)] <- c("FG%", "3PM", "3PA", "3P%", "2PM", "2PA", "2P%", "FT%")
  } else {
    names(df2)[c(16:22, 25)] <- c("FG%","3PM","3PA","3P%","2PM","2PA","2P%","FT%")
    df2 <- df2[, -c(2:7, 11, 12)]
  }

  df <- df %>%
    inner_join(., df2, by = "Season")
  df
}

nhl_team <- function(team){
  url <- "https://www.hockey-reference.com"
  s <- access_page(url, team)
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)
  #remove unnecessary columns and rows
  df <- df[-1, 1:12]
  df$T[is.na(df$T)] <- 0
  df$Team <- str_extract(df$Team, "[a-zA-Z_ ]*")

  df
}

mlb_team <- function(team, defensive){
  url <- "https://www.baseball-reference.com"
  s <- access_page(url, team)
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.) %>%
    .[, 1:19]
  #remove unnecessary columns and rows

  df
}
