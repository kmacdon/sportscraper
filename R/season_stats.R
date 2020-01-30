#' Scrape Season Stats
#'
#' Scrape the seasonal statistics for a specific league and year
#' @param team A year to get statistics for. For season that occur across two years, use the first.
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @param defensive Whether to return defensive stats or offensive. Only applys for NBA and MLB teams
#' @return A data frame containing seasonal data for the league and year specified.
#' @export
season_stats(year, league){
  if(league == "NBA"){
    df <- nba_season(year, defensive)
  } else if (league == "NHL"){
    df <- nhl_season(year)
  } else if (league == "MLB"){
    df <- mlb_season(year, defensive)
  } else if (league == "NFL"){
    df <- nfl_season(year, defensive)
  }  else {
    stop(paste0("Error: league ", "'", league,"'"," not recognized"))
  }
  df
}

# This function searches for the season and league
access_season_page <- function(url, search){
  s <- rvest::html_session(url)
  f <-
    rvest::html_form(s)[[1]] %>%
    rvest::set_values(., search=search)
  s <-
    rvest::submit_form(s,f)$url %>%
    rvest::html_session(.)
  s <- rvest::follow_link(s, search)
}

nba_season <- function(year){
  url <- "https://www.basketball-reference.com"
  page <- access_team_page(url, year)
  
}

