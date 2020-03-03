
#' Scrape Team Season Stats
#'
#' Scrape the seasonal statistics for a specific team.
#' @param team A string containing the full name of the team
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @param defensive Whether to return defensive stats or offensive. Only applies for NBA and MLB teams
#' @return A data frame containing seasonal data for the team. Both offensive
#'     and defensive return the same set of advanced statistics for the
#'     season and then the rest are their respective statistics.
#' @export
team_stats <- function(team, league, defensive = F){
  if(league == "NBA"){
    df <- nba_team(team, defensive)
  } else if (league == "NHL"){
    if(defensive){
      warning("defensive has no use with NHL")
    }
    df <- nhl_team(team)
  } else if (league == "MLB"){
    df <- mlb_team(team, defensive)
  } else {
    stop(paste0("Error: league ", "'", league,"'"," not recognized"))
  }
  df
}

# This function searches for team and navigates to appropriate page
access_team_page <- function(url, search){
  s <- rvest::html_session(url)
  f <-
    rvest::html_form(s)[[1]] %>%
    rvest::set_values(., search=search)
  s <-
    rvest::submit_form(s,f)$url %>%
    rvest::html_session(.)
  s <- rvest::follow_link(s, search)
}


nba_team <- function(team, defensive){
  url <- "https://www.basketball-reference.com/"
  page <- access_team_page(url, team)

  # Data Collection
  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)

  if (defensive){
    page <-
      page %>%
      rvest::follow_link(., "Opponent Stats Per Game")
  } else {
    page <-
      page %>%
      rvest::follow_link(., "Team Stats Per Game")
  }

  # Data Cleaning
  df2 <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)
  df2 <- df2[!stringr::str_detect(df2$Season, "[a-zA-Z]"), ]
  df2 <- df2[!(colSums(is.na(df2)) == nrow(df2))]
  df2 <-
    df2 %>%
    dplyr::select(., -Lg, -Tm, -W, -L, -Finish, -G, -MP)
  if(defensive){
    names(df2)[-1] <- paste("O_", names(df2)[-1], sep = "")
  }
  df <- dplyr::left_join(df, df2, by = "Season")

  df$Team <- stringr::str_extract(df$Team, "[a-zA-Z ]*")
  df <- df[!(colSums(is.na(df)) == nrow(df))]
}

nhl_team <- function(team){
  url <- "https://www.hockey-reference.com"
  page <- access_team_page(url, team)

  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)

  # Data Cleaning
  df$T[is.na(df$T)] <- 0
  df$Finish <- stringr::str_extract(df$Finish, "[0-9]")
  df$Team <- stringr::str_extract(df$Team, "[a-zA-Z_ ]*")

  df
}

mlb_team <- function(team, defensive){
  url <- "https://www.baseball-reference.com"
  page <- access_team_page(url, team)

  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)

  if(defensive){
    page <-
      page %>%
      rvest::follow_link(., "Pitching")
  } else {
    page <-
      page %>%
      rvest::follow_link(., "Batting")
  }

  df2 <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)
  df2 <-
    df2 %>%
    dplyr::select(., -Lg, -W, -L, -Finish)

  if(defensive){
    #should change variable names that this shares with batting statistics
  }
  df <- dplyr::left_join(df, df2, by = "Year")
  # Data Cleaning
  df$GB[df$GB == "--"] <- 0

  df
}
