
#' Scrape Team Season Stats
#'
#' Scrape the seasonal statistics for a specific team.
#' @param team A string containing the full name of the team
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @param defensive Whether to return defensive stats or offensive. Only applys for NBA and MLB teams
#' @return A data frame containing seasonal data for the team. Both offensive
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
  } else if (league == "NFL"){
    df <- nfl_team(team)
  } else {
    stop("Error: league not recognized")
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
  s
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

nfl_team <- function(team){
  url <- "https://www.pro-football-reference.com"
  page <- access_team_page(url, team)
  
  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.)
  
  # Cleaning
  col_names <- df[1, ]
  col_names[stringr::str_detect(names(df), "Players")] <- paste0("Top_", col_names[stringr::str_detect(names(df), "Players")])
  col_names[stringr::str_detect(names(df), "Off")] <- paste0("Off_", col_names[stringr::str_detect(names(df), "Off")])
  col_names[stringr::str_detect(col_names, "out of")] <- "Team_Count"
  
  names(df) <- col_names
  df <- df[-1, ]
  
  # remove rows that only have text
  df <- df[apply(df, 1, function(x){any(stringr::str_detect(x, "[0-9]"))}), ]
  
  df <- 
    df %>% 
    mutate_if(., 
              function(x){
      !any(stringr::str_detect(x, "[a-zA-Z]"))
      }, 
      function(x){
        as.numeric(x)
        })
  
  df$Tm <- stringr::str_remove_all(df$Tm, "[\\*]")
  df$`Div. Finish` <- as.numeric(stringr::str_sub(df$`Div. Finish`, end=1))
  
  df
}
