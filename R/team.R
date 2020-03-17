
#' Scrape Team Season Stats
#'
#' Scrape the seasonal statistics for a specific team.
#' @param team A string containing the full name of the team
#' @param league A string containing the league to search. One of: 'NFL', 'NBA', 'NHL', 'MLB'
#' @param defensive Whether to return defensive stats or offensive. Only applies for NBA and MLB teams
#' @return A data frame containing seasonal data for the team. 
#' @export
team_data <- function(team, league, defensive = FALSE){
  page <- get_team_tables(team, league, defensive)
  if(league == "NBA"){
    df <- nba_team(team, page, defensive)
  } else if (league == "NHL"){
    if(defensive){
      warning("defensive has no use with NHL")
    }
    df <- nhl_team(team, page)
  } else if (league == "MLB"){
    df <- mlb_team(team, page, defensive)
  } else if (league == "NFL"){
    df <- nfl_team(team, page)
  }
  df
}

# This function searches for team and navigates to appropriate page
get_team_tables <- function(team, league, defensive){
  url <- switch(toupper(league),
                "NBA" = "https://www.basketball-reference.com/",
                "NFL" = "https://www.pro-football-reference.com",
                "NHL" = "https://www.hockey-reference.com",
                "MLB" = "https://www.baseball-reference.com")
  
  if(is.null(url)){
    stop(paste0("league \'", league, "\' not recognized."))
  }
  
  s <- rvest::html_session(url)
  f <-
    rvest::html_form(s)[[1]] %>%
    rvest::set_values(search=team)
  
  # Get rid of the "Submitting with 'NULL'" message
  s <-
    suppressMessages(rvest::submit_form(s,f)$url) %>%
    rvest::html_session() %>% 
    rvest::follow_link(team)
  
  if(toupper(league) == "NHL"){
    # Part time fix until more robust solution is found
    s <- 
      s %>% 
      rvest::jump_to("history.html") %>% 
      xml2::read_html() %>% 
      rvest::html_table(fill = TRUE)
    return(s)
  } else if (toupper(league) == "MLB"){
    # Another stop gap to keep this working for the time being
    return(s)
  } else if (toupper(league) == "NBA"){
    df <-
      s %>%
      rvest::html_table(fill=T) %>%
      as.data.frame()
    
    if (defensive){
      s <-
        s %>%
        rvest::follow_link("Opponent Stats Per Game")
    } else {
      s <-
        s %>%
        rvest::follow_link("Team Stats Per Game")
    }
    
    df2 <-
      s %>%
      xml2::read_html() %>%
      rvest::html_table(fill=T) %>%
      as.data.frame()
    
    return(list(df, df2))
  }
  
  s <- 
    s %>% 
    rvest::follow_link(team) %>% 
    xml2::read_html() %>% 
    rvest::html_table(fill=TRUE)
  s
}


nba_team <- function(team, tables, defensive){

  df1 <- tables[[1]]
  df2 <- tables[[2]]
  
  
  # Data Cleaning
  df2 <- df2[!stringr::str_detect(df2$Season, "[0-9]{4}\\-[0-9]{2}"), ]
  df2 <- df2[, colSums(is.na(df2)) != nrow(df2)]
  
  df2 <-
    df2 %>%
    dplyr::select(-"Lg", -"Tm", -"W", -"L", -"Finish", -"G", -"MP")
  if(defensive){
    names(df2)[-1] <- paste("O_", names(df2)[-1], sep = "")
  }
  
  df <- dplyr::left_join(df, df2, by = "Season")

  df$Team <- stringr::str_extract(df$Team, "[a-zA-Z ]*")
  df <- df[!(colSums(is.na(df)) == nrow(df))]
  
}

nhl_team <- function(team, tables){

  df <- as.data.frame(tables)

  # Data Cleaning
  df$T[is.na(df$T)] <- 0
  df$Finish <- stringr::str_extract(df$Finish, "[0-9]")
  df$Team <- stringr::str_extract(df$Team, "[a-zA-Z_ ]*")

  df
}

mlb_team <- function(team, page, defensive){

  df <-
    page %>%
    xml2::read_html() %>% 
    rvest::html_table(fill=T) %>%
    as.data.frame()

  if(defensive){
    page <-
      page %>%
      rvest::follow_link("Pitching")
  } else {
    page <-
      page %>%
      rvest::follow_link("Batting")
  }

  df2 <-
    page %>%
    xml2::read_html() %>%
    rvest::html_table(fill=T) %>%
    as.data.frame()
  df2 <-
    df2 %>%
    dplyr::select(-"Lg", -"W", -"L", -"Finish")
  # USE setdiff instead of this select
  
  if(defensive){
    #should change variable names that this shares with batting statistics
  }
  df <- dplyr::left_join(df, df2, by = "Year")
  # Data Cleaning
  df$GB[df$GB == "--"] <- 0

  df
}


nfl_team <- function(team, tables){
  
  prefix <- tolower(stringr::str_replace_all(names(tables[[1]]), " ", "_"))
  col_names <- tables[[1]][1, ]
  col_names <- paste(prefix, col_names, sep = "_")
  col_names <- stringr::str_remove(col_names, "^_")
  df <- as.data.frame(tables[[1]][-1, ])
  names(df) <- col_names
  
  # Cleaning
  # remove rows that only have text
  df <- df[apply(df, 1, function(x){any(stringr::str_detect(x, "[0-9]"))}), ]
  
  df <- 
    df %>% 
    dplyr::mutate_if(function(x){
      !any(stringr::str_detect(x, '[a-zA-Z]'))
    }, 
    function(x){
      as.numeric(x)
    })
  df$Tm <- stringr::str_remove_all(df$Tm, "[\\*]")
  df$`Div. Finish` <- as.numeric(stringr::str_sub(df$`Div. Finish`, end=1))
  
  df
}
