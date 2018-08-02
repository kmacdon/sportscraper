# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


#' Player PG Stats
#'
#' Scrape the career per game statistics for a given player
#' @param player A string containing the player's full name
#' @param league A string containing the league to be searched. One of: 'NFL', 'NBA', 'MLB', 'NHL'.
#' @return Returns a data frame containing the career per game stats by season
#' @export
player_stats <- function(player, league){
  if(league == "NBA"){
    url <- "https://www.basketball-reference.com/"
    df <- nba_player(url, player)
  } else if (league == "NFL"){
    player = "Tom Brady"
    url = "https://www.pro-football-reference.com"
    df <- nfl_player(url, player)
  }
  as.tibble(df)
}

nba_player <- function(url, player){
  #This is the code to seach for player name on basketball reference
  s <- html_session(url)
  f <- html_form(s)[[1]] %>%
    set_values(., search=player)
  s <- submit_form(s,f)$url %>%
    html_session(.)

  #This checks if search goes directly to player page or search page
  #search page ends in '=', players page ends in 'html'
  if(str_sub(s$url, nchar(s$url), -1) == "="){
    if(class(follow_link(s, player)) == "try-error"){
      stop(paste("No player named", player, "in database."))
    }
    s <- follow_link(s, player)
    # warning("Multiple players returned. Selecting first on search page.")
  }

  #Data cleaning
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.tibble(.) %>%
    dplyr::filter(., Season != "Career")

  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  df
  names(df)[11:21] <- c("FG%", "3PM", "3PA", "3P%", "2PM", "2PA", "2P%", "eFG%", "FT", "FTA", "FT%")
  df$Names <- rep(player, nrow(df))
  df
}

nba_pgstats <- function(player){

  #This is the code to seach for player name on basketball reference
  s <- html_session("http://www.basketball-reference.com/")
  f <- html_form(s)[[1]] %>%
    set_values(., search=player)
  s <- submit_form(s,f)$url %>%
    html_session(.)

  #This checks if search goes directly to player page or search page
  #search page ends in '=', players page ends in 'html'
  if(str_sub(s$url, nchar(s$url), -1) == "="){
    if(class(follow_link(s, player)) == "try-error"){
      stop(paste("No player named", player, "in database."))
    }
    s <- follow_link(s, player)
    # warning("Multiple players returned. Selecting first on search page.")
  }

  #Data cleaning
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    dplyr::filter(., Season != "Career") %>%
    .[grep("-"), ]

  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  df
  names(df)[11:22] <- c("FG%", "3PM", "3PA", "3P%", "2PM", "2PA", "2P%", "eFG%", "FT", "FTA", "FT%")
  df$Names <- rep(player, nrow(df))
  df
}

nfl_player <- function(url, player){
  #This is the code to seach for player name on basketball reference
  s <- html_session(url)
  f <- html_form(s)[[1]] %>%
    set_values(., search=player)
  s <- submit_form(s,f)$url %>%
    html_session(.)

  #This checks if search goes directly to player page or search page
  #search page ends in '=', players page ends in 'html'
  if(str_sub(s$url, nchar(s$url), -1) == "="){
    if(class(follow_link(s, player)) == "try-error"){
      stop(paste("No player named", player, "in database."))
    }
    s <- follow_link(s, player)
    # warning("Multiple players returned. Selecting first on search page.")
  }

  #Data cleaning
  df <- s %>%
    read_html(.) %>%
    html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.) %>%
    dplyr::filter(., Year != "Career")
  df$Year <- substr(df$Year, 1, 4)

  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  # df <- separate(df, QBRec, c("W","L","T"), sep = "-")
  df$Names <- rep(player, nrow(df))
  df
}
