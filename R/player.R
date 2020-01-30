#' Player Per Game Stats
#'
#' Scrape the career per game statistics for a given player
#' @param player A string containing the player's full name
#' @param league A string containing the league to be searched. One of: 'NFL', 'NBA', 'MLB', 'NHL', 'CBB'.
#' @param advanced Default is false. Make true to retrieve advanced NBA, CBB, and MLB stats.
#' @return Returns a data frame containing the career per game stats by season. Descriptions of statistics
#'     can be found by searching the following sites:
#'     \itemize{
#'        \item www.basketball-reference.com
#'        \item www.pro-football-reference.com
#'        \item www.hockey-reference.com
#'        \item www.baseball-reference.com
#'        \item www.sports-reference.com/cbb
#'     }
#'
#' @examples
#' \dontrun{
#'    df <- player_stats("Kobe Bryant", "NBA")
#'    df <- player_stats("Mike Trout", "MLB", advanced = TRUE)
#' }
#' @export
player_stats <- function(player, league, advanced = F){
  if(league == "NBA"){
    df <- nba_player(player, advanced)
  } else if (league == "NFL"){
    df <- nfl_player(player)
  } else if (league == "NHL"){
    df <- nhl_player(player)
  } else if (league == "MLB"){
    df <- mlb_player(player, advanced)
  } else if (league == "CBB"){
    df <- cbb_player(player, advanced)
  } else {
    stop(paste("Error: league ", "'", league,"'"," not recognized", sep=""))
  }
  df
}

#' @importFrom magrittr "%>%"

# This function searches for player and navigates to appropriate page
access_page <- function(url, search){
  s <- rvest::html_session(url)
  f <-
    rvest::html_form(s)[[1]] %>%
    rvest::set_values(., search=search)
  s <-
    rvest::submit_form(s,f)$url %>%
    rvest::html_session(.)

  # This checks if search goes directly to player page or search page
  # search page ends in '=', players page ends in 'html'
  if(stringr::str_sub(s$url, nchar(s$url), -1) == "="){
    test <- tryCatch(rvest::follow_link(s, search), error = function(e) e)
    if(inherits(test, "error")){
      stop(paste0("No player named \'", search, "\' in database."))
    }
    # Figure out how many players show up in search results
    text <-
      s %>%
      xml2::read_html(.) %>%
      rvest::html_text(.)
    choices <-
      text %>%
      stringr::str_extract_all(., paste(search, ".*?\\n")) %>%
      lapply(., stringr::str_remove, pattern = "\\n") %>%
      unlist(.)
    if(length(choices) > 1){
      # Present option to select player based on years played
      print("Multiple players with that name")
      links <-
        text %>%
        stringr::str_extract_all(., "/players.*\\n") %>%
        unlist(.) %>%
        stringr::str_remove(., "\\n")
      links <- paste(url, links, sep = "")

      print(choices)
      while(1){
        selection <- as.numeric(readline(prompt = "Enter number of player to pick: "))
        if(selection %in% 1:length(choices)){
          s <- rvest::html_session(links[selection])
          break
        } else {
          print("Invalid choice. Please choose again")
        }
      }
    } else {
      # Follow link if there is only one player in results
      s <- rvest::follow_link(s, search)
    }

  }
  s
}

nba_player <- function(player, advanced){
  url <- "https://www.basketball-reference.com/"
  page <- access_page(url, player)

  # Data collection
  if(!advanced){
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.)
  } else {
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T)
    # Select advanced data frame from list of all tables on page
    df <- df[sapply(df, function(x) {"PER" %in% names(x)}, simplify = "vector")][[1]]
  }

  # Data cleaning: Remove empty col, summary rows, add name
  df <- df[!(colSums(is.na(df)) == nrow(df))]
  df <- df[stringr::str_detect(df$Season, "[0-9]*-[0-9]*"), ]
  df <- data.frame(Name = rep(player, nrow(df)), df)
}

nfl_player <- function(player){
  url <- "https://www.pro-football-reference.com"
  page <- access_page(url, player)

  # Data Collection
  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    .[[2]]

  # Data Cleaining
  if(names(df)[1] == ""){
    # Need to fix variable names
    names <- numeric(ncol(df))
    for(i in 1:ncol(df)){
      if(names(df)[i] == ""){
        names[i] <- df[1, i]
        next
      }
      names[i] <- paste(names(df)[i],df[1, i], sep = "_")
    }
    names(df) <- names
  }
  df <- df[!stringr::str_detect(df$Year, "[a-zA-Z]"), ]
  df$Year <- stringr::str_extract(df$Year, "[0-9]*")
  df$Pos <- toupper(df$Pos)
  df <- data.frame(Name = rep(player, nrow(df)), df)
}

nhl_player <- function(player){
  url <- "https://www.hockey-reference.com"
  page <- access_page(url, player)

  # Data Collection
  df <-
    page %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    .[[2]]

  # Data Cleaning

  # Fix variable names
  names <- numeric(ncol(df))
  for(i in 1:ncol(df)){
    if(names(df)[i] == ""){
      names[i] <- df[1, i]
      next
    }
    names[i] <- paste(names(df)[i],df[1, i], sep = "_")
  }
  names(df) <- names
  df <- df[stringr::str_detect(df[, 1], "[0-9]*-[0-9]*"), ]
  if("Awards" %in% names(df)){
    df <-
      df %>%
      dplyr::select(., -Awards)
  }
  df <- data.frame(Name = rep(player, nrow(df)), df)
}

mlb_player <- function(player, advanced){
  url <- "https://www.baseball-reference.com"
  page <- access_page(url, player)

  # Data Colleciton
  if(!advanced){
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.)
  } else {
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T)
    df <- df[sapply(df, function(x) "Salary" %in% names(x), simplify = "vector")][[1]]
  }

  # Data Cleaning
  df <- df[!stringr::str_detect(df$Year, "[a-zA-Z]"), ]
  if("Awards" %in% names(df)){
    df <-
      df %>%
      dplyr::select(., -Awards)
  }

  df <- data.frame(Name = rep(player, nrow(df)), df)
}

cbb_player <- function(player, advanced){
  url <- "https://www.sports-reference.com/cbb"
  page <- access_page(url, player)

  # Data Collection
  if(!advanced){
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.)
  } else {
    df <-
      page %>%
      xml2::read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T)
    # Extract list element that contains advanced stats
    df <- df[sapply(df, function(x) "WS" %in% names(x), simplify = "vector")][[1]]
  }

  # Data Cleaning
  df <- df[stringr::str_detect(df$Season, "[0-9]*-[0-9]*"),]
  df <- df[!(colSums(is.na(df)) == nrow(df))]
  if("Awards" %in% names(df)){
    df <-
      df %>%
      dplyr::select(., -Awards)
  }

  df <- data.frame(Name = rep(player, nrow(df)), df)
}
