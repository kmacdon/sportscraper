#' Player PG Stats
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
#'    df <- player_stats("Kobe Bryant", "NBA")
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
    stop("Error: league not recognized")
  }
  tibble::as.tibble(df)
}

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
    if(class(tryCatch(follow_link(s, search))) == "try-error"){
      stop(paste("No ", search, "in database."))
    }
    # Figure out how many players show up in search results
    text <-
      s %>%
      xml2::read_html(.) %>%
      rvest::html_text(.)
    choices <-
      text %>%
      str_extract_all(., paste(search, "\\(.*?\\)")) %>%
      unlist(.)
    if(length(choices) > 1){
      # Present option to select player based on years played
      print("Multiple players with that name")
      links <-
        text %>%
        stringr::str_remove_all(., "\n") %>%
        stringr::str_squish(.) %>%
        stringr::str_extract_all(., "\\) /.*?\\.html") %>%
        unlist(.) %>%
        stringr::str_extract(., "/players/.*?.html")
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
  s <- access_page(url, player)

  #Data cleaning
  if(!advanced){
    df <-
      s %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.) %>%
      tibble::as_tibble(.) %>%
      dplyr::filter(., Season != "Career")
  } else {
    df <-
      s %>% read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T) %>%
      .[[4]] %>%
      .[, -c(20, 25)] %>%
      tibble::as_tibble(.) %>%
      .[, grep("[a-zA-Z]", names(.))]
  }
  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  df <- data.frame(name = rep(player, nrow(df)), df)
}

nfl_player <- function(player){
  url <- "https://www.pro-football-reference.com"
  s <- access_page(url, player)

  #Data cleaning
  df <-
    s %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.) %>%
    .[grep("[0-9]{4}", .$Year), ]


  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Year)) > 0){
    df <- df %>%
      .[-grep("season", .$Year), ]
  }
  df <- suppressWarnings(separate(df, QBrec, c("W","L","Ties"), sep = "-"))
  #In case QBrec is '0'
  df$W[is.na(df$W)] <- 0
  df$Ties[is.na(df$Ties)] <- 0

  df$Name <- rep(player, nrow(df))
  df$Year <- stringr::str_extract(df$Year, "[0-9]*")

  df
}

nhl_player <- function(player){
  url <- "https://www.hockey-reference.com"
  s <- access_page(url, player)

  #Data cleaning
  df <-
    s %>%
    xml2::read_html(.) %>%
    rvest::html_table(., fill=T) %>%
    as.data.frame(.) %>%
    as.tibble(.)
  c_names <- df[1, ]
  c_names[9] <- "PM"
  c_names[11:14] <- paste("G", df[1, 11:14], sep = "_")
  c_names[15:17] <- paste("A", df[1, 15:17], sep = "_")
  names(df) <- c_names
  if(grep("Awards", names(df)))
    df <- df[, -grep("Awards", names(df))]

  df <-
    df %>%
    filter(., Season != "Career") %>%
    .[-1, ]
  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }

  df$Name <- rep(player, nrow(df))
  df
}

mlb_player <- function(player, advanced){
  url <- "https://www.baseball-reference.com"
  s <- access_page(url, player)

  #Data cleaning
  if(!advanced){
    df <-
      s %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.) %>%
      as.tibble(.) %>%
      dplyr::select(., -Awards) %>%
      dplyr::filter(., stringr::str_detect(Year, "[0-9]{4}"))
  } else {
    df <-
      s %>% xml2::read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T) %>%
      .[[1]] %>%
      tibble::as_tibble(.) %>%
      .[grep("[0-9]{4}", .$Year), ] %>%
      dplyr::select(., -Awards)
  }

  df$Name <- rep(player, nrow(df))
  df
}

cbb_player <- function(player, advanced){
  url <- "https://www.sports-reference.com/cbb"
  s <- access_page(url, player)

  #Data cleaning
  if(!advanced){
    df <-
      s %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill=T) %>%
      as.data.frame(.) %>%
      tibble::as_tibble(.) %>%
      dplyr::filter(., Season != "Career")
  } else {
    df <-
      s %>% read_html(.) %>%
      rvest::html_nodes(., xpath = "//comment()") %>%
      rvest::html_text(.) %>%
      paste(., collapse = "") %>%
      xml2::read_html(.) %>%
      rvest::html_table(., fill = T)
    #Extract list element that contains advanced stats
    df <-
      df %>%
      lapply(., function(x){
        if("WS" %in% names(x)){
          x
        } else {
          x <- NULL
        }
        x
      })
    df <-
      df[sapply(df, function(x){!is.null(x)})] %>%
      .[[1]] %>%
      .[, grep("[a-zA-Z]", names(.))] %>%
      tibble::as_tibble(.) %>%
      dplyr::filter(., Season != "Career")
  }
  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  df <- data.frame(Name = rep(player, nrow(df)), df)
  df
}
