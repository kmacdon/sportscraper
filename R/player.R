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
#'    df <- player_data("Kobe Bryant", "NBA")
#'    df <- player_data("Mike Trout", "MLB", advanced = TRUE)
#' }
#' @export
player_data <- function(player, league, advanced = F){
  tables <- get_tables(player, league)
  if(league == "NBA"){
    df <- nba_player(player, tables, advanced)
  } else if (league == "NFL"){
    df <- nfl_player(player, tables)
  } else if (league == "NHL"){
    df <- nhl_player(player, tables)
  } else if (league == "MLB"){
    df <- mlb_player(player, tables, advanced)
  } else if (league == "CBB"){
    df <- cbb_player(player, tables, advanced)
  }
  df
}

#' @importFrom magrittr "%>%"

# This function searches for player and navigates to appropriate page
get_tables <- function(search, league){
  url <- switch(toupper(league),
                "NBA" = "https://www.basketball-reference.com/",
                "NFL" = "https://www.pro-football-reference.com",
                "NHL" = "https://www.hockey-reference.com",
                "MLB" = "https://www.baseball-reference.com",
                "CBB" = "https://www.sports-reference.com/cbb")
  
  if(is.null(url)){
    stop(paste0("league \'", league, "\' not recognized."))
  }
  
  s <- rvest::html_session(url)
  old_url <- s$url
  f <-
    rvest::html_form(s)[[1]] %>%
    rvest::set_values( search=search)
  
  # Get rid of the "Submitting with 'NULL'" message
  s <-
    suppressMessages(rvest::submit_form(s,f))$url %>%
    rvest::html_session()

  # make sure that search didn't switch sites
  if (stringr::str_extract(old_url, "www\\..*\\.com") != stringr::str_extract(s$url, "www\\..*\\.com")){
    stop(paste0("No player named \'", search, "\' on ", url))
  }
  # This checks if search goes directly to player page or search page
  # search page ends in '=', players page ends in 'html'
  if(stringr::str_sub(s$url, nchar(s$url), -1) == "="){
    test <- tryCatch(rvest::follow_link(s, search), 
                     error = function(e) {
                       stop(paste0("No player named \'", search, "\' on ", url))
                     })

    # Figure out how many players show up in search results
    text <-
      s %>%
      xml2::read_html() %>%
      rvest::html_text()
    choices <-
      text %>%
      stringr::str_extract_all( paste(search, ".*?\\n")) %>%
      lapply( stringr::str_remove, pattern = "\\n") %>%
      unlist()
    if(length(choices) > 1){
      # Present option to select player based on years played
      print("Multiple players with that name")
      links <-
        text %>%
        stringr::str_extract_all( "/players.*\\n") %>%
        unlist() %>%
        stringr::str_remove( "\\n")
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
  
  tables1 <- 
    s %>% 
    xml2::read_html() %>% 
    rvest::html_table(fill = T)
  
  # Some tables are stored in commented out sections
  tables2 <- 
    s %>% 
    xml2::read_html() %>% 
    rvest::html_nodes(xpath = "//comment()") %>%
    rvest::html_text() %>%
    paste(collapse = "") %>%
    xml2::read_html() %>%
    rvest::html_table(fill = T)
  
  # Append lists
  for(i in 1:length(tables2)){
    tables1[[length(tables1) + i]] <- tables2[[i]]
  }
  
  tables1
}

nba_player <- function(player, tables, advanced){
  # Data collection
  if(!advanced){
    df <- tables[[1]]
  } else {
    df <- tables[sapply(tables, function(x) {"PER" %in% names(x)}, simplify = "vector")][[1]]
  }

  # Data cleaning: Remove empty col, summary rows, add name
  df <- df[!(colSums(is.na(df)) == nrow(df))]
  df <- df[stringr::str_detect(df$Season, "[0-9]*-[0-9]*"), ]
  df <- data.frame(Name = rep(player, nrow(df)), df, stringsAsFactors = FALSE)
  df
}

nfl_player <- function(player, tables){
  # Data Collection
  df <- tables[[1]]

  # QBs have differently structured DFs
  if(any(df$Pos == "QB")){
    names(df)[which(names(df) == "Yds")[2]] <- "Yds_L"
    df$Year <- stringr::str_remove_all(df$Year, "[\\*+]")
    
    df <- df[!stringr::str_detect(df$Year, "[a-z]")]
    df <- 
      suppressWarnings(df %>% tidyr::separate("QBrec", c("W", "L", "Tie"), sep="-"))
    
    df$Tie <- ifelse(is.na(df$Tie), 0, df$Tie)
    
    df <- 
      suppressWarnings(df %>% 
      dplyr::mutate_if(function(x){any(stringr::str_detect(x, "[0-9]"))},
                function(x){as.numeric(x)}))
    
    df[is.na(df)] <- 0
    return(df)
  }
  
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
  df <- df[stringr::str_detect(df$Year, "[0-9]{4}"), ]
  df$Year <- stringr::str_extract(df$Year, "[0-9]*")
  
  df <- 
  suppressWarnings(df %>% 
                     dplyr::mutate_if(function(x){any(stringr::str_detect(x, "[0-9]"))},
                                      function(x){as.numeric(x)}))
  
  df$Pos <- toupper(df$Pos)
  df <- data.frame(Name = rep(player, nrow(df)), df, stringsAsFactors = FALSE)
  df
}

nhl_player <- function(player, tables){
  # Data Collection
  df <- tables[[1]]

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
    df <- df[, grep("Awards", names(df), invert=TRUE)]
  }
  df <- data.frame(Name = rep(player, nrow(df)), df, stringsAsFactors = FALSE)
  df
}

mlb_player <- function(player, tables, advanced){
  # Data Colleciton
  if(!advanced){
    df <- tables[[1]]
  } else {
    df <- tables[sapply(tables, function(x) "Salary" %in% names(x), simplify = "vector")][[1]]
  }

  # Data Cleaning
  df <- df[stringr::str_detect(df$Year, "[0-9]{4}"), ]
  if("Awards" %in% names(df)){
    df <- df[, grep("Awards", names(df), invert=TRUE)]
  }
  
  # Convert data types
  df <- 
    df %>% 
    dplyr::mutate_if(function(x){!any(stringr::str_detect(x, "[a-zA-Z]"))},
                     function(x){as.numeric(x)})
  

  df <- data.frame(Name = rep(player, nrow(df)), df, stringsAsFactors = FALSE)
}

cbb_player <- function(player, tables, advanced){
  # Data Collection
  if(!advanced){
    df <- tables[[1]]
  } else {
    # Extract list element that contains advanced stats
    df <- tables[sapply(tables, function(x) "WS" %in% names(x), simplify = "vector")][[1]]
  }

  # Data Cleaning
  df <- df[stringr::str_detect(df$Season, "[0-9]*-[0-9]*"),]
  df <- df[!(colSums(is.na(df)) == nrow(df))]

  df <- data.frame(Name = rep(player, nrow(df)), df, stringsAsFactors = FALSE)
  df
}
