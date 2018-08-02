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

nba_player("Tom Brady", "https://www.pro-football-reference.com")
player = "Tom Brady"
url = "https://www.pro-football-reference.com"
