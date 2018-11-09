#' ---
#' output:
#'   md_document:
#'     pandoc_args: [
#'     ]
#' ---

#'<!-- language-all: lang-r -->

#+ reprex-setup, include = FALSE
options(tidyverse.quiet = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)



#+ reprex-body

access_page <- function(url, search){
  #This is the code to seach for player name on basketball reference
  s <- html_session(url)
  f <- html_form(s)[[1]] %>%
    set_values(., search=search)
  s <- submit_form(s,f)$url %>%
    html_session(.)

  #This checks if search goes directly to player page or search page
  #search page ends in '=', players page ends in 'html'
  if(str_sub(s$url, nchar(s$url), -1) == "="){
    if(class(follow_link(s, search)) == "try-error"){
      stop(paste("No ", search, "in database."))
    }
    s <- follow_link(s, search)
    # warning("Multiple players returned. Selecting first on search page.")
  }
  s
}

nba_player <- function(player, advanced = F){
  url <- "https://www.basketball-reference.com/"
  s <- access_page(url, player)

  #Data cleaning
  if(!advanced){
    df <- s %>%
      read_html(.) %>%
      html_table(., fill=T) %>%
      as.data.frame(.) %>%
      as.tibble(.) %>%
      dplyr::filter(., Season != "Career")
  } else {
    df <- s %>% read_html(.) %>%
      html_nodes(., xpath = "//comment()") %>%
      html_text(.) %>%
      paste(., collapse = "") %>%
      read_html(.) %>%
      html_table(., fill = T) %>%
      .[[4]] %>%
      .[, grep("[a-zA-Z]", names(df))]
  }
  #Checks if player has team stats too (only if he's been traded)
  if(length(grep("season", df$Season))> 0){
    df <- df %>%
      .[-grep("season", .$Season), ]
  }
  names(df)[11:21] <- c("FG%", "3PM", "3PA", "3P%", "2PM", "2PA", "2P%", "eFG%", "FT", "FTA", "FT%")
  df$Names <- rep(player, nrow(df))
  df
}



#' Created on `r Sys.Date()` by the [reprex package](http://reprex.tidyverse.org) (v`r utils::packageVersion("reprex")`).

