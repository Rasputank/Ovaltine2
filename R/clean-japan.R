#' Scrapes Batting or Pitching Leaders from Baseball-Reference
#'
#' Takes the url for a Baseball-References leaderboard and scrapes the data over
#' a specified timeframe
#' @param yrs numeric. Vector of the years to scrape the data from
#' @param type Whether to scrape batting or pitching data.  Defaults to batting
#' @param bref_base_url character containing the url with the correct
#'   Baseball-Reference leaderboard.  Defaults to Japanese leaders.
#' @details Pastes \code{yrs} to the end of \code{brefBaseURL} to create a
#'   vectors of urls.  Then scrapes the first table at each of these urls using
#'   \code{\link{readHTMLTable}}.  Then binds the table for each year using
#'   \code{\link{rbind_all}} and returns these stacked tables as a
#'   \code{\link{tbl_df}} object.
#' @return \code{tbl_df} of data scraped from Baseball-Reference.
#' @examples
#' # batters
#' x <- scrape_bref(2005:2014)
#' # pitchers
#' y <- scrape_bref(2005:2014, type = "pit")
scrape_bref <- function(yrs, type = c("bat", "pit"), brefBaseURL = "http://www.baseball-reference.com/japan/leader.cgi?type=bat&year=") {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "pit") brefBaseURL <- stringr::str_replace_all(brefBaseURL, "bat", "pit")
  urls <- paste0(brefBaseURL, yrs)
  downloads <- lapply(urls, function(link) {
    z <- XML::readHTMLTable(link, which = 1, header = T, stringsAsFactors = F)
    z$Year <- substr_right(link, 4)
    z
  })
  downloads[sapply(downloads, is.data.frame)] %>%
    rbind_all %>%
    tbl_df() %>%
    mutate_each(funs(x = stringr::str_replace_all(., ",", "-")), Tm:Year)
}

#' Clean Japanese Leaders Downloaded from Baseball-Reference
#'
#' Takes the Japanese data scraped from Baseball-Reference using
#' \code{\link{scrape_bref}} and cleans it so that it can be combined with
#' the data downloaded from PIA.  Only used to process data for the apps.
#' @param dat data.frame.  Data scraped from Baseball-Reference
#' @param j_ids data.frame of japanese ids for players who have played in both
#'   Japan and America.  The j_ids are custom, used only in this app
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return tbl_df of japanese statistics that have been cleaned.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' dat <- read.csv("0-downloads/bat_japan.csv", header = T, stringsAsFactors = F)
#' jids <- read.csv("manual-info/japan_ids.csv", header = T, stringsAsFactors = F)
#' y <- clean_japan(dat, jids, "bat")
#' setwd(curr_wd)
clean_japan <- function(dat, j_ids, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  clean1 <- dat %>%
    mutate(display_name = j_display_name(Name),
           lastf = getLastF(display_name)) %>%
    group_by(display_name, lastf) %>%
    mutate(id_num = Name %>% as.factor() %>% as.numeric(),
           j_id = paste0(lastf, id_num)) %>%
    ungroup() %>%
    mutate(ML.Org = Tm,
           Level = Lg,
           K = SO)
  if(type == "bat") {
    clean2 <- clean1 %>%
      mutate(X1B = H - X2B - X3B - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, PA, AB, X1B, X2B, X3B, HR, BB, HBP, K)
  } else  {
    clean2 <- clean1 %>%
      mutate(TBF = BF,
             SDT = H - HR) %>%
      dplyr::select(j_id, Year, Level, ML.Org, G, IP, TBF, SDT, HR, BB, HBP, K)
  }
  clean2 %>%
    left_join(j_ids) %>%
    mutate(MLBID = ifelse(is.na(MLBID),
                          j_id,
                          as.character(MLBID))) %>%
    dplyr::select(MLBID, Year:K)
}
