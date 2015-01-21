#' Extract Bio Information
#'
#' Selects Bio information from the specified data.frame
#' @param dat data.frame of statistics.  Obtained from
#'   \code{\link{final_clean}}.
#' @return data.frame containing a player's biographical information
#' @examples
#' x <- read.csv("N:/Apps/simScoresApp/data/6-for-apps/batters/bat-raw.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x1 <- select_bio(x)
select_bio <- function(dat) {
  dat %>%
    group_by(MLBID) %>%
    mutate(Positions = paste(unique(Position), collapse = "/"),
           Years = paste(first(Year), last(Year), sep = ":"),
           Ages = paste(first(Age), last(Age), sep = ":")) %>%
    summarise_each(funs(first), c(Name:ROO, Positions, Years, Ages))
}

#' Selects Traditional Statistics
#'
#' Extracts just the traditional statistics from the specified data.frame
#' @param dat data.frame of statistics.  Obtained from
#'   \code{\link{final_clean}}.
#' @return data.frame containing a player's traditional stats
#' @examples
#' x <- read.csv("N:/Apps/simScoresApp/data/6-for-apps/batters/bat-raw.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x1 <- select_traditional(x)
select_traditional <- function(dat) {
  dat %>%
    select(Name, Year, Age, Experience, Position, DL.Days, Level, ML.Org, G:K) %>%
    arrange(Name, Year, Level, ML.Org)
}

#' Selects Advanced Statistics
#'
#' Extracts just the advanced statistics from the specified data.frame
#' @param dat data.frame of statistics.  Obtained from
#'   \code{\link{final_clean}}.
#' @return data.frame containing a player's advanced stats
#' @examples
#' x <- read.csv("N:/Apps/simScoresApp/data/6-for-apps/batters/bat-raw.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x1 <- select_advanced(x, type = "bat")
select_advanced <- function(dat, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "bat") {
    dat1 <- dat %>%
      select(Name, Year, Level, ML.Org, PA.G:BABIP) %>%
      arrange(Name, Year, Level, ML.Org)
  } else {
    dat1 <- dat %>%
      select(Name, Year, Level, ML.Org, IP.G, K.9:FIP) %>%
      arrange(Name, Year, Level, ML.Org)
  }
  dat1
}

#' Sum Statistics over Grouping Variables
#'
#' Sums statistics for batters or pitchers.  Statistics can then be used in
#' either \code{\link{age_comps}} or \code{\link{level_comps}}.
#' @param dat data.frame of statistics.  Obtained from
#'   \code{\link{final_clean}}.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @param group_level boolean.  Whether these statistics should be grouped by
#'   level.  FALSE for use in \code{\link{age_comps}}, TRUE for use in
#'   \code{\link{level_comps}}.Defaults to FALSE.
#' @return data.frame containing a player's statistics that have been summed. If
#'   \code{group_level} is TRUE, these statistics will be summed for each level
#'   a player recorded statistics.  If it is FALSE, it will be summed by age.
#' @examples
#' x <- read.csv("N:/Apps/simScoresApp/data/6-for-apps/batters/bat-raw.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x1 <- sum_stats(x)
sum_stats <- function(dat, type = c("bat", "pit"), group_level = F) {
  type <- match.arg(type)
  grouped_dat <- dat %>%
    setNames(tolower(names(.))) %>%
    group_by(mlbid, name, height, weight, throws, roo, age, year, position, experience, dl.days)
  if(type == "bat") grouped_dat <- grouped_dat %>% group_by(bats, add = T)
  if(group_level) grouped_dat <- grouped_dat %>% group_by(level, add = T)
  grouped_dat %>%
    summarise_each(funs(sum), g:k) %>%
    ungroup()
}

#' Change Names for Presentation
#'
#' Changes the names of certain statistics to make them look nicer for
#' presentation
#' @param dat data.frame
#' @param names_to_switch
#' @return data.frame with names changed for display
#' @examples
#' x <- read.csv("N:/Apps/simScoresApp/data/6-for-apps/batters/bat-raw.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x1 <- switch_names(x)
switch_names <- function(dat, names_to_switch = c("X1B", "X2B", "X3B", "ML.Org", "DL.Days", "PA.G", "IP.G", "K.perc", "BB.perc", "K.9", "BB.9", "HR.9")) {
  to_switch <- names(dat) %in% names_to_switch
  switched <- sapply(names(dat)[to_switch], function(x) {
    switch(x,
           "X1B" = "1B",
           "X2B" = "2B",
           "X3B" = "3B",
           "ML.Org" = "Org",
           "DL.Days" = "DL_Days",
           "PA.G" = "PA/G",
           "IP.G" = "IP/G",
           "K.perc" = "K%",
           "BB.perc" = "BB%",
           "K.9" = "K/9",
           "BB.9" = "BB/9",
           "HR.9" = "HR/9")
  })
  names(dat)[to_switch] <- switched
  dat
}


