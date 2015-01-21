#' Some Basic Cleaning on Data Downloaded from PIA
#'
#' This function changes column names of batting and pitching data downloaded
#' from PIA.  It also changes some of the labels, and removes rows where player
#' did not record a plate appearance.  Finally, it ensures that player played in
#' levels for which we have MLEs.
#' @param minors is a data.frame with the statistics from the minors that have
#'   been download from PIA.
#' @param minors is a data.frame with the statistics from the minors that have
#'   been download from PIA.
#' @param okay_levels the levels for which we wish to calculate MLEs.  Any
#'   observations which aren't at these levels are removed.  Defaults to
#'   c("High_A", "AA", "AAA", "MLB").
#' @details Changes column names, names of levels, and name of Florida
#'   organization to make typing easier.  For example,
#'   "Level.of.Play.Name.Abbrev" is changed to "Level."  Then removes any rows
#'   where the batter recorded 0 PA or the pitcher has 0 batters faced.
#' @return \code{tbl_df} of cleaned statistics
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' min <- read.csv("0-downloads/bat_minors2.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' maj <- read.csv("0-downloads/bat_majors2.csv", header = T, stringsAsFactors = F) %>% tbl_df()
#' x <- clean_PIA(min, maj)
#' x
#' setwd(curr_wd)
clean_PIA <- function(minors, majors, okay_levels = c("High_A", "AA", "AAA", "MLB")) {
  dat <- rbind_list(minors, majors) %>% tbl_df()
  names(dat) <- sapply(names(dat), function(x) {
    switch(x,
           "MLBID" = "MLBID", # id info
           "Level.of.Play.Name.Abbrev" = "Level",
           "Organization.Shortname" = "ML.Org",
           "Stat.Season.Year" = "Year",
           # batting info
           "PA..B." = "PA",
           "AB..B." = "AB",
           "G..B." = "G",
           "X1B..B." = "X1B",
           "X2B..B." = "X2B",
           "X3B..B." = "X3B",
           "HR..B." = "HR",
           "SO..B." = "K",
           "HBP..B." = "HBP",
           "BB..B." = "BB",
           # pitching info
           "G..P." = "G",
           "IP...formula.only..P." = "IP",
           "PA..P." = "TBF",
           "X1B..P." = "X1B",
           "X2B..P." = "X2B",
           "X3B..P." = "X3B",
           "HR..P." = "HR",
           "SO..P." = "K",
           "HBP..P." = "HBP",
           "BB..P." = "BB")
  })
  dat_write <- dat %>%
    mutate(Level = stringr::str_replace_all(Level, "A \\(Adv\\)", "High_A"),
           ML.Org = stringr::str_replace_all(ML.Org, "FLA", "MIA")) %>%
    filter(G > 0,
           Level %in% okay_levels)
  if("PA" %in% names(dat)) dat_write <- filter(dat_write, PA > 0)
  if("TBF" %in% names(dat)) dat_write <- filter(dat_write, TBF > 0)
  dat_write
}

#' Adjust statistics for park effects
#'
#' This takes a data.frame returned from \code{\link{clean_PIA}} and adjusts the
#' statistics for park effects.
#' @param stats data.frame of player statistics.  Obtained from
#'   \code{\link{clean_PIA}}
#' @param pfs data.frame of multipliers for park factors.  Must be in proper
#'   form.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting.
#' @param b_bio optional data.frame.  Contains the handedness of batters.  Used
#'   because park factors vary for left vs right handed batters.
#' @details If \code{type = "bat"}, then \code{stats} is left joined with
#'   \code{b_bio} and the two are joined by the \code{MLBID} column.  This will
#'   add the \code{Bats} column to the \code{stats} data.  For pitchers, the
#'   \code{Bats} column is set to be "Both" (i.e. they are treated as switch
#'   hitters).
#'
#'   Next, the statistics are turned into long format using \code{\link{gather}}
#'   from the \code{tidyr} package.  If any park factors are missing,
#'   they are set to average (which is 100).  Statistics are then adjusted using
#'   the formula: \deqn{adjusted = count / (PF / 200 + .5)} Extraneous columns
#'   are discarded then the data are returned to a wide format using
#'   \code{\link{spread}} from the \code{tidyr} package.
#' @return \code{tbl_df} of statistics that have been adjusted for park factors.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("1-cleaned/batters/pia.csv", header = T, stringsAsFactors = F)
#' pfs <- read.csv("manual-info/Park_Factors.csv", header = T, stringsAsFactors = F)
#' b_bio <- read.csv("manual-info/bio_bat.csv", header = T, stringsAsFactors = F)
#' x <- adjust_park_factors(stats, pfs, "bat", b_bio)
#' setwd(curr_wd)
adjust_park_factors <- function(stats, pfs, type = c("bat", "pit"), b_bio = NULL) {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "bat") {
    stats_long <- stats %>%
      tbl_df() %>%
      left_join(dplyr::select(b_bio, MLBID, Bats)) %>%
      mutate(Bats = as.character(Bats),
             Bats = ifelse(Bats == "" | is.na(Bats), "Both", Bats)) %>%
      tidyr::gather(Statistic, Count, -MLBID, -Bats, -Year, -Level, -ML.Org, -G, -PA, -AB)
  } else {
    stats_long <- stats %>%
      tbl_df() %>%
      tidyr::gather(Statistic, Count, -MLBID, -Year, -Level, -ML.Org, -G, -IP, -TBF)
    pfs <- pfs %>% filter(Bats == "Both") %>% select(-Bats)
  }
  stats_pf <- rbind_list(inner_join(stats_long, pfs),
                         anti_join(stats_long, pfs)) %>%
    tbl_df() %>%
    mutate(wPF = ifelse(is.na(wPF), 100, wPF),
           adj_pf = Count / (wPF / 200 + .5)) %>%
    dplyr::select(-Count, -wPF) %>%
    tidyr::spread(Statistic, adj_pf)
  stats_pf
}

#' Adjust for Changes in Run-Scoring Environment
#'
#' Uses year multipliers to adjust statistics to account for changes in run
#' scoring environment.  This is particularly useful because a 0.300 OBP in 2006
#' is much different from a 0.300 OBP in 2014.
#' @param stats data.frame of player statistics.  Obtained from
#'   \code{\link{adjust_park_factors}}.
#' @param yfs data.frame of multipliers for year effects.  Must be in proper
#'   form.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @details \code{stats} are left joined with \code{yfs}.  If any year factors
#'   are missing, they are set to average (which is 100).  The statistics are
#'   turned into long format using \code{\link{gather}} from the
#'   \code{tidyr} package.  Statistics are then adjusted using the
#'   formula: \deqn{adjusted = Count * 100 / YF} Extraneous columns are
#'   discarded then the data are returned to a wide format using
#'   \code{\link{spread}} from the \code{tidyr} package.
#' @return \code{tbl_df} of statistics that have been adjusted for year effects.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("2-park-adjusted/bat-pf-adjust.csv", header = T, stringsAsFactors = F)
#' yfs <- read.csv("manual-info/Year-Factors.csv", header = T, stringsAsFactors = F)
#' x <- adjust_year_effects(stats, yfs, type = "bat")
#' setwd(curr_wd)
adjust_year_effects <- function(stats, yfs, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "bat" & ("Bats" %in% names(stats))) {
    stats <- stats %>% dplyr::select(-Bats)
  }
  if(type == "bat") {
    stats <- stats %>%
      tbl_df() %>%
      tidyr::gather(Statistic, Count, -MLBID, -Year, -ML.Org, -Level, -G, -PA, -AB)
  } else {
    stats <- stats %>%
      tbl_df() %>%
      tidyr::gather(Statistic, Count, -MLBID, -Year, -ML.Org, -Level, -G, -IP, -TBF)
  }
  stats %>%
    left_join(yfs) %>%
    mutate(YF = ifelse(is.na(YF), 100, YF),
           adj_yf = Count * 100 / YF) %>%
    dplyr::select(-Count, -YF) %>%
    tidyr::spread(Statistic, adj_yf)
}

#' Calculates Major League Equivalencies (MLEs) for specified statistics
#'
#' Takes statistics returned from adjust_park_factors() and turns them into MLEs
#' @param stats data.frame of player statistics.  Obtained from
#'   adjust_park_factors()
#' @param mles data.frame of multipliers for MLEs.  Must be in proper form.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @details \code{stats} are left joined with \code{mles}.  If any MLEs are
#'   missing, they are set to average (which is 1).  The statistics are turned
#'   into long format using \code{\link{gather}} from the \code{tidyr}
#'   package.
#'
#'   For batters statistics are adjusted by multiplying by the multiplier
#'   \deqn{adjusted = Count * MLE}.  For pitchers, statistics are divided by the
#'   multiplier \deqn{adjusted = Count / MLE}. Extraneous columns are discarded
#'   then the data are returned to a wide format using \code{\link{spread}} from
#'   the \code{tidyr} package.
#'
#'   Since the same multiplier is used for batters and pitchers, it is assumed
#'   that the transition from one level to the next is the same for batters and
#'   pitchers.  That is, this assumes that strikeouts increase for batters and
#'   decrease for pitchers at the same rate when changing level.  Additional
#'   research could be done to see if this assumption is valid.
#' @return \code{tbl_df} of statistics that have been adjusted for level
#'   effects.
#' @examples
#' curr_sd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("3-year-adjusted/bat-yf-adjust.csv", header = T, stringsAsFactors = F)
#' mults <- read.csv("manual-info/Level_Multipliers.csv", header = T, stringsAsFactors = F)
#' x <- calc_MLEs(stats, mults, type = "bat")
#' setwd(curr_wd)
calc_MLEs <- function(stats, mles, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  if(type == "bat") stats <- mutate(stats, SDT = X1B + X2B + X3B)
  if(type == "pit") mles$Multiplier <- 1 / mles$Multiplier
  mles_temp <- stats %>%
    tbl_df() %>%
    mutate(SDT = ifelse(is.na(SDT), X1B + X2B + X3B, SDT)) %>%
    dplyr::select(-X1B)
  if(type == "bat") mles_temp <- mles_temp %>% tidyr::gather(Statistic, Count, -c(MLBID, ML.Org, Level, Year:AB))
  else mles_temp <- mles_temp %>% tidyr::gather(Statistic, Count, -c(MLBID, ML.Org, Level, Year:TBF))
  mles <- mles_temp %>%
    left_join(mles) %>%
    mutate(Multiplier = ifelse(is.na(Multiplier), 1, Multiplier),
           MLE = Count * Multiplier) %>%
    dplyr::select(-Count, -Multiplier) %>%
    tidyr::spread(Statistic, MLE) %>%
    mutate(X1B = SDT - X2B - X3B)
  if(type == "bat") mles <- dplyr::select(mles, MLBID, Year, ML.Org, Level, G:AB, X1B, X2B, X3B, HR, BB, HBP, K)
  else mles <- dplyr::select(mles, MLBID, Year, ML.Org, Level, G:TBF, SDT, HR, BB, HBP, K)
  mles
}

#' Combine Statistics with Bio, Position, and DL Info
#'
#' Takes the clean statistics (can be either actual or MLEs) and adds bio,
#' position, and dl info
#' @param stats data.frame of player statistics.  Obtained from clean_PIA()
#' @param bio data.frame of Bio Data.  Must be in proper form.
#' @param position data.frame of Positions.  Must be in proper form.
#' @param dl data.frame of DL Time.  Must be in proper form.
#' @return \code{tbl_df} of statistics that have been joined with \code{bio},
#'   \code{pos}, \code{dl}.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("4-mles/bat-mles.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' bio <- read.csv("manual-info/bio_bat.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' pos <- read.csv("manual-info/positions.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' dl <- read.csv("manual-info/injuries.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' x <- combine(stats, bio, pos, dl)
#' setwd(curr_wd)
combine <- function(stats, bio, pos, dl) {
  stats %>%
    left_join(bio) %>%
    left_join(pos) %>%
    left_join(dl) %>%
    mutate(DL.Days = ifelse(is.na(DL.Days), 0, DL.Days),
           Age = year_diff(Date.of.Birth, paste0("6/30/", Year)),
           Experience = Year - Start.Year + 1,
           Level = factor(Level, levels = c("High_A", "AA", "AAA", "JPPL", "JPCL",  "MLB")))
}

#' Find Missing Data
#'
#' Identifies players with missing data.  Records these players and saves files
#' with these players' MLBID so that missing data may be filled in by the user
#' at a later time.
#' @param dat data.frame.  This contains both statistics and bio data so that
#'   missing data may be identified by the user.  Obtained from
#'   \code{\link{combine}}.
#' @param ignore data.frame.  This contains MLBID that the user wants to ignore
#'   when identifying missing data.  Must be a data.frame with a column named
#'   MLBID
#' @return a data.frame containing bio info for any player with missing data.
#'   Most of the time, it should just be bio data that is missing.  If however,
#'   a batting/pitching statistic is missing, the player with the missing data
#'   will be identified.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("4-mles/bat-mles.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' bio <- read.csv("manual-info/bio_bat.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' pos <- read.csv("manual-info/positions.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' dl <- read.csv("manual-info/injuries.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' ignore <- read.csv("manual-info/players_to_ignore.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' x <- combine(stats, bio, pos, dl)
#' m <- identify_missing(x, ignore)
#' setwd(curr_wd)
identify_missing <- function(dat, ignore) {
  miss_bio <- dat %>%
    filter(!(MLBID %in% ignore$MLBID))
  if("Bats" %in% names(dat)) {
    miss_bio <- miss_bio %>%
      filter(is.na(Name) | Name == "" |
               is.na(Height) | Height == "" |
               is.na(Weight) | Weight == "" |
               is.na(Bats) | Bats == "" |
               is.na(Throws) | Throws == "" |
               is.na(Date.of.Birth) | Date.of.Birth == "" |
               is.na(ROO) | ROO == "" |
               is.na(Start.Year) | as.character(Start.Year) == "")
  } else {
   miss_bio <- miss_bio %>%
     filter(is.na(Name) | Name == "" |
              is.na(Height) | Height == "" |
              is.na(Weight) | Weight == "" |
              is.na(Throws) | Throws == "" |
              is.na(Date.of.Birth) | Date.of.Birth == "" |
              is.na(ROO) | ROO == "" |
              is.na(Start.Year) | as.character(Start.Year) == "")
  }
  if(nrow(miss_bio) > 0) {
    miss_bio <- miss_bio %>%
      group_by(MLBID) %>%
      summarise_each(funs(x = paste(unique(.), collapse = "/")),
                     c(MLBID, Name:Start.Year)) %>%
      arrange(MLBID)
  }
  miss_bio
}
#' Find Missing Position Information
#'
#' Identifies player-seasons for which the player's position is missing
#' @param dat data.frame containing MLBID, Year, and Position columns.  Obtained
#'   from \code{\link{combine}}.
#' @param ignore data.frame.  This contains MLBID that the user wants to ignore
#'   when identifying missing data.  Must be a data.frame with a column named
#'   MLBID
#' @return a data.frame containing the MLBID and Year in which the Player's
#'   Position is missing.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("4-mles/bat-mles.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' bio <- read.csv("manual-info/bio_bat.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' pos <- read.csv("manual-info/positions.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' dl <- read.csv("manual-info/injuries.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' ignore <- read.csv("manual-info/players_to_ignore.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' x <- combine(stats, bio, pos, dl)
#' m2 <- identify_missing_positions(x, ignore)
#' setwd(curr_wd)
identify_missing_positions <- function(dat, ignore) {
  miss_pos <- dat %>%
    filter(!(MLBID %in% ignore$MLBID)) %>%
    filter(Position == "" | is.na(Position))
  if(nrow(miss_pos) > 0) {
    miss_pos <- miss_pos %>%
      group_by(MLBID, Name, Year) %>%
      summarise(Position = first(Position)) %>%
      ungroup()
  }
  miss_pos
}

#' Final Cleaning before Processing for Apps
#'
#' This removes extra columns that are no longer needed and sorts the data.
#' @param dat data.frame obtained from \code{\link{combine}}.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return \code{tbl_df} of statistics, bio, position, and DL information that
#'   are ready to be processed for the apps.
#' @examples
#' curr_wd <- getwd()
#' setwd("N:/Apps/simScoresApp/data")
#' stats <- read.csv("4-mles/bat-mles.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' bio <- read.csv("manual-info/bio_bat.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' pos <- read.csv("manual-info/positions.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' dl <- read.csv("manual-info/injuries.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' x <- combine(stats, bio, pos, dl)
#' f <- final_clean(x, "bat")
#'
#' # pitchers
#' pstats <- read.csv("4-mles/pit-mles.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' pbio <- read.csv("manual-info/bio_pit.csv", header = T, stringsAsFactors = F) %>% tbl_df() %>% mutate(MLBID = as.character(MLBID))
#' px <- combine(pstats, pbio, pos, dl)
#' pf <- final_clean(px, "pit")
#' setwd(curr_wd)
final_clean <- function(dat, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  dat_clean <- dat %>%
    dplyr::select(MLBID, Name:Throws, ROO, Position, Experience, Year, Age, DL.Days, Level, ML.Org, G:K) %>%
    arrange(Name, Year, ML.Org, Level) %>%
    mutate_each(funs(x = round(., 1)), G:K) %>%
    filter(!is.na(Name), !is.na(Age))
  if(type == "bat") {
    dat_clean <- dat_clean %>%
      mutate(PA.G = round(PA / G, 2),
             K.perc = round(K / PA * 100, 2),
             BB.perc = round(BB / PA * 100, 2),
             OBP = round((X1B + X2B + X3B + HR + BB + HBP) / PA, 3),
             ISO = round((X2B + 2*X3B + 3*HR) / AB, 3),
             BABIP = round((X1B + X2B + X3B) / (AB - K - HR), 3))
  } else {
    dat_clean <- dat_clean %>%
      mutate(IP = ifelse(IP <= 0, .1, round(IP, 1)),
             K.9 = round(K / IP * 9, 2),
             BB.9 = round((BB + HBP) / IP * 9, 2),
             HR.9 = round(HR / IP * 9, 2),
             BABIP = round(SDT / (TBF - BB - HBP - K - HR), 3),
             WHIP = round((SDT + HR + BB + HBP) / IP, 2),
             FIP = round((13 * HR + 3 * (BB + HBP) - 2 * K) / IP + 3.2, 2),
             IP.G = round(IP / G, 2))
  }
  dat_clean
}
