#' Clean Stats and Get MLEs
#'
#' This function is a wrapper which cleans data, then adjusts for park factors,
#' then calculates MLEs, then adds biographical, position, and DL information.
#' @param minors character.  Path to csv file of minor league data.  Used in
#'   \code{\link{read.csv}}.  Must be downloaded from PIA in proper format
#' @param majors character.  Path to csv file of major league data.  Used in
#'   \code{\link{read.csv}}.  Must be downloaded from PIA in proper format
#' @param bio_info character.  Path to csv file of bio data.  Used in
#'   \code{\link{read.csv}}.  Must be in proper format
#' @param position character.  Path to csv file of position data.  Used in
#'   \code{\link{read.csv}}.  Must be in proper format
#' @param injuries character.  Path to csv file of DL time.  Used in
#'   \code{\link{read.csv}}. Must be in proper format
#' @param park_factors character.  Path to csv file of park factors.  Used in
#'   \code{\link{read.csv}}.  Must be in proper format
#' @param year_factors character.  Path to csv file of year factors.  Used in
#'   \code{\link{read.csv}}.  Must be in proper format
#' @param multipliers character.  Path to csv file of multipliers for MLEs. Used
#'   in \code{\link{read.csv}}.  Must be in proper format
#' @param ignore_players character.  Path to csv file of players to ignore. Used
#'   in \code{\link{read.csv}}.  Must be in proper format
#' @param clean_dir character.  Path to write csv file of cleaned data.  Used in
#'   \code{\link{write.csv}}.
#' @param pf_dir character.  Path to write csv file of stats adjusted for park
#'   factors.  Used in \code{\link{write.csv}}.
#' @param yf_dir character.  Path to write csv file of stats adjusted for year
#'   effects  Used in \code{\link{write.csv}}.
#' @param mle_dir character.  Path to write csv file of MLEs.  Used in
#'   \code{\link{write.csv}}.
#' @param app_dir character.  Path to write csv file with data for apps.  Used
#'   in \code{\link{write.csv}}.
#' @param miss_dir character.  Path to write csv file with missing data.  Used
#'   in \code{\link{write.csv}}.
#' @param wd character.  Used in \code{\link{setwd}} to change the working
#'   directory
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return nothing.  Saves the data into the specified directories.
#' @examples
#' # batters
#' get_MLEs(minors = "0-downloads/bat_minors2.csv",
#'          majors = "0-downloads/bat_majors2.csv",
#'          bio_info = "manual-info/bio_bat.csv",
#'          position = "manual-info/positions.csv",
#'          injuries = "manual-info/injuries.csv",
#'          park_factors = "manual-info/Park_Factors.csv",
#'          year_factors = "manual-info/Year-Factors.csv",
#'          multipliers = "manual-info/Level_Multipliers.csv",
#'          ignore_players = "manual-info/players_to_ignore.csv",
#'          japan_ids = "manual-info/japan_ids.csv",
#'          download_dir = "0-downloads/",
#'          clean_dir = "1-cleaned/batters/",
#'          pf_dir = "2-park-adjusted/",
#'          yf_dir = "3-year-adjusted/",
#'          mle_dir = "4-mles/",
#'          miss_dir = "5-missing/",
#'          app_dir = "6-for-apps/batters/",
#'          wd = "N:/Apps/simScoresApp/data/",
#'          type = "bat",
#'          yrs = 2005:2014)
#' # pitchers
#' get_MLEs(minors = "0-downloads/pit_minors2.csv",
#'          majors = "0-downloads/pit_majors2.csv",
#'          bio_info = "manual-info/bio_pit.csv",
#'          position = "manual-info/positions.csv",
#'          injuries = "manual-info/injuries.csv",
#'          park_factors = "manual-info/Park_Factors.csv",
#'          year_factors = "manual-info/Year-Factors.csv",
#'          multipliers = "manual-info/Level_Multipliers.csv",
#'          ignore_players = "manual-info/players_to_ignore.csv",
#'          japan_ids = "manual-info/japan_ids.csv",
#'          download_dir = "0-downloads/",
#'          clean_dir = "1-cleaned/pitchers/",
#'          pf_dir = "2-park-adjusted/",
#'          yf_dir = "3-year-adjusted/",
#'          mle_dir = "4-mles/",
#'          miss_dir = "5-missing/",
#'          app_dir = "6-for-apps/pitchers/",
#'          wd = "N:/Apps/simScoresApp/data/",
#'          type = "pit",
#'          yrs = 2005:2014)
get_MLEs <- function(minors, majors, bio_info, position, injuries, park_factors,
                     year_factors, multipliers, ignore_players, japan_ids,
                     download_dir, clean_dir, pf_dir, yf_dir, mle_dir, miss_dir,
                     app_dir, type = c("bat", "pit"), wd = NULL, yrs = 2005:2015) {
  if(!is.null(wd)) {
    curr_wd <- getwd()
    setwd(wd)
  }
  type <- match.arg(type, c("bat", "pit"))
  # loading
  bio <- read.csv(bio_info, header = T, stringsAsFactors = F) %>%
    tbl_df() %>%
    mutate(MLBID = as.character(MLBID))
  pos <- read.csv(position, header = T, stringsAsFactors = F) %>%
    tbl_df() %>%
    mutate(MLBID = as.character(MLBID))
  inj <- read.csv(injuries, header = T, stringsAsFactors = F) %>%
    tbl_df() %>%
    mutate(MLBID = as.character(MLBID))
  pfs <- read.csv(park_factors, header = T, stringsAsFactors = F) %>% tbl_df()
  yfs <- read.csv(year_factors, header = T, stringsAsFactors = F) %>% tbl_df()
  mults <- read.csv(multipliers, header = T, stringsAsFactors = F) %>% tbl_df()
  ignore <- read.csv(ignore_players, header = T, stringsAsFactors = F) %>% tbl_df()
  # 0 - scraping japan
  j <- scrape_bref(yrs, type)
  write.csv(j, file = paste0(download_dir, type, "_japan.csv"), quote = F, row.names = T)
  # 1 - cleaning
  j_ids <- read.csv(japan_ids, header = T, stringsAsFactors = F) %>% tbl_df()
  min <- read.csv(minors, header = T, stringsAsFactors = F) %>% tbl_df()
  maj <- read.csv(majors, header = T, stringsAsFactors = F) %>% tbl_df()
  j <- read.csv(paste0(download_dir, type, "_japan.csv"), header = T, stringsAsFactors = F) %>% tbl_df()
  pia_clean <- clean_PIA(min, maj)
  j_clean <- clean_japan(j, j_ids, type)
  write.csv(pia_clean, file = paste0(clean_dir, "pia.csv"), quote = F, row.names = F)
  write.csv(j_clean, file = paste0(clean_dir, "japan.csv"), quote = F, row.names = F)
  clean_list <- list.files(clean_dir)
  clean_all <- lapply(clean_list, function(x) {
    read.csv(paste(clean_dir, x, sep = "/"), header = T, stringsAsFactors = F) %>%
      mutate(MLBID = as.character(MLBID))
  }) %>%
    rbind_all %>% tbl_df()
  if(type == "bat") {
    pf_adjusted <- adjust_park_factors(clean_all, pfs, type, b_bio = bio)
  } else {
    pf_adjusted <- adjust_park_factors(clean_all, pfs, type)
  }
  yf_adjusted <- adjust_year_effects(pf_adjusted, yfs, type)
  mles <- calc_MLEs(yf_adjusted, mults, type)
  combined_raw <- combine(clean_all, bio, pos, inj)
  combined_mles <- combine(mles, bio, pos, inj)
  miss <- identify_missing(combined_mles, ignore)
  miss_pos <- identify_missing_positions(combined_mles, ignore)
  final_raw <- final_clean(combined_raw, type)
  final_mles <- final_clean(combined_mles, type)

  # saving
  write.csv(pf_adjusted, paste0(pf_dir, type, "-pf-adjust.csv"), quote = F, row.names = F)
  write.csv(yf_adjusted, paste0(yf_dir, type, "-yf-adjust.csv"), quote = F, row.names = F)
  write.csv(mles, paste0(mle_dir, type, "-mles.csv"), quote = F, row.names = F)
  write.csv(miss, paste0(miss_dir, type, "-miss-bio.csv"), quote = F, row.names = F)
  write.csv(miss_pos, paste0(miss_dir, type, "-miss-positions.csv"), quote = F, row.names = F)
  write.csv(final_raw, paste0(app_dir, type, "-raw.csv"), quote = F, row.names = F)
  write.csv(final_mles, paste0(app_dir, type, "-mles.csv"), quote = F, row.names = F)
  if(!is.null(wd)) setwd(curr_wd)
}

#' Process Statistics for Comparison Apps
#'
#' Creats Data for Bio table, Traditional Stats table, and Advanced Stats table.
#' Then, groups statistics by level to compare players
#' @param dat_dir character.  Path to data cleaned by \code{\link{get_MLEs}}.
#' @param app_dir character.  Path to the comparison app that is being used
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return nothing.  Just saves the processed data
#' @examples
#' get_comp_app_data(app_data_dir = "N:/Apps/simScoresApp/data/6-for-apps/batters/",
#'                   app_dir = "N:/Apps/simScoresApp/b-comparison-app/",
#'                   type = "bat")
#' get_comp_app_data(app_data_dir = "N:/Apps/simScoresApp/data/6-for-apps/pitchers/",
#'                   app_dir = "N:/Apps/simScoresApp/p-comparison-app/",
#'                   type = "pit")
get_comp_app_data <- function(app_data_dir, app_dir, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  # loading
  raw <- read.csv(paste0(app_data_dir, type, "-raw.csv"), header = T, stringsAsFactors = F) %>% tbl_df()
  mles <- read.csv(paste0(app_data_dir, type, "-mles.csv"), header = T, stringsAsFactors = F) %>% tbl_df()
  # grouped stats
  age_grp <- sum_stats(mles, type = type, group_level = F)
  level_grp <- sum_stats(mles, type = type, group_level = T)
  # bio info
  bio <- select_bio(raw)
  # traditional stats
  raw_trad<- select_traditional(raw) %>% switch_names()
  mle_trad <- select_traditional(mles) %>% switch_names()
  # advanced stats
  raw_adv <- raw %>% select_advanced(type) %>% switch_names()
  mle_adv <- mles %>% select_advanced(type) %>% switch_names()
  # names and date
  player_names <- unique(raw$Name)
  player_names <- player_names[order(player_names)]
  update_date <- Sys.Date()
  save(age_grp, level_grp, bio, raw_trad, mle_trad, raw_adv, mle_adv,
       player_names, update_date,
       file = paste0(app_dir, type, "_simscore_data.RData"))
}

#' Process Statistics for Aging Curve Apps
#'
#' Creats data for the aging curve apps
#' @param dat_dir character.  Path to data cleaned by \code{\link{get_MLEs}}.
#' @param app_dir character.  Path to the curve app that is being used
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting
#' @return nothing.  Just saves the processed data
#' @examples
#' get_curve_app_data(app_data_dir = "N:/Apps/simScoresApp/data/6-for-apps/batters/",
#'                    app_dir = "N:/Apps/simScoresApp/b-comparison-app/",
#'                    type = "bat")
#' get_curve_app_data(app_data_dir = "N:/Apps/simScoresApp/data/6-for-apps/pitchers/",
#'                    app_dir = "N:/Apps/simScoresApp/p-curve-app/",
#'                    type = "pit")
get_curve_app_data <- function(app_data_dir, app_dir, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  age_grps <- read.csv(paste0(app_data_dir, type, "-mles.csv"), header = T, stringsAsFactors = F) %>%
    tbl_df() %>%
    sum_stats(type = type, group_level = F)
  player_names <- unique(age_grps$name)
  player_names <- player_names[order(player_names)]
  save(age_grps, player_names, file = paste0(app_dir, type, "_curve_data.RData"))
}

#' Updates Data and Deploys Apps with Updated Data to ShinyApps
#'
#' The use of this function is described in the "How the Updates Work" vignette.
#' It downloads data from Japan, then cleans Japanese and PIA data.  It then
#' performs park, year, and level adjustments on the statistics.  Missing data
#' are recorded, then the data are processed for the apps.  The data are saved
#' into the appropriate folders for each app and then the updated apps are
#' deployed to shinyapps.io
#' @param wd character.  The location of the working directory where the apps
#'   are located.
#' @param yrs numeric.  The years for which to scrape Baseball-Reference data
#'   for Japan.
#' @return nothing.  Just downloads and processes data then uploads the
#'   processed data to shinyapps.io
#' @examples
#' update_apps(wd = "N:/Apps/simScoresApp")
update_apps <- function(wd = NULL, yrs = 2005:2015, ...) {
  if(!is.null(wd)) {
    curr_wd <- getwd()
    setwd(wd)
    stopifnot("app-dirs.RData" %in% list.files("data"))
    load("data/app-dirs.RData")
  }
  # updating data
  get_MLEs(minors = paste0(download_dir, "bat_minors2.csv"),
           majors = paste0(download_dir, "bat_majors2.csv"),
           bio_info = paste0(manual_dir, "bio_bat.csv"),
           position = paste0(manual_dir, "positions.csv"),
           injuries = paste0(manual_dir, "injuries.csv"),
           park_factors = paste0(manual_dir, "Park_Factors.csv"),
           year_factors = paste0(manual_dir, "Year-Factors.csv"),
           multipliers = paste0(manual_dir, "Level_Multipliers.csv"),
           ignore_players = paste0(manual_dir, "players_to_ignore.csv"),
           japan_ids = paste0(manual_dir, "japan_ids.csv"),
           download_dir = download_dir,
           clean_dir = paste0(clean_dir, "batters/"),
           pf_dir = pf_dir,
           yf_dir = yf_dir,
           mle_dir = mle_dir,
           miss_dir = miss_dir,
           app_dir = paste0(app_data_dir, "pitchers/"),
           wd = wd,
           type = "bat",
           yrs = yrs)
  print("Batter data updated")
  get_MLEs(minors = paste0(download_dir, "pit_minors2.csv"),
           majors = paste0(download_dir, "pit_majors2.csv"),
           bio_info = paste0(manual_dir, "bio_pit.csv"),
           position = paste0(manual_dir, "positions.csv"),
           injuries = paste0(manual_dir, "injuries.csv"),
           park_factors = paste0(manual_dir, "Park_Factors.csv"),
           year_factors = paste0(manual_dir, "Year-Factors.csv"),
           multipliers = paste0(manual_dir, "Level_Multipliers.csv"),
           ignore_players = paste0(manual_dir, "players_to_ignore.csv"),
           download_dir = download_dir,
           japan_ids = paste0(manual_dir, "japan_ids.csv"),
           clean_dir = paste0(clean_dir, "pitchers/"),
           pf_dir = pf_dir,
           yf_dir = yf_dir,
           mle_dir = mle_dir,
           miss_dir = miss_dir,
           app_dir = paste0(app_data_dir, "pitchers/"),
           wd = wd,
           type = "pit",
           yrs = yrs)
  print("Pitcher data updated")
  # processing for apps
  get_comp_app_data(app_data_dir = paste0(app_data_dir, "batters/"),
                    app_dir = b_comp_dir,
                    type = "bat")
  get_comp_app_data(app_data_dir = paste0(app_data_dir, "pitchers/"),
                    app_dir = p_comp_dir,
                    type = "pit")
  get_curve_app_data(app_data_dir = paste0(app_data_dir, "batters/"),
                     app_dir = b_curve_dir,
                     type = "bat")
  get_curve_app_data(app_data_dir = paste0(app_data_dir, "pitchers/"),
                     app_dir = p_curve_dir,
                     type = "pit")
  print("Data processed for apps")
  # deploy apps
  shinyapps::deployApp(substr(b_comp_dir, 1, nchar(b_comp_dir)-1),
                       b_comp_name, launch.browser = F, lint = F)
  shinyapps::deployApp(substr(p_comp_dir, 1, nchar(p_comp_dir)-1),
                       p_comp_name, launch.browser = F, lint = F)
  print("Comparison apps deployed")
  shinyapps::deployApp(substr(b_curve_dir, 1, nchar(b_curve_dir)-1),
                       b_curve_name, launch.browser = F, lint = F)
  shinyapps::deployApp(substr(p_curve_dir, 1, nchar(p_curve_dir)-1),
                       p_curve_name, launch.browser = F, lint = F)
  print("Aging Curve apps deployed")
  setwd(curr_wd)
}



