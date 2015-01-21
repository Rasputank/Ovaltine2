#' Differences in Numeric Variables
#'
#' Computes the scaled difference between a vector of numeric variables and a
#' matrix of numeric variables.  Should not usually be called by hand.  Instead,
#' use the \code{\link{calcScores2}} function.
#' @param vec a numeric vector.  Each value in the vector corresponds to a
#'   unique variable.
#' @param dat a numeric matrix or data.frame of numeric variables.  The number
#'   of columns must be equal to the length of \code{vec}.
#' @return a matrix of the same dimensions as \code{dat}.  The ith entry in the
#'   jth row corresponds to the absolute value of the difference between the jth
#'   value of \code{vec} and the jth value of the ith row of \code{dat}.If
#'   either the ith value of the jth row of \code{dat} or the jth value of
#'   \code{vec} are missing, and NA will be returned.
#' @examples
#' example_vec <- c(1,2, NA)
#' example_dat <- iris[1:6, 1:3]
#' numDiffs2(example_vec, example_dat)
numDiffs2 <- function(vec, dat) {
  ranges <- apply(dat, 2, function(x) diff(range(x, na.rm = T)))
  vec_to_mat <- matrix(as.matrix(vec), nrow = nrow(dat), ncol = ncol(dat), byrow = T)
  diffs <- abs(as.matrix(dat) - vec_to_mat)
  apply(diffs, 1, function(x) x / ranges) %>% t()
}

#' Differences in Categorical Variables
#'
#' Computes the scaled difference between a vector of categorical variables and
#' a matrix of categorical variables.  Should not usually be called by hand.
#' Instead, use the \code{\link{calcScores2}} function.
#' @param vec a character vector.  Each value in the vector corresponds to a
#'   unique variable.
#' @param dat a character matrix or data.frame of character variables.  The
#'   number of columns must be equal to the length of \code{vec}.
#' @return a matrix of the same dimensions as \code{dat}.  The ith entry in the
#'   jth row is 0 if the jth value of \code{vec} is equal to the jth value of
#'   the ith row of {dat}.  It is 1 if they are different.  If either the ith
#'   value of the jth row of \code{dat} or the jth value of \code{vec} are
#'   missing, and NA will be returned.
#' @examples
#' example_vec <- c("a", "b", NA)
#' example_dat <- matrix(c("a", "a", "a",
#'                         "b", "b", "b",
#'                         NA, "b", "a"),
#'                       nrow = 3, byrow = T)
#' catDiffs2(example_vec, example_dat)
catDiffs2 <- function(vec, dat) {
  dat <- as.matrix(dat)
  vec_to_mat <- matrix(as.matrix(vec), nrow = nrow(dat), ncol = ncol(dat), byrow = T)
  diffs <- ifelse(vec_to_mat == dat, 0, 1)
  diffs[is.na(dat)] <- NA
  diffs
}

#' Calculates Differences for a Specified Player
#'
#' Compares statistics for a player of interest to a the statistics for a large
#' group of players.  Returns the distance from each player in the group to the
#' player of interest.
#' @param stats_fp data.frame of one row.  The statistics for the focus player.
#' @param stats_comps data.frame with identical columns to \code{stats_fp}.  The
#'   statistics for the group of players to be compared.  Each row corresponds
#'   to the statistics for a player. Differences will be calculated by comparing
#'   each row of \code{stats_comps} to \code{stats_fp}.
#' @param weights named vector of length \code{=ncol(stats_comps)} specifying a
#'   weight for each variable.
#' @return a vector of differences.  The jth element of the vector corresponds
#'   to the difference between \code{stats_fp} and the jth row of
#'   \code{stats_comps}.  Differences are calculated using Gower's distance
#'   formula.
#' @details \code{findDiffs2} separates the statistics into quantitative and
#'   categorical statistics.  It then passes the categorical statistics (for
#'   both the focus player and the comps) and the categorical weights to
#'   \code{\link{catDiffs2}} to calculate differences for categorical variables.
#'   It passes the quantitative statistics and weights to
#'   \code{\link{numDiffs2}} calculate differences for quantitative variables.
#'
#'   Using Gower's formula, distances for categorical variables between player
#'   \eqn{j} and the focus player are 0 if the players have the same value of the
#'   variable, and 1 if it's different.  Difference for the \eqn{i^{th}}
#'   quantitative variable for the \eqn{j^{th}} player are \eqn{d_{ij} = (stat_{ij} -
#'   stat_{i,fp}) / range(stat_i)}.
#'
#'   \code{findDiffs2} then records missing data as \eqn{m_{ij} = 0} if the \eqn{i^{th}}
#'   statistic for player \eqn{j} or for the focus player is missing.  Let
#'   \eqn{delta_{j}} be the distance between player \eqn{j} and the focus player and
#'   \eqn{w_i} be the weight for the \eqn{i^{th}} statistic (\eqn{i = 1...I}).  Using
#'   Gower's distance metric, \deqn{delta_j = sum(i=1:I; w_i m_{ij} d_{ij}) /
#'   sum(i=1:I; w_i d_{ij})}
#'
#' @references Gower, J. C. (1971) A general coefficient of similarity and some
#'   of its properties, Biometrics 27, 857 - 874.
#' @examples
#' ### Using the iris data
#' wts <- (1:5) %>% setNames(names(iris))
#' fp <- data.frame(5,4,3,2,"setosa") %>% setNames(names(iris))
#' i <- head(iris)
#' findDiffs2(fp, i, wts)
#' # ratio of weights are all that matters
#' wts <- wts / 10
#' findDiffs2(fp, i, wts) # same thing!
#' # columns not names in wts will be removed
#' wts <- wts[-4]
#' findDiffs2(fp, i, wts) # still works
#'
#' # can handle missing data!
#' i <- head(iris)
#' i[3,4] <- NA
#' findDiffs2(fp, i, wts)
findDiffs2 <- function(stats_fp, stats_comps, weights) {
  # determining which statistics are numeric
  num_stats <- sapply(stats_comps, function(x) {
    class(x) %in% c("numeric", "integer", "double")
  })
  # differences for numeric stats
  if(length(num_stats[num_stats == T]) > 0) {
    num_diffs <- numDiffs2(stats_fp[num_stats], stats_comps[num_stats])
  }
  if(length(num_stats[num_stats == F]) > 0) {
    cat_diffs <- catDiffs2(stats_fp[!num_stats], stats_comps[!num_stats])
  }
  # combining numeric and categorical differences into one matrix
  if(exists("num_diffs")) {
    if(exists("cat_diffs")) diffs <- cbind(num_diffs, cat_diffs)
    else diffs <- num_diffs
  } else {
    if(exists("cat_diffs")) diffs <- cat_diffs
    else diffs <- NULL
  }
  if(!is.null(diffs)) {
    diffs <- diffs[, names(weights)]
    # handling missing data
    miss_mat <- ifelse(is.na(diffs), 0, 1)
    diffs[is.na(diffs)] <- 0
    # calculating gower's distance
    weighted_diff <- (diffs %*% weights) / (miss_mat %*% weights)
  } else {
    weighted_diff <- NA
  }
  as.vector(weighted_diff)
}

#' Wrapper to Calculate Similarity Scores
#'
#' Uses the \code{\link{findDiffs2}} function to calculate similarity scores.
#' This is used internally by \code{\link{age_comps}}, and
#' \code{\link{level_comps}}.  This function should only be used inside these
#' two functions.
#' @param dat data.frame of player information.  Must be in proper format.
#' @param player name of player to find similarity scores for.
#' @param weights named vector.  The names are the stats for which we will be
#'   calculating similarity scores.  The values are the weight for each
#'   statistics to use when calculating similarity scores.
#' @return data.frame containing scores for each player
#' @references Gower, J. C. (1971) A general coefficient of similarity and some
#'   of its properties, Biometrics 27, 857 - 874.
calcScores2 <- function(dat, player, weights) {
  stats_fp <- dat %>% filter(name == player)
  # remove columns which aren't needed
  stats_fp <- stats_fp[, names(weights)]
  dat_to_compare <- dat[, names(weights)]
  # calculate scores
  diffs <- findDiffs2(stats_fp, dat_to_compare, weights)
  data.frame(mlbid = dat$mlbid,
             name = dat$name,
             score = round(1000 * (1 - diffs), 1)
  ) %>% tbl_df()
}

#' Use MLEs to Calculate Similarity Scores for Batters
#'
#' @param player character.  The name of the focus player, formatted as "Last;
#'   First M."
#' @param data  The dataset of MLEs.  Should be the \code{level_grp} dataset
#'   from `\code{simScoresApp/b-comparison-app/bat_simscore_data.RData}.
#' @param weights  Numeric, named vector.  The names tell which variables to use
#'   for comparisons.  The value tells the weight put on that variable when
#'   comparing.
#' @param years numeric vector of length 1 or 2.  If length 1, specifies how
#'   many years of statistcs to include.  If length two, specifies the age range
#'   over which to compare (inclusive).
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting.
#' @examples
#' load("H:/simScoresApp/b-comparison-app/bat_simscore_data.RData")
#' wts <- c(position = .05, pa = .9, bb.perc = .66, k.perc = .84, iso = .84, babip = .23, obp = .92)
#' # comparing stats based on three most recent years
#' age_comps("Brown; Domonic L.", age_grp, wts, 3, "bat")
#' # comparing stats from age 24 to 26
#' age_comps("Brown; Domonic L.", age_grp, wts, c(24, 26), "bat")
#' # same thing!
age_comps <- function(player, data, weights, years = 3, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  # identifying age range for comparison
  if(length(years) == 1) {
    fp <- filter(data, name == player)
    max_age <- max(fp$age)
    min_age <- max(max_age - years + 1, min(fp$age))
  } else {
    fp <- data %>% filter(name == player, age >= min(years), age <= max(years))
    assertthat::assert_that(nrow(fp) > 0)
    max_age <- max(fp$age)
    min_age <- min(fp$age)
  }
  # 1) filtering to specified timeframe
  comparison_data <- data %>% filter(age >= min_age, age <= max_age)
  # 2) & 3) summarising each player's stats then adding stats
  summed_data <- comparison_data %>%
    group_by(mlbid, name, height, weight, throws, roo)
  if(type == "bat") {
    summed_data <- summed_data %>%
      group_by(bats, add = TRUE) %>%
      summarise(
        years = paste(min(year), max(year), sep = ":"),
        position = position[which.max(pa)],
        experience = last(experience),
        dl.days = sum(dl.days),
        g = sum(g),
        pa = sum(pa),
        ab = sum(ab),
        x1b = sum(x1b),
        x2b = sum(x2b),
        x3b = sum(x3b),
        hr = sum(hr),
        h = sum(x1b + x2b + x3b + hr),
        bb = sum(bb + hbp),
        k = sum(k)
      ) %>%
      mutate(
        pa.g = pa / g,
        obp = (h + bb) / pa,
        bb.perc = bb / pa * 100,
        k.perc = k / pa * 100,
        iso = (x2b + 2*x3b + 3*hr) / (pa - bb),
        babip = (x1b + x2b + x3b) / (pa - bb - k - hr)
      ) %>%
      ungroup()
  } else {
    summed_data <- summed_data %>%
      summarise(
        years = paste(min(year), max(year), sep = ":"),
        position = position[which.max(ip)],
        experience = last(experience),
        dl.days = sum(dl.days),
        g = sum(g),
        ip = sum(ip),
        tbf = sum(tbf),
        sdt = sum(sdt),
        hr = sum(hr),
        bb = sum(bb + hbp),
        k = sum(k)
      ) %>%
      mutate(
        ip.g = ip / g,
        bb.9 = (bb) / ip * 9,
        babip = (sdt) / (tbf - hr - bb),
        k.9 = k / ip * 9,
        hr.9 = hr / ip * 9,
        whip = (sdt + hr + bb) / ip,
        fip = ( 13 * hr + 3 * bb - 2 * k ) / ip + 3.2
      ) %>%
      ungroup()
  }
  # 4) & 5) calculating scores
  scores <- calcScores2(summed_data, player, weights) %>%
    inner_join(summed_data %>% dplyr::select(mlbid, name, years)) %>%
    arrange(desc(score)) %>%
    dplyr::select(mlbid, name, years, score)
  # returning
  list(table = scores, max.age = max_age, min.age = min_age)
}

#' Similarity Scores Averaged Across Level
#'
#' @param player character.  The name of the focus player, formatted as "Last;
#'   First M."
#' @param data  The dataset of MLEs.  Should be the \code{level_grp} dataset
#'   from `\code{simScoresApp/b-comparison-app/bat_simscore_data.RData}.
#' @param weights  Numeric, named vector.  The names tell which variables to use
#'   for comparisons.  The value tells the weight put on that variable when
#'   comparing.
#' @param years numeric vector of length 1 or 2.  If length 1, specifies how
#'   many years of statistcs to include.  If length two, specifies the age range
#'   over which to compare (inclusive).
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting.
#' @examples
#' load("H:/simScoresApp/b-comparison-app/bat_simscore_data.RData")
#' wts <- c(position = .05, pa = .9, bb.perc = .66, k.perc = .84, iso = .84, babip = .23, obp = .92)
#' # comparing stats based on three most recent years
#' level_comps("Brown; Domonic L.", level_grp, wts, 3, "bat")
#' # comparing stats from age 24 to 26
#' level_comps("Brown; Domonic L.", level_grp, wts, c(24, 26), "bat")
#' # same thing!
level_comps <- function(player, data, weights, years = 3, type = c("bat", "pit")) {
  type <- match.arg(type, c("bat", "pit"))
  # identifying age range for comparison
  if(length(years) == 1) {
    fp <- filter(data, name == player)
    max_age <- max(fp$age)
    min_age <- max(max_age - years + 1, min(fp$age))
    fp <- fp %>% filter(age >= min_age, age <= max_age)
  } else {
    fp <- data %>% filter(name == player, age >= min(years), age <= max(years))
    assertthat::assert_that(nrow(fp) > 0)
    max_age <- max(fp$age)
    min_age <- min(fp$age)
    fp <- fp %>% filter(age >= min_age, age <= max_age)
  }
  # 1) filtering to specified timeframe
  comparison_data <- data %>%
    filter(age >= min_age, age <= max_age, level %in% fp$level)
  # 2) & 3) summarising each player's stats then adding stats
  summed_data <- comparison_data %>%
    group_by(mlbid, name, height, weight, throws, roo, level)
  if(type == "bat") {
    summed_data <- summed_data %>%
      group_by(bats, add = TRUE) %>%
      summarise(
        position = position[which.max(pa)],
        experience = last(experience),
        dl.days = sum(dl.days),
        g = sum(g),
        pa = sum(pa),
        ab = sum(ab),
        x1b = sum(x1b),
        x2b = sum(x2b),
        x3b = sum(x3b),
        hr = sum(hr),
        h = sum(x1b + x2b + x3b + hr),
        bb = sum(bb + hbp),
        k = sum(k)
      ) %>%
      mutate(
        pa.g = pa / g,
        obp = (h + bb) / pa,
        bb.perc = bb / pa * 100,
        k.perc = k / pa * 100,
        iso = (x2b + 2*x3b + 3*hr) / (pa - bb),
        babip = (x1b + x2b + x3b) / (pa - bb - k - hr)
      ) %>%
      ungroup()
    # focus player's PA by level
    level_wt <- summed_data %>%
      filter(name == player) %>%
      dplyr::select(level, pa) %>%
      setNames(c("level", "wt"))
  } else {
    summed_data <- summed_data %>%
      summarise(
        position = position[which.max(ip)],
        experience = last(experience),
        dl.days = sum(dl.days),
        g = sum(g),
        ip = sum(ip),
        tbf = sum(tbf),
        sdt = sum(sdt),
        hr = sum(hr),
        bb = sum(bb + hbp),
        k = sum(k)
      ) %>%
      mutate(
        ip.g = ip / g,
        bb.9 = (bb) / ip * 9,
        babip = (sdt) / (tbf - hr - bb),
        k.9 = k / ip * 9,
        hr.9 = hr / ip * 9,
        whip = (sdt + hr + bb) / ip,
        fip = ( 13 * hr + 3 * bb - 2 * k ) / ip + 3.2
      ) %>%
      ungroup()
    # focus player's IP by level
    level_wt <- summed_data %>%
      filter(name == player) %>%
      dplyr::select(level, ip) %>%
      setNames(c("level", "wt"))
  }
  # calculating scores
  scores <- summed_data %>%
    group_by(level) %>%
    do(calcScores2(., player, weights)) %>%
    ungroup() %>%
    tidyr::spread(level, score) %>%
    mutate_each(funs(x = ifelse(is.na(.), min(., na.rm = T), .)), -name) %>%
    tidyr::gather(level, score, -mlbid, -name) %>%
    inner_join(level_wt) %>%
    group_by(mlbid, name) %>%
    summarise(score = weighted.mean(score, wt)) %>%
    ungroup() %>%
    arrange(desc(score))
  # adding years column
  comparison_data %>%
    filter(mlbid %in% scores$mlbid) %>%
    group_by(mlbid) %>%
    summarise(years = paste(min(year), max(year), sep = ":")) %>%
    inner_join(scores) %>%
    dplyr::select(name, years, score) %>%
    arrange(desc(score))
}


