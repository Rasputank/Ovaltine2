#' Predict Aging Curve
#'
#' Predicts the aging curve for a sequence of ages using the fixed effect
#' coefficients from a fitted model
#' @param ages numeric.  The ages for which to estimate the player's aging
#'   curve.
#' @param coefs  The coefficients from the fitted model.  Can be either a vector
#'   or a matrix where each column consists of a different set of coefficients.
#' @param poly_model the model used to decompose the \code{ages} vector into
#'   orthogonal polynomials.  Must have degree equal to the number of rows in
#'   the \code{coefs} matrix or, if \code{coefs} is a vector, the length of
#'   \code{coefs}.
#' @return data.frame containing the ages and the estimated aging curve for each
#'   age.
pred_curve <- function(ages, coefs, poly_model) {
  orthog_ages <- cbind(1, predict(poly_model, ages))
  orthog_ages %*% as.matrix(coefs) %>%
    data.frame() %>%
    tbl_df() %>%
    mutate(age = ages)
}
#' Simulate the Aging Curve of a Focus Player
#'
#' Models the aging curve for a focus player based on his performance, and the
#' performance of his comparables.  Then simulates the fitted curve so that it
#' can be plotted by \code{\link{plot_age_curve}}.
#' @param fp name of focus player.  Must be in the format "Last; First M."
#' @param comps object obtained from \code{\link{age_comps}}.
#' @param dat statistics for comps.  Obtained from \code{\link{sum_stats}}.
#' @param ages_pred numeric of length two in the form (lower, upper).  The ages
#'   to simulate the focus players aging curve.
#' @param type character.  Whether these are batting or pitching data.  Defaults
#'   to batting.
#' @param wt_power A single number which specifies the weights to use when
#'   fitting the model.  The weights are based off the similarity score for each
#'   player in the \code{comps} object.  Weights are calculated as (score /
#'   1000) ^ wt_power so setting \code{wt_power = 0} weights all players
#'   equally.
#' @param ncomps The number of comparable players to used when modelling the
#'   focus player's aging curve.  Defaults to 10.  The focus player is also used
#'   in the model, so the total number of unique players included in the model
#'   is \code{ncomps} + 1.
#' @param nsim The number of simulations to run.  Defaults to 500.
#' @param simplify boolean.  Whether to use a simpler model where only the
#'   intercept varies by player (TRUE) or a more complex model where the all
#'   terms vary by player (FALSE).  Defaults to FALSE.
#' @param model_ages numeric of length two.  The range of ages to use when
#'   fitting the model (inclusive).  For example, it may not make sense to use
#'   information from Albert Pujols's age 20 season because he is currently 34.
#'   Defaults to 18-49 (so basically everything is included.
#' @param stat character of length one.  The statistic for which the focus
#'   player's aging curve should be simulated.  If \code{type = "bat"}, must be
#'   one of \code{woba, iso, k, bb}.  If \code{type = "pit"}, must be one of
#'   \code{k, bb, fip, hr}.  Formulas for wOBA and FIP are obtained from the
#'   Fangraphs Library.  The formula for FIP has been modified for simplicity.
#'   Instead of calculating the constant, the value of 3.2 is used.
#' @details This function calculates the statistic of interest (wOBA, ISO, ...)
#'   for the focus player and the top \code{ncomps} comparables (ranked by
#'   similarity score) for each age specified by \code{model_ages}.  It then
#'   decomposes the ages for each observation into orthogonal polynomials of
#'   degree 2 using the \code{\link{poly}} function.
#'
#'   It then fits a multilevel model where age is used to explain the specified
#'   statistics.  Because age has been decomposed into an orthogonal polynomial
#'   of degree 2, this is a quadratic model for aging.  This model was inspired
#'   by a paper by Jim Albert; see the References section for details. If
#'   \code{simplify = FALSE}, then the intercept, linear, and quadratic terms
#'   are modeled as varying by player.  If \code{simplify = TRUE}, then only the
#'   intercept varies by player.  For more details on this model, see the
#'   "Calculating Aging Curves" vignette by typing
#'   \code{vignette("aging-curve")} in R.
#'
#'   Finally, simulations of the fixed effects part of the model are drawn using
#'   the \code{\link{sim}} function from the \code{arm} package.
#' @return A list of length four.  The first three elements are the data for the
#'   focus player, the estimated aging trend, and the simulated aging trends.
#'   These are used in \code{\link{plot_age_curve}}.  The fourth element,
#'   warnings, is boolean telling whether the \code{\link{lmer}} function threw
#'   any warnings when fitting the model.  If \code{warnings} is \code{TRUE},
#'   the model should be refit before plotting.
#' @references For FIP formula: FIP.  FanGraphs Glossary.  Fangraphs.
#'   \url{http://www.fangraphs.com/library/pitching/fip/}.  Accessed 11/24/2014.
#'
#'   For wOBA formula: wOBA.  FanGraphs Glossary.  Fangraphs.
#'   \url{http://www.fangraphs.com/library/offense/woba/}.  Accessed 11/24/2014.
#'
#'   For aging curve model: Albert, Jim. "Smoothing career trajectories of
#'   baseball hitters." Manuscript, Bowling Green State University (2002).
#'
#'   For multilevel models and simulating coefficients: Andrew Gelman and
#'   Yu-Sung Su (2014). arm: Data Analysis Using Regression and
#'   Multilevel/Hierarchical Models. R package version 1.7-07.
#'   \url{http://CRAN.R-project.org/package=arm}
#' @seealso \code{\link{lme4}} for the function used to fit multilevel models,
#'   \code{\link{sim}} for simulating regression coeffeicients from a fitted
#'   model.
#' @examples
#' load("H:/simScoresApp/b-comparison-app/bat_simscore_data.RData")
#' wts <- c(position = .05, pa = .9, bb.perc = .66, k.perc = .84, iso = .84, babip = .23, obp = .92)
#' f <- age_comps("Brown; Domonic L.", age_grp, wts)
#' x <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
#'                    type = "bat", stat = "woba")
#' ## records if lmer throws a warning
#' x$warnings # suggests a problem
#' x1 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
#'                     type = "bat", stat = "woba", wt_power = 500)
#' x1$warnings # so no serious problems
#' ## simplifying
#' x2 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
#'                    type = "bat", stat = "woba", simplify = T)
#' x2$warnings # may be a better approach
#'
#' ## pitchers
#' load("H:/simScoresApp/p-comparison-app/pit_simscore_data.RData")
#' wts2 <- c(throws = .01, ip = .62, k.9 = .83, bb.9 = .83, hr.9 = .91, babip = .55, fip = .35)
#' m <- age_comps("Buchanan; David A.", age_grp, wts2, type = "pit")
#' b <- sim_age_curve("Buchanan; David A.", m, age_grp, c(23, 28),
#'                    type = "pit", stat = "fip")
#' b$warnings #Hooray!
sim_age_curve <- function(fp, comps, dat, ages_pred, type = c("bat", "pit"),
                          wt_power = 30, ncomps = 10, nsim = 500,
                          simplify = FALSE, model_ages = c(18, 49),
                          stat = c("woba", "iso", "k", "bb", "fip", "hr")) {
  type <- match.arg(type, c("bat", "pit"))
  stat <- match.arg(stat, c("woba", "iso", "k", "bb", "fip", "hr"))
  if(type == "bat") stat <- match.arg(stat, c("woba", "iso", "k", "bb"))
  else stat <- match.arg(stat, c("fip", "hr", "k", "bb"))
  ages <- seq(min(ages_pred), max(ages_pred), by = .1)
  # selecting comps
  stats <- comps$table %>%
    slice(1:(ncomps + 1)) %>%
    inner_join(dat)
  # calculating advanced stats
  if(type == "bat") {
    stats <- stats %>%
      mutate(woba = (.69*bb + .722*hbp + .888*x1b + 1.271*x2b + 1.616*x3b + 2.101*hr) / pa,
             iso = (x2b + 2*x3b + 3*hr) / (pa - bb - hr - hbp),
             bbrate = bb / pa * 100,
             krate = k / pa * 100) %>%
      dplyr::select(mlbid, name, age, score, woba:krate)
  } else {
    stats <- stats %>%
      mutate(fip = (13*hr + 3*(bb+hbp) - 2*k)/ip + 3.2,
             bbrate = bb / tbf * 100,
             krate = k / tbf * 100,
             hrrate = hr / ip * 9) %>%
      dplyr::select(mlbid, name, age, score, fip:hrrate)
  }
  # constructing orthogonal polynomial of degree 2
  orthog_age <- poly(stats$age, degree = 2)
  model_data <- orthog_age %>%
    data.frame() %>%
    setNames(c("a1", "a2")) %>%
    cbind(stats, .) %>%
    tbl_df() %>%
    filter(age >= min(model_ages), age <= max(model_ages))
  # modelling
  if(simplify) {
    if(stat == "woba") {
      mod <- countWarnings(
        lme4::lmer(woba ~ a1 + a2 + (1| mlbid),
                   data = model_data,
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "iso") {
      mod <- countWarnings(
        lme4::lmer(iso ~ a1 + a2 + (1| mlbid),
                   data = model_data %>% filter(iso > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "bb") {
      mod <- countWarnings(
        lme4::lmer(bbrate ~ a1 + a2 + (1| mlbid),
                   data = model_data %>% filter(bbrate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "k") {
      mod <- countWarnings(
        lme4::lmer(krate ~ a1 + a2 + (1| mlbid),
                   data = model_data %>% filter(krate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "fip") {
      mod <- countWarnings(
        lme4::lmer(fip ~ a1 + a2 + (1| mlbid),
                   data = model_data,
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "hr") {
      mod <- countWarnings(
        lme4::lmer(hrrate ~ a1 + a2 + (1| mlbid),
                   data = model_data %>% filter(hrrate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    }
  } else {
    if(stat == "woba") {
      mod <- countWarnings(
        lme4::lmer(woba ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data,
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "iso") {
      mod <- countWarnings(
        lme4::lmer(iso ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data %>% filter(iso > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "bb") {
      mod <- countWarnings(
        lme4::lmer(bbrate ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data %>% filter(bbrate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "k") {
      mod <- countWarnings(
        lme4::lmer(krate ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data %>% filter(krate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "fip") {
      mod <- countWarnings(
        lme4::lmer(fip ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data,
                   weights = (score / 1000) ^ wt_power)
      )
    } else if(stat == "hr") {
      mod <- countWarnings(
        lme4::lmer(hrrate ~ a1 + a2 + (1 + a1 + a2 | mlbid),
                   data = model_data %>% filter(hrrate > 0),
                   weights = (score / 1000) ^ wt_power)
      )
    }
  }
  sims <- arm::sim(mod$result, nsim)
  # returning
  fp_dat <- stats %>%
    filter(name == fp, age >= min(ages_pred), age <= max(ages_pred))
  avg_curve <- pred_curve(ages, fixef(mod$result), orthog_age) %>%
    setNames(c("pred", "age"))
  sim_curves <- pred_curve(ages, t(sims@fixef), orthog_age) %>%
    tidyr::gather(sim, pred, -age)
  list(fp = fp_dat, avg = avg_curve, sims = sim_curves, warnings = ifelse(mod$n > 0, T, F))
}

#' Plots Simulated Aging Curves
#'
#' Uses the results from \code{\link{sim_aging_curve}} to plot the focus
#' player's performance, the estimated aging curve from the model, and the
#' simulated aging curves from the mode.
#' @param curves list returned from \code{\link{sim_aging_curve}}
#' @param nsim The number of simulations that were run
#' @param fp The name of the focus player
#' @param stat character of length one.  The statistic for which the focus
#'   player's aging curve has been simulated.
#' @examples
#' load("H:/simScoresApp/b-comparison-app/bat_simscore_data.RData")
#' wts <- c(position = .05, pa = .9, bb.perc = .66, k.perc = .84, iso = .84, babip = .23, obp = .92)
#' f <- age_comps("Brown; Domonic L.", age_grp, wts)
#' x1 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "woba", wt_power = 500)
#' p1 <- plot_age_curve(x1, 500, "Brown; Domonic L.", "woba")
#' p1 # not good - curve makes no sense
#'
#' ## let's try using more players -> increase ncomps
#' x2 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "woba",
#'                     wt_power = 500, ncomps = 30)
#' x2$warnings # no warnings - that's good
#' p2 <- plot_age_curve(x2, 500, "Brown; Domonic L.", "woba")
#' p2 # plot still doesn't make much sense though
#'
#' ## maybe we should weigh top comparables less
#' x3 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "woba",
#'                     wt_power = 10, ncomps = 30)
#' x3$warnings # no warnings - still good
#' p3 <- plot_age_curve(x3, 500, "Brown; Domonic L.", "woba")
#' p3 # much better!
#'
#' ## plotting ISO
#' x4 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "iso",
#'                     wt_power = 0, ncomps = 30)
#' p4 <- plot_age_curve(x4, 500, "Brown; Domonic L.", "iso")
#' p4
#'
#' ## plotting K%
#' x5 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "k",
#'                     wt_power = 0, ncomps = 30)
#' p5 <- plot_age_curve(x5, 500, "Brown; Domonic L.", "k")
#' p5
#'
#' ## plotting BB%
#' x6 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21,30),
#'                     type = "bat", stat = "bb",
#'                     wt_power = 0, ncomps = 30)
#' p6 <- plot_age_curve(x6, 500, "Brown; Domonic L.", "bb")
#' p6 # this curve doesn't make much sense
#' x6$warnings # just because there aren't warnings, doesn't imply no problems
plot_age_curve <- function(curves, nsim, fp,
                           stat = c("woba", "iso", "k", "bb", "fip", "hr")) {
  stat <- match.arg(stat, c("woba", "iso", "k", "bb", "fip", "hr"))
  my_plot <- ggplot2::ggplot(curves$sims, ggplot2::aes(x = age, y = pred)) +
    ggplot2::geom_line(ggplot2::aes(group = sim), alpha = I(.05)) +
    ggplot2::geom_line(data = curves$avg, colour = "#284898", size = I(1.1)) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Age") + ggplot2::ggtitle(paste(fp, "Aging Curve"))
  if(stat == "woba") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = woba),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("wOBA")
  } else if(stat == "iso") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = iso),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("ISO")
  } else if(stat == "k") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = krate),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("K%")
  } else if(stat == "bb") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = bbrate),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("BB%")
  } else if(stat == "fip") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = fip),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("FIP")
  } else if(stat == "hr") {
    my_plot <- my_plot +
      ggplot2::geom_line(data = curves$fp, ggplot2::aes(y = hrrate),
                         colour = "#E81828", size = I(1.1)) +
      ggplot2::ylab("HR/9")
  }
  my_plot
}
