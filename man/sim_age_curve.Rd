% Generated by roxygen2 (4.0.2.9000): do not edit by hand
\name{sim_age_curve}
\alias{sim_age_curve}
\title{Simulate the Aging Curve of a Focus Player}
\usage{
sim_age_curve(fp, comps, dat, ages_pred, type = c("bat", "pit"),
  wt_power = 30, ncomps = 10, nsim = 500, simplify = FALSE,
  model_ages = c(18, 49), stat = c("woba", "iso", "k", "bb", "fip", "hr"))
}
\arguments{
\item{fp}{name of focus player.  Must be in the format "Last; First M."}

\item{comps}{object obtained from \code{\link{age_comps}}.}

\item{dat}{statistics for comps.  Obtained from \code{\link{sum_stats}}.}

\item{ages_pred}{numeric of length two in the form (lower, upper).  The ages
to simulate the focus players aging curve.}

\item{type}{character.  Whether these are batting or pitching data.  Defaults
to batting.}

\item{wt_power}{A single number which specifies the weights to use when
fitting the model.  The weights are based off the similarity score for each
player in the \code{comps} object.  Weights are calculated as (score /
1000) ^ wt_power so setting \code{wt_power = 0} weights all players
equally.}

\item{ncomps}{The number of comparable players to used when modelling the
focus player's aging curve.  Defaults to 10.  The focus player is also used
in the model, so the total number of unique players included in the model
is \code{ncomps} + 1.}

\item{nsim}{The number of simulations to run.  Defaults to 500.}

\item{simplify}{boolean.  Whether to use a simpler model where only the
intercept varies by player (TRUE) or a more complex model where the all
terms vary by player (FALSE).  Defaults to FALSE.}

\item{model_ages}{numeric of length two.  The range of ages to use when
fitting the model (inclusive).  For example, it may not make sense to use
information from Albert Pujols's age 20 season because he is currently 34.
Defaults to 18-49 (so basically everything is included.}

\item{stat}{character of length one.  The statistic for which the focus
player's aging curve should be simulated.  If \code{type = "bat"}, must be
one of \code{woba, iso, k, bb}.  If \code{type = "pit"}, must be one of
\code{k, bb, fip, hr}.  Formulas for wOBA and FIP are obtained from the
Fangraphs Library.  The formula for FIP has been modified for simplicity.
Instead of calculating the constant, the value of 3.2 is used.}
}
\value{
A list of length four.  The first three elements are the data for the
  focus player, the estimated aging trend, and the simulated aging trends.
  These are used in \code{\link{plot_age_curve}}.  The fourth element,
  warnings, is boolean telling whether the \code{\link{lmer}} function threw
  any warnings when fitting the model.  If \code{warnings} is \code{TRUE},
  the model should be refit before plotting.
}
\description{
Models the aging curve for a focus player based on his performance, and the
performance of his comparables.  Then simulates the fitted curve so that it
can be plotted by \code{\link{plot_age_curve}}.
}
\details{
This function calculates the statistic of interest (wOBA, ISO, ...)
  for the focus player and the top \code{ncomps} comparables (ranked by
  similarity score) for each age specified by \code{model_ages}.  It then
  decomposes the ages for each observation into orthogonal polynomials of
  degree 2 using the \code{\link{poly}} function.

  It then fits a multilevel model where age is used to explain the specified
  statistics.  Because age has been decomposed into an orthogonal polynomial
  of degree 2, this is a quadratic model for aging.  This model was inspired
  by a paper by Jim Albert; see the References section for details. If
  \code{simplify = FALSE}, then the intercept, linear, and quadratic terms
  are modeled as varying by player.  If \code{simplify = TRUE}, then only the
  intercept varies by player.  For more details on this model, see the
  "Calculating Aging Curves" vignette by typing
  \code{vignette("aging-curve")} in R.

  Finally, simulations of the fixed effects part of the model are drawn using
  the \code{\link{sim}} function from the \code{arm} package.
}
\examples{
load("H:/simScoresApp/b-comparison-app/bat_simscore_data.RData")
wts <- c(position = .05, pa = .9, bb.perc = .66, k.perc = .84, iso = .84, babip = .23, obp = .92)
f <- age_comps("Brown; Domonic L.", age_grp, wts)
x <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
                   type = "bat", stat = "woba")
## records if lmer throws a warning
x$warnings # suggests a problem
x1 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
                    type = "bat", stat = "woba", wt_power = 500)
x1$warnings # so no serious problems
## simplifying
x2 <- sim_age_curve("Brown; Domonic L.", f, age_grp, c(21:30),
                   type = "bat", stat = "woba", simplify = T)
x2$warnings # may be a better approach

## pitchers
load("H:/simScoresApp/p-comparison-app/pit_simscore_data.RData")
wts2 <- c(throws = .01, ip = .62, k.9 = .83, bb.9 = .83, hr.9 = .91, babip = .55, fip = .35)
m <- age_comps("Buchanan; David A.", age_grp, wts2, type = "pit")
b <- sim_age_curve("Buchanan; David A.", m, age_grp, c(23, 28),
                   type = "pit", stat = "fip")
b$warnings #Hooray!
}
\references{
For FIP formula: FIP.  FanGraphs Glossary.  Fangraphs.
  \url{http://www.fangraphs.com/library/pitching/fip/}.  Accessed 11/24/2014.

  For wOBA formula: wOBA.  FanGraphs Glossary.  Fangraphs.
  \url{http://www.fangraphs.com/library/offense/woba/}.  Accessed 11/24/2014.

  For aging curve model: Albert, Jim. "Smoothing career trajectories of
  baseball hitters." Manuscript, Bowling Green State University (2002).

  For multilevel models and simulating coefficients: Andrew Gelman and
  Yu-Sung Su (2014). arm: Data Analysis Using Regression and
  Multilevel/Hierarchical Models. R package version 1.7-07.
  \url{http://CRAN.R-project.org/package=arm}
}
\seealso{
\code{\link{lme4}} for the function used to fit multilevel models,
  \code{\link{sim}} for simulating regression coeffeicients from a fitted
  model.
}

