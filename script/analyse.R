rm(list=ls())
library(dplyr)
source('../lib/analysis-functions.R', chdir=T)

bug_data <- readRDS("../data/firefox-bug-data.rds")

###

# PRODUCTIVITY
#   fixes per day
#   reopens per day
#   backouts per day (TODO: create plots for early and late backout per day)
#   reviews- per day (TODO: create this plot)

{
  headers <- c("", "Q1", "median", "Q3", "traditional", "rapid", "significance")
  tab <- headers

  tab <- c("PRODUCTIVITY", "", "", "", "", "", "") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs fixed per day", bug_data, "time_first_fix", "fixes_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs negatively reviewed per day", bug_data, "time_first_review_minus", "review_minus_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs backed out per day", bug_data, "time_first_backout", "backouts_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs reopened per day", bug_data, "time_first_reopen", "reopens_per_day") %>% rbind(tab, .)

  tab <- c("EFFICACY", "", "", "", "", "", "") %>% rbind(tab, .)
    tab <- rowify_binary("  Negative review rate", bug_data, "time_first_review_ask", "has_review_minus") %>% rbind(tab, .)
    tab <- rowify_binary("  Backout rate", bug_data, "time_first_fix", "has_backout") %>% rbind(tab, .)
    tab <- rowify_binary("    Early backout rate", bug_data, "time_first_fix", "has_early_backout") %>% rbind(tab, .)
    tab <- rowify_binary("    Late backout rate", bug_data, "time_first_fix", "has_late_backout") %>% rbind(tab, .)
    tab <- rowify_binary("  Reopening rate", bug_data, "time_first_buildok", "has_reopen") %>% rbind(tab, .)

  tab <- c("EFFICIENCY", "", "", "", "", "", "") %>% rbind(tab, .)
  # time-to-fix
    tab <- rowify_continuous("  Time-to-ask review (days)", bug_data, 'time_create', 'days_to_review_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-fix (days)", bug_data, 'time_create', 'days_to_fix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-buildok (days)", bug_data, 'time_create', 'days_to_buildok') %>% rbind(tab, .)
    # TODO: diff between time-to-fix and time-to-buildok
  tab <- c("  -", "", "", "", "", "", "") %>% rbind(tab, .)
  # time-to-badfix
    tab <- rowify_continuous("  Time-to-bad review ask (days)", bug_data, 'time_create', 'days_to_badreview_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-badfix (days)", bug_data, 'time_create', 'days_to_badfix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-badbuildok (days)", bug_data, 'time_create', 'days_to_badbuildok') %>% rbind(tab, .)
  tab <- c("  ---", "", "", "", "", "", "") %>% rbind(tab, .)
  # time-to-refix
    tab <- rowify_continuous("  Time-to-rereview ask (days)", bug_data, 'time_first_review_minus', 'days_to_rereview_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-refix (days)", bug_data, 'time_first_backout', 'days_to_refix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-rebuildok (days)", bug_data, 'time_first_reopen', 'days_to_rebuildok') %>% rbind(tab, .)
  tab <- c("  ---", "", "", "", "", "", "") %>% rbind(tab, .)
  # time-to-reopen
    tab <- rowify_continuous("  Time-to-review- (days)", bug_data, 'time_first_review_ask', 'days_to_review_minus') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-review+ (days)", bug_data, 'time_first_review_ask', 'days_to_review_plus') %>% rbind(tab, .)

    tab <- rowify_continuous("  Time-to-backout (days)", bug_data, 'time_first_fix', 'days_to_backout') %>% rbind(tab, .)
    tab <- rowify_continuous("    Time-to-early backout (days)", bug_data, 'time_first_fix', 'days_to_early_backout') %>% rbind(tab, .)
    tab <- rowify_continuous("    Time-to-late backout (days)", bug_data, 'time_first_fix', 'days_to_late_backout') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-reopen (days)", bug_data, 'time_first_buildok', 'days_to_reopen') %>% rbind(tab, .)

  rownames(tab) <- NULL
  tab
}

# Sum time-to-fix
{
  badfix <- sum(bug_data$days_to_fix * bug_data$has_backout)
  goodfix <- sum(bug_data$days_to_fix * !bug_data$has_backout)
  mean(bug_data$has_backout, na.rm=T)
  badfix / (badfix + goodfix)

  # How much time is spent writing inappropriate fixes
  badreview_ask <- sum(bug_data$days_to_review_ask * bug_data$has_review_minus, na.rm=T)
  goodreview_ask <- sum(bug_data$days_to_review_ask * !bug_data$has_review_minus, na.rm=T)
  mean(bug_data$has_review_minus, na.rm=T)
  badreview_ask / (badreview_ask + goodreview_ask)

  # is second fix time positively correlated to time-to-withdraw?
  df <- subset(bug_data, !is.na(days_to_refix))
  cor.test(df$days_to_refix, df$days_to_backout)
  cor.test(df$days_to_refix, df$days_to_fix)
  # (same for second attached patch)
  df <- subset(bug_data, !is.na(days_to_rereview_ask) & !is.na(days_to_review_minus))
  cor.test(df$days_to_rereview_ask, df$days_to_review_minus)
  cor.test(df$days_to_rereview_ask, df$days_to_review_ask)

  # writing second fix takes much less time than writing the first one
  median(bug_data$days_to_refix / bug_data$days_to_fix, na.rm=T)
  boxplot(1+bug_data$days_to_fix, 1+bug_data$days_to_refix, log="y", names=c("fix", "refix"))
  #
  median(bug_data$days_to_rereview_ask / bug_data$days_to_review_ask, na.rm=T)
  #
  median(bug_data$days_to_rebuildok / bug_data$days_to_buildok, na.rm=T)
}

source('../lib/compute-month-groups.R')

###

#' # Facet 1: Productivity
#'

#' ## fixes per day

myplot(data_month_first_fix, "fixes_per_day", "month")

#' ## compare with number of active developers

a <- data_month_first_fix
a$month <- strftime(a$month, "%Y-%m")
# a$month
developers_month <- readRDS("../data/firefox-developers-month.rds")
x <- a %>% inner_join(developers_month, by="month")
x$month <- as.yearmon(x$month)
x$nfixerfactor <- x$n_fixers * 0.25
# myplot(x, "n_fixers", "month")
# mylines(data_month_first_fix, "fixes_per_day", "month", col=2)
myplot(data_month_first_fix, "fixes_per_day", "month")
mylines(x, "nfixerfactor", "month", col=2)

x$ratio <- x$fixes_per_day / x$n_fixers
myplot(x, "ratio", "month")
cor.test(x$fixes_per_day, x$n_fixers, method='spearman')

#' The number of fixes has been increasing, from about 15/day to about 40/day.
#' This trend is highly correlated with the increase in the number of programmers.

#'
#' ---------------------------------------------------------------------------
#'

#' ## reopens per day

myplot(data_month_first_reopen, "reopens_per_day", "month")

#' On average, one bug is reopened every day.
#' No significant change in number of reopens per day.

#'
#' ---------------------------------------------------------------------------
#'

#' ## backouts per day

myplot(data_month_first_backout, "backouts_per_day", "month")

#' On average, 1 to 4 bugs are backed out every day.
#' The number of backout per day increased signifcantly, from 1/day to 3 or 4/day.

#'
#' ---------------------------------------------------------------------------
#'

#' # Facet 2: Efficacy

#' ## reopening rate

myplot(data_month_first_fix, "reopen_rate", "month")

#' The reopening rate is decreasing.
#' Since bug reopening can only occur after a successful build, this result
#' may be an indicator that automated testing has become more effective,
#' improving early detection of problems.

#'
#' ---------------------------------------------------------------------------
#'

#' ## backout rate

myplot(data_month_first_fix, "backout_rate", "month")

#' About 8% of the fixes are eventually backed out.
#' The backout rate seems to have increased a little.
#' Let's break it down into early and late backout rate.

#'
#' ---------------------------------------------------------------------------
#'

#' ## early & late backout rate

myplot(data_month_first_fix, "early_backout_rate", "month")
mylines(data_month_first_fix, "late_backout_rate", "month", col=2)
legend("topleft", c("early", "late"), lwd=1, col=c(1,2))

#' The early backout rate increased, and the late backout rate decreased.
#'
#' The increase in **early backout rate** can be explained by
#' 
#' * better testing tools
#' * creation of sheriff-managed integration repositories
#'   * testing before commit is less comprehensive (cultural change)
#'
#' The increase in early backout rate does not cause significant overhead,
#' since it means that the inappropiate patch was backed out before getting
#' into mozilla-central, the repository from which developers base their patches.
#'
#' The decrease in **late backout rate** can be explained by better 
#' testing tools: automated testing detects more problems than before.
#'
#' The decrease in late backout rate comes with an overhead reduction, since
#' late backouts are associated with more severe impacts:
#'
#' * the longer the time-to-backout, the more difficult it is for developers to
#'   remember the bug fix context and set up their environments.
#' * with integration branches, it means that an inappropriate patch was merged
#'   into mozilla-central, disturbing developers' work.

#'
#' ---------------------------------------------------------------------------
#'

#' # Facet 3: Efficiency (time)

#' ## time-to-fix and variants

myplot(data_month_create, "median_days_to_buildok", "month")
mylines(data_month_first_reopen, "median_days_to_rebuildok", "month", col=2)
myplot(data_month_create, "median_days_to_fix", "month")
mylines(data_month_first_backout, "median_days_to_refix", "month", col=2)

myplot(data_month_first_backout, "median_p_days_to_refix", "month")
myplot(data_month_first_reopen, "median_p_days_to_rebuildok", "month")

#' The time to create a fix has reduced. TODO: why? so what?
#'
#' Discussion: with short releases, do developers rush less, since the next 
#' release is only 6 weeks away?
#'
#' The second fix is about 85% faster than the first one.
#' The ratio between time-to-first-fix and time-to-second-fix is reducing?

#'
#' ---------------------------------------------------------------------------
#'

#' ## time-to-bad-fix

myplot(data_month_create, "median_days_to_badfix", "month")
mylines(data_month_create, "median_days_to_fix", "month", col=2)
legend("topleft", c("bad fix", "fix"), lwd=1, col=c(1,2))
myplot(data_month_create, "median_p_days_to_badfix", "month")

#' A bad fix takes longer 2x to 3x longer.
#' Bad fix-time is reducing.'

#' ## time-to-bad-buildok'

myplot(data_month_create, "median_days_to_badbuildok", "month")
mylines(data_month_create, "median_days_to_buildok", "month", col=2)
legend("topleft", c("bad buildok", "buildok"), lwd=1, col=c(1,2))
myplot(data_month_create, "median_p_days_to_badbuildok", "month")


#'
#' ---------------------------------------------------------------------------
#'

#' ## time-to-reopen and time-to-backout

myplot(data_month_first_buildok, "median_days_to_reopen", "month")
myplot(data_month_first_fix, "median_days_to_backout", "month")

#' A typical bug takes about 5 days to be backed out (if it is ever backed out)
#' 
#' There was a spike in time-to-backout in late-2010/early-2011.

#'
#' ---------------------------------------------------------------------------
#'

#' # Reviews

myplot(data_month_first_review_ask, "review_ask_rate", "month")
myplot(data_month_first_review_ask, "review_plus_rate", "month")
myplot(data_month_first_review_ask, "review_minus_rate", "month")

#' All bug fixes are reviewed. Almost all receive a review+.
#' Between 10 and 15% ever receive a review-. The number seems to be reducing.

myplot(data_month_first_review_ask, "median_days_to_review_minus", "month")
mylines(data_month_first_review_ask, "median_days_to_review_plus", "month", col=2)
legend("topleft", c("review-", "review+"), lwd=1, col=c(1,2))

#' Negative reviews take longer.

#' # Correlations

x <- data_month_first_fix %>%
  select(median_days_to_buildok, median_days_to_fix, median_days_to_reopen, median_days_to_backout,
    median_days_to_rebuildok, median_days_to_refix,
    reopen_count, backout_count, early_backout_count, late_backout_count)
library(PerformanceAnalytics)
chart.Correlation(x, method='spearman')

a <- subset(bug_data, !is.na(days_to_refix) & !is.na(days_to_backout))
with(a, cor.test(days_to_refix, days_to_backout), method='spearman')
with(a, plot(y=days_to_refix, x=days_to_backout))
#
a <- subset(bug_data, !is.na(days_to_rebuildok) & !is.na(days_to_reopen))
with(a, cor.test(days_to_rebuildok, days_to_reopen), method='spearman')
with(a, plot(y=days_to_rebuildok, x=days_to_reopen))

# Associations

x <- bug_data %>%
  select(has_backout, has_reopen, has_review_minus, has_early_backout, has_late_backout)
library(vcd)
mosaic(~ has_review_minus + has_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_early_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_late_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_reopen, data=bug_data, direction='v', shade=T)
fisher.test(xtabs(~ has_review_minus + has_backout, data=bug_data))

#' There's an association between review- and backout/reopen

# hist(bug_data$days_to_buildok)
# hist(bug_data$days_to_reopen)
# hist(bug_data$days_to_fix)
# hist(bug_data$days_to_backout)
