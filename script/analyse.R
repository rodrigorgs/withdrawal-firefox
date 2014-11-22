rm(list=ls())
library(dplyr)
library(Hmisc)
library(zoo)
source('../lib/na.R')

bug_data <- readRDS("../data/firefox-bug-data.rds")

###

#
# `groupped_bug_data`: the result of 
#
#     bug_data %>% group_by(some_criteria...) %>% mutate(num_days = ...)
#
# Please note that we require a column called `num_days` with the number of
# days within each group, or a column called `month` in the format "2014-12".
#
compute_summary <- function(groupped_bug_data) {
  ret <- groupped_bug_data %>%
    summarise(
      fixes = n(),
      num_days = ifelse(
        exists("num_days", groupped_bug_data),
        max(num_days),
        sum(monthDays(unique(as.Date(paste0(month, "-01")))))
        ),
      fixes_per_day = fixes / num_days,
      #
      reopen_count = sum(has_reopen),
      reopen_rate = mean(has_reopen),
      reopens_per_day = reopen_count / num_days,
      #
      backout_count = sum(has_backout),
      backout_rate = mean(has_backout),
      backouts_per_day = backout_count / num_days,
      #
      early_backout_count = sum(has_early_backout),
      early_backout_rate = mean(has_early_backout),
      early_backouts_per_day = early_backout_count / num_days,
      #
      late_backout_count = sum(has_late_backout),
      late_backout_rate = mean(has_late_backout),
      late_backouts_per_day = late_backout_count / num_days,
      #
      review_minus_count = sum(has_review_minus),
      review_minus_rate = sum(has_review_ask & has_review_minus) / sum(has_review_ask),
      review_plus_rate = sum(has_review_ask & has_review_plus) / sum(has_review_ask),
      review_ask_rate = mean(has_review_ask),
      #
      median_hours_to_fix = median(hours_to_fix, na.rm=T),
      median_hours_to_refix = median(hours_to_refix, na.rm=T),
      median_p_hours_to_refix = median(hours_to_refix / hours_to_fix, na.rm=T),
      median_hours_to_backout = median(hours_to_backout, na.rm=T),
      #
      median_hours_to_reopen = median(hours_to_reopen, na.rm=T),
      median_hours_to_buildok = median(hours_to_buildok, na.rm=T),
      median_p_hours_to_rebuildok = median(hours_to_rebuildok / hours_to_buildok, na.rm=T),
      median_hours_to_rebuildok = median(hours_to_rebuildok, na.rm=T),
      #
      # Bad fix etc.
      median_hours_to_badfix = median(ifelse(has_backout, hours_to_fix, NA), na.rm=T),
      median_p_hours_to_badfix = median_hours_to_badfix / median_hours_to_fix,
      #
      median_hours_to_badbuildok = median(ifelse(has_reopen, hours_to_buildok, NA), na.rm=T),
      median_p_hours_to_badbuildok = median_hours_to_badbuildok / median_hours_to_buildok,
      #
      median_hours_to_badbuildok = median(ifelse(has_reopen, hours_to_buildok, NA), na.rm=T),
      median_p_hours_to_badbuildok = median_hours_to_badbuildok / median_hours_to_buildok,      
      #
      # review
      median_hours_to_review_plus = median(hours_to_review_plus, na.rm=T),
      median_hours_to_review_minus = median(hours_to_review_minus, na.rm=T),
      #
      #
      mean_hours_to_fix = mean(hours_to_fix, na.rm=T),
      mean_hours_to_backout = mean(hours_to_backout, na.rm=T),
      mean_hours_to_reopen = mean(hours_to_reopen, na.rm=T),
      mean_hours_to_buildok = mean(hours_to_buildok, na.rm=T))

  invisible(ret)
}

between <- function(x, min, max) {
  x >= min & x <= max
}
group_by_release_type <- function(df) {
  ret <- df %>%
    mutate(
      month = strftime(time, "%Y-%m"),
      release_type = ifelse(
        month %>% between('2009-02', '2011-02'), 'planned',
        ifelse(month %>% between('2011-07', '2013-07'), 'rapid',
        NA))) %>%
    group_by(release_type) %>%
    filter(!is.na(release_type))
  invisible(ret)
}

# {
#   data_release_first_fix <- bug_data %>% mutate(time = time_first_fix) %>%
#     group_by_release_type() %>% compute_summary()
#   data_release_first_buildok <- bug_data %>% mutate(time = time_first_buildok) %>%
#     group_by_release_type() %>% compute_summary()
#   data_release_first_reopen <- bug_data %>% mutate(time = time_first_reopen) %>%
#     group_by_release_type() %>% compute_summary()
#   data_release_first_backout <- bug_data %>% mutate(time = time_first_backout) %>%
#     group_by_release_type() %>% compute_summary()
#   data_release_first_review_ask <- bug_data %>% mutate(time = time_first_review_ask) %>%
#     group_by_release_type() %>% compute_summary()
#   data_release_create <- bug_data %>% mutate(time = time_create) %>%
#     group_by_release_type() %>% compute_summary()
# }


stars_for_pvalue <- function(pvalue) {
  stars <- ""
  if (pvalue < 0.001) {
    stars <- "***"
  } else if (pvalue < 0.01) {
    stars <- "**"
  } else if (pvalue < 0.05) {
    stars <- "*"
  }
  stars
}
rowify_binary <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type()
  planned <- z[z$release_type == 'planned',][[column]]
  rapid <- z[z$release_type == 'rapid',][[column]]
  pvalue <- fisher.test(z$release_type, z[[column]])$p.value

  strvalues <- c(
    title,
    sprintf("%.2f%%", mean(planned, na.rm=T) * 100),
    sprintf("%.2f%%", mean(rapid, na.rm=T) * 100),
    stars_for_pvalue(pvalue))
  strvalues
}
rowify_continuous <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type()
  planned <- z[z$release_type == 'planned',][[column]]
  rapid <- z[z$release_type == 'rapid',][[column]]
  pvalue <- wilcox.test(planned, rapid)$p.value

  strvalues <- c(
    title,
    sprintf("%.2f", median(planned, na.rm=T)),
    sprintf("%.2f", median(rapid, na.rm=T)),
    stars_for_pvalue(pvalue)
    )
  strvalues
}

rowify_binary("Reopening rate", bug_data, "time_first_buildok", "has_reopen")
rowify_binary("Backout rate", bug_data, "time_first_fix", "has_backout")
rowify_binary("Early backout rate", bug_data, "time_first_fix", "has_early_backout")
rowify_binary("Late backout rate", bug_data, "time_first_fix", "has_late_backout")
rowify_binary("Review- rate", bug_data, "time_first_review_ask", "has_review_minus")

# time-to-fix
rowify_continuous("Time-to-fix (hours)", bug_data, 'time_create', 'hours_to_fix')
rowify_continuous("Time-to-buildok (hours)", bug_data, 'time_create', 'hours_to_buildok')
# time-to-refix
rowify_continuous("Time-to-refix (hours)", bug_data, 'time_first_backout', 'hours_to_refix')
rowify_continuous("Time-to-rebuildok (hours)", bug_data, 'time_first_reopen', 'hours_to_rebuildok')
# time-to-badfix#
### TODO: compute hours_to_badfix in aggregate)
# rowify_continuous("Time-to-badfix (hours)", bug_data, 'time_create', 'hours_to_badfix')
# rowify_continuous("Time-to-badbuildok (hours)", bug_data, 'time_create', 'hours_to_badbuildok')

# time-to-reopen
rowify_continuous("Time-to-backout (hours)", bug_data, 'time_first_fix', 'hours_to_backout')
rowify_continuous("Time-to-reopen (hours)", bug_data, 'time_first_buildok', 'hours_to_reopen')
rowify_continuous("Time-to-review- (hours)", bug_data, 'time_first_review_ask', 'hours_to_review_minus')

{
  min_date <- '2009-01-01'
  max_date <- '2013-07-01'

  data_month_first_fix <- bug_data %>%
    filter(time_first_fix >= min_date & time_first_fix < max_date) %>%
    mutate(month = strftime(time_first_fix, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_buildok <- bug_data %>%
    filter(time_first_buildok >= min_date & time_first_buildok < max_date) %>%
    mutate(month = strftime(time_first_buildok, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_reopen <- bug_data %>%
    filter(time_first_reopen >= min_date & time_first_reopen < max_date) %>%
    mutate(month = strftime(time_first_reopen, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))
  
  data_month_first_backout <- bug_data %>%
    filter(time_first_backout >= min_date & time_first_backout < max_date) %>%
    mutate(month = strftime(time_first_backout, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_create <- bug_data %>%
    filter(time_create >= min_date & time_create < max_date) %>%
    mutate(month = strftime(time_create, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_review_ask <- bug_data %>%
    filter(time_first_review_ask >= min_date & time_first_review_ask < max_date) %>%
    mutate(month = strftime(time_first_review_ask, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))
}

###

myplot <- function(data, y, x, ylim=NULL, ...) {
  if (is.null(ylim)) {
    ylim <- c(0, max(data[[y]]))
  }

  plot(data[[x]], data[[y]], type='l', ylim=ylim, xlab=x, ylab=y, ...)

  lo <- loess(data[[y]] ~ seq(data[[x]]))
  lines(y=predict(lo), x=data[[x]], lwd=0.5, lty=2, ...)
}
mylines <- function(data, y, x, ...) {
  lines(data[[x]], data[[y]], ...)
  lo <- loess(data[[y]] ~ seq(data[[x]]))
  lines(y=predict(lo), x=data[[x]], lwd=0.5, lty=2, ...)
}

#' # Facet 1: Productivity
#'

#' ## fixes per day

myplot(data_month_first_fix, "fixes_per_day", "month")

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

myplot(data_month_create, "median_hours_to_buildok", "month")
mylines(data_month_first_reopen, "median_hours_to_rebuildok", "month", col=2)
myplot(data_month_create, "median_hours_to_fix", "month")
mylines(data_month_first_backout, "median_hours_to_refix", "month", col=2)

myplot(data_month_first_backout, "median_p_hours_to_refix", "month")
myplot(data_month_first_reopen, "median_p_hours_to_rebuildok", "month")

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

myplot(data_month_create, "median_hours_to_badfix", "month")
mylines(data_month_create, "median_hours_to_fix", "month", col=2)
legend("topleft", c("bad fix", "fix"), lwd=1, col=c(1,2))
myplot(data_month_create, "median_p_hours_to_badfix", "month")

#' A bad fix takes longer 2x to 3x longer.
#' Bad fix-time is reducing.'

#' ## time-to-bad-buildok'

myplot(data_month_create, "median_hours_to_badbuildok", "month")
mylines(data_month_create, "median_hours_to_buildok", "month", col=2)
legend("topleft", c("bad buildok", "buildok"), lwd=1, col=c(1,2))
myplot(data_month_create, "median_p_hours_to_badbuildok", "month")


#'
#' ---------------------------------------------------------------------------
#'

#' ## time-to-reopen and time-to-backout

myplot(data_month_first_buildok, "median_hours_to_reopen", "month")
myplot(data_month_first_fix, "median_hours_to_backout", "month")

#' A typical bug takes about 5 hours to be backed out (if it is ever backed out)
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

myplot(data_month_first_review_ask, "median_hours_to_review_minus", "month")
mylines(data_month_first_review_ask, "median_hours_to_review_plus", "month", col=2)
legend("topleft", c("review-", "review+"), lwd=1, col=c(1,2))

#' Negative reviews take longer.

#' # Correlations

x <- data_month_first_fix %>%
  select(median_hours_to_buildok, median_hours_to_fix, median_hours_to_reopen, median_hours_to_backout,
    median_hours_to_rebuildok, median_hours_to_refix,
    reopen_count, backout_count, early_backout_count, late_backout_count)
library(PerformanceAnalytics)
chart.Correlation(x, method='spearman')

a <- subset(bug_data, !is.na(hours_to_refix) & !is.na(hours_to_backout))
with(a, cor.test(hours_to_refix, hours_to_backout), method='spearman')
with(a, plot(y=hours_to_refix, x=hours_to_backout))
#
a <- subset(bug_data, !is.na(hours_to_rebuildok) & !is.na(hours_to_reopen))
with(a, cor.test(hours_to_rebuildok, hours_to_reopen), method='spearman')
with(a, plot(y=hours_to_rebuildok, x=hours_to_reopen))

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

# hist(bug_data$hours_to_buildok)
# hist(bug_data$hours_to_reopen)
# hist(bug_data$hours_to_fix)
# hist(bug_data$hours_to_backout)