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
      review_minus_per_day = review_minus_count / num_days,
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
    sprintf("%3.2f%%", mean(planned, na.rm=T) * 100),
    sprintf("%3.2f%%", mean(rapid, na.rm=T) * 100),
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
    sprintf("%3.2f", median(planned, na.rm=T)),
    sprintf("%3.2f", median(rapid, na.rm=T)),
    stars_for_pvalue(pvalue)
    )
  strvalues
}
rowify_count <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type() %>% compute_summary()
  planned <- z[z$release_type == 'planned',][[column]]
  rapid <- z[z$release_type == 'rapid',][[column]]
  # pvalue <- wilcox.test(planned, rapid)$p.value

  strvalues <- c(
    title,
    sprintf("%3.2f", planned),
    sprintf("%3.2f", rapid),
    "N/A")
  strvalues
}

# PRODUCTIVITY
#   fixes per day
#   reopens per day
#   backouts per day (TODO: create plots for early and late backout per day)
#   reviews- per day (TODO: create this plot)

{
  headers <- c("", "traditional", "rapid", "significance")
  tab <- headers

  tab <- c("PRODUCTIVITY", "", "", "") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs fixed per day", bug_data, "time_first_fix", "fixes_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs negatively reviewed per day", bug_data, "time_first_review_minus", "review_minus_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs backed out per day", bug_data, "time_first_backout", "backouts_per_day") %>% rbind(tab, .)
    tab <- rowify_count("  Bugs reopened per day", bug_data, "time_first_reopen", "reopens_per_day") %>% rbind(tab, .)

  tab <- c("EFFICACY", "", "", "") %>% rbind(tab, .)
    tab <- rowify_binary("  Negative review rate", bug_data, "time_first_review_ask", "has_review_minus") %>% rbind(tab, .)
    tab <- rowify_binary("  Backout rate", bug_data, "time_first_fix", "has_backout") %>% rbind(tab, .)
    tab <- rowify_binary("    Early backout rate", bug_data, "time_first_fix", "has_early_backout") %>% rbind(tab, .)
    tab <- rowify_binary("    Late backout rate", bug_data, "time_first_fix", "has_late_backout") %>% rbind(tab, .)
    tab <- rowify_binary("  Reopening rate", bug_data, "time_first_buildok", "has_reopen") %>% rbind(tab, .)

  tab <- c("EFFICIENCY", "", "", "") %>% rbind(tab, .)
  # time-to-fix
    tab <- rowify_continuous("  Time-to-ask review (hours)", bug_data, 'time_create', 'hours_to_review_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-fix (hours)", bug_data, 'time_create', 'hours_to_fix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-buildok (hours)", bug_data, 'time_create', 'hours_to_buildok') %>% rbind(tab, .)
    # TODO: diff between time-to-fix and time-to-buildok
  tab <- c("  -", "", "", "") %>% rbind(tab, .)
  # time-to-badfix
    tab <- rowify_continuous("  Time-to-bad review ask (hours)", bug_data, 'time_create', 'hours_to_badreview_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-badfix (hours)", bug_data, 'time_create', 'hours_to_badfix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-badbuildok (hours)", bug_data, 'time_create', 'hours_to_badbuildok') %>% rbind(tab, .)
  tab <- c("  ---", "", "", "") %>% rbind(tab, .)
  # time-to-refix
    tab <- rowify_continuous("  Time-to-rereview ask (hours)", bug_data, 'time_first_review_minus', 'hours_to_rereview_ask') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-refix (hours)", bug_data, 'time_first_backout', 'hours_to_refix') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-rebuildok (hours)", bug_data, 'time_first_reopen', 'hours_to_rebuildok') %>% rbind(tab, .)
  tab <- c("  ---", "", "", "") %>% rbind(tab, .)
  # time-to-reopen
    tab <- rowify_continuous("  Time-to-review- (hours)", bug_data, 'time_first_review_ask', 'hours_to_review_minus') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-review+ (hours)", bug_data, 'time_first_review_ask', 'hours_to_review_plus') %>% rbind(tab, .)

    tab <- rowify_continuous("  Time-to-backout (hours)", bug_data, 'time_first_fix', 'hours_to_backout') %>% rbind(tab, .)
    tab <- rowify_continuous("  Time-to-reopen (hours)", bug_data, 'time_first_buildok', 'hours_to_reopen') %>% rbind(tab, .)

  rownames(tab) <- NULL
  tab
}

# Sum time-to-fix
{
  badfix <- sum(bug_data$hours_to_fix * bug_data$has_backout)
  goodfix <- sum(bug_data$hours_to_fix * !bug_data$has_backout)
  mean(bug_data$has_backout, na.rm=T)
  badfix / (badfix + goodfix)

  # How much time is spent writing inappropriate fixes
  badreview_ask <- sum(bug_data$hours_to_review_ask * bug_data$has_review_minus, na.rm=T)
  goodreview_ask <- sum(bug_data$hours_to_review_ask * !bug_data$has_review_minus, na.rm=T)
  mean(bug_data$has_review_minus, na.rm=T)
  badreview_ask / (badreview_ask + goodreview_ask)

  # is second fix time positively correlated to time-to-withdraw?
  df <- subset(bug_data, !is.na(hours_to_refix))
  cor.test(df$hours_to_refix, df$hours_to_backout)
  cor.test(df$hours_to_refix, df$hours_to_fix)
  # (same for second attached patch)
  df <- subset(bug_data, !is.na(hours_to_rereview_ask) & !is.na(hours_to_review_minus))
  cor.test(df$hours_to_rereview_ask, df$hours_to_review_minus)
  cor.test(df$hours_to_rereview_ask, df$hours_to_review_ask)

  # writing second fix takes much less time than writing the first one
  median(bug_data$hours_to_refix / bug_data$hours_to_fix, na.rm=T)
  boxplot(1+bug_data$hours_to_fix, 1+bug_data$hours_to_refix, log="y", names=c("fix", "refix"))
  #
  median(bug_data$hours_to_rereview_ask / bug_data$hours_to_review_ask, na.rm=T)
  #
  median(bug_data$hours_to_rebuildok / bug_data$hours_to_buildok, na.rm=T)
}

{
  min_date <- '2009-01-01'
  max_date <- '2013-07-01'

# group_by_month <- function(df) {
#   ret <- df %>%
#     mutate(month = strftime(time, "%Y-%m")) %>%
#     filter(month %>% between('2009-02', '2013-07')) %>%
#     group_by(month)
#   invisible(ret)
# }
# TODO: adapta code below to use function group_by_month

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