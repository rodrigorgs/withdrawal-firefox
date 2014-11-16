rm(list=ls())
library(dplyr)
library(Hmisc)
library(zoo)

bug_data <- readRDS("../data/firefox-bug-data.rds")

###

#
# `groupped_bug_data`: the result of 
#
#     bug_data %>% group_by(some_criteria...) %>% mutate(num_days = ...)
#
# Please note that we require a column called `num_days` with the number of
# days within each group.
#
compute_summary <- function(groupped_bug_data) {
  ret <- groupped_bug_data %>%
    summarise(
      fixes = n(),
      num_days = max(num_days),
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
      median_hours_to_backout = median(hours_to_backout, na.rm=T),
      median_hours_to_reopen = median(hours_to_reopen, na.rm=T),
      median_hours_to_buildok = median(hours_to_buildok, na.rm=T),
      #
      mean_hours_to_fix = mean(hours_to_fix, na.rm=T),
      mean_hours_to_backout = mean(hours_to_backout, na.rm=T),
      mean_hours_to_reopen = mean(hours_to_reopen, na.rm=T),
      mean_hours_to_buildok = mean(hours_to_buildok, na.rm=T))

  invisible(ret)
}

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

plot(data_month_first_fix$fixes_per_day ~ data_month_first_fix$month, type='l', ylim=c(0,41))
#
plot(data_month_first_reopen$reopens_per_day ~ data_month_first_fix$month, type='l')
plot(data_month_first_backout$backouts_per_day ~ data_month_first_fix$month, type='l')

plot(data_month_first_fix$reopen_rate ~ data_month_first_fix$month, type='l')
plot(data_month_first_fix$backout_rate ~ data_month_first_fix$month, type='l')
plot(data_month_first_fix$early_backout_rate ~ data_month_first_fix$month, type='l', col=1, ylim=c(0, 0.11))
lines(data_month_first_fix$late_backout_rate ~ data_month_first_fix$month, col=2)

plot(data_month_create$median_hours_to_buildok ~ data_month_first_fix$month, type='l')
plot(data_month_create$median_hours_to_fix ~ data_month_first_fix$month, type='l')
#
plot(data_month_first_buildok$median_hours_to_reopen ~ data_month_first_fix$month, type='l')
plot(data_month_first_fix$median_hours_to_backout ~ data_month_first_fix$month, type='l')

plot(data_month_first_review_ask$review_ask_rate ~ data_month_first_fix$month, type='l', ylim=c(0,1))
plot(data_month_first_review_ask$review_minus_rate ~ data_month_first_fix$month, type='l', ylim=c(0,0.2))
plot(data_month_first_review_ask$review_plus_rate ~ data_month_first_fix$month, type='l', ylim=c(0,1))

# Correlations

x <- data_month_first_fix %>%
  select(median_hours_to_buildok, median_hours_to_fix, median_hours_to_reopen, median_hours_to_backout,
    reopen_count, backout_count, early_backout_count, late_backout_count)
library(PerformanceAnalytics)
chart.Correlation(x, method='spearman')

# Associations

x <- bug_data %>%
  select(has_backout, has_reopen, has_review_minus, has_early_backout, has_late_backout)
library(vcd)
mosaic(~ has_review_minus + has_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_early_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_late_backout, data=bug_data, direction='v', shade=T)
mosaic(~ has_review_minus + has_reopen, data=bug_data, direction='v', shade=T)


# hist(bug_data$hours_to_buildok)
# hist(bug_data$hours_to_reopen)
# hist(bug_data$hours_to_fix)
# hist(bug_data$hours_to_backout)