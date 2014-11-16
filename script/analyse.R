rm(list=ls())
library(dplyr)

bug_data <- readRDS("../data/firefox-bug-data.rds")

###

compute_summary <- function(groupped_bug_data) {
  ret <- groupped_bug_data %>%
    summarise(
      fixes = n(),
      num_days = as.numeric(max(time_create) - min(time_create), units='days'),
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
  x <- bug_data %>%
    filter(time_first_fix >= '2009-01-01' & time_first_fix <= '2013-06-30') %>%
    mutate(month = strftime(time_first_fix, "%Y-%m", origin=origin)) %>%
    group_by(month) %>%
    compute_summary()

  # TODO: also compute taking time_first_reopen and time_first_backout as reference
}

###

plot(x$reopens_per_day, type='l')
plot(x$backouts_per_day, type='l')
plot(x$fixes_per_day, type='l')

plot(x$reopen_rate, type='l')
plot(x$backout_rate, type='l')
plot(x$early_backout_rate, type='l', col=1, ylim=c(0, 0.11))
lines(x$late_backout_rate, col=2)

plot(x$median_hours_to_buildok, type='l')
plot(x$median_hours_to_fix, type='l')
plot(x$median_hours_to_reopen, type='l')
plot(x$median_hours_to_backout, type='l')

# plot(x$mean_hours_to_buildok, type='l')
# plot(x$mean_hours_to_fix, type='l')
# plot(x$mean_hours_to_reopen, type='l')
# plot(x$mean_hours_to_backout, type='l')

# hist(bug_data$hours_to_buildok)
# hist(bug_data$hours_to_reopen)
# hist(bug_data$hours_to_fix)
# hist(bug_data$hours_to_backout)