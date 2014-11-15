rm(list=ls())
library(dplyr)
source("../lib/build-events.R", chdir=T)

# Use dplyr at least 3.0.9000 because of bug https://github.com/hadley/dplyr/issues/194

event_data <- readRDS("../data/firefox-event-data.rds")
event_labels <- readRDS("../data/firefox-event-labels.rds")
bugs <- readRDS("../data/firefox-bugs.rds")

###

events <- build_events(bugs, event_data, event_labels)

nrow(event_data)
nrow(events)

# Annotate events with times
events$time_create <- events$time; events[events$label != "create", "time_create"] <- NA
events$time_buildok <- events$time; events[events$label != "buildok", "time_buildok"] <- NA
events$time_reopen <- events$time; events[events$label != "reopen" | events$prev_bug_status != "buildok", "time_reopen"] <- NA
#
events$time_fix <- events$time; events[events$label != "fix", "time_fix"] <- NA
events$time_backout <- events$time; events[events$label != "backout", "time_backout"] <- NA
events$time_early_backout <- events$time; events[events$label != "backout" | events$prev_bug_status == "buildok", "time_early_backout"] <- NA
events$time_late_backout <- events$time; events[events$label != "backout" | events$prev_bug_status != "buildok", "time_late_backout"] <- NA
#
events$time_review_ask <- events$time; events[events$label != "review?", "time_review_ask"] <- NA
events$time_review_plus <- events$time; events[events$label != "review+", "time_review_plus"] <- NA
events$time_review_minus <- events$time; events[events$label != "review-", "time_review_minus"] <- NA

#
{
  bug_data <- events %>%
    group_by(bug) %>%
    summarise(
      time_create = min(time_create, na.rm=T),
      time_first_buildok = min(time_buildok, na.rm=T),
      time_first_reopen = min(time_reopen, na.rm=T),
      time_first_fix = min(time_fix, na.rm=T),
      time_first_backout = min(time_backout, na.rm=T),
      time_first_early_backout = min(time_early_backout, na.rm=T),
      time_first_late_backout = min(time_late_backout, na.rm=T),
      time_first_review_ask = min(time_review_ask, na.rm=T),
      time_first_review_plus = min(time_review_plus, na.rm=T),
      time_first_review_minus = min(time_review_minus, na.rm=T)) %>%
    mutate(
      has_reopen = !is.na(time_first_reopen),
      has_fix = !is.na(time_first_fix),
      has_backout = !is.na(time_first_backout),
      has_early_backout = !is.na(time_first_early_backout),
      has_late_backout = !is.na(time_first_late_backout),
      has_review_ask = !is.na(time_first_review_ask),
      has_review_plus = !is.na(time_first_review_plus),
      has_review_minus = !is.na(time_first_review_minus))
}

mean(!is.na(bug_data$time_first_review_minus))
mean(!is.na(bug_data$time_first_backout))
mean(!is.na(bug_data$time_first_reopen))
#
mean(!is.na(bug_data$time_first_early_backout))
mean(!is.na(bug_data$time_first_late_backout))

###

saveRDS(bug_data, "../data/firefox-bug-data.rds")
