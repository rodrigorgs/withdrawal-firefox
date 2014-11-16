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
events$time_create <- events$time
events$time_buildok <- events$time
events$time_rebuildok <- events$time
events$time_reopen <- events$time
events$time_fix <- events$time
events$time_refix <- events$time
events$time_backout <- events$time
events$time_early_backout <- events$time
events$time_late_backout <- events$time
events$time_review_ask <- events$time
events$time_review_plus <- events$time
events$time_review_minus <- events$time

col <- "time_create"; events[!(events$label == "create"), col] <- NA
col <- "time_buildok"; events[!(events$label == "buildok"), col] <- NA
col <- "time_rebuildok"; events[!(events$label == "buildok" & events$prev_bug_status == 'reopen'), col] <- NA
col <- "time_reopen"; events[!(events$label == "reopen" & events$prev_bug_status == "buildok"), col] <- NA
col <- "time_fix"; events[!(events$label == "fix"), col] <- NA
col <- "time_refix"; events[!(events$label == "fix" & events$prev_commit_status == 'backout'), col] <- NA
col <- "time_backout"; events[!(events$label == "backout"), col] <- NA
col <- "time_early_backout"; events[!(events$label == "backout" & events$prev_bug_status == "create"), col] <- NA
col <- "time_late_backout"; events[!(events$label == "backout" & events$prev_bug_status == "buildok"), col] <- NA
col <- "time_review_ask"; events[!(events$label == "review?"), col] <- NA
col <- "time_review_plus"; events[!(events$label == "review+"), col] <- NA
col <- "time_review_minus"; events[!(events$label == "review-"), col] <- NA

#
{
  bug_data <- events %>%
    group_by(bug) %>%
    summarise(
      time_create = min(time_create, na.rm=T),
      time_first_buildok = min(time_buildok, na.rm=T),
      time_first_rebuildok = min(time_rebuildok, na.rm=T),
      time_first_reopen = min(time_reopen, na.rm=T),
      time_first_fix = min(time_fix, na.rm=T),
      time_first_refix = min(time_refix, na.rm=T),
      time_first_backout = min(time_backout, na.rm=T),
      time_first_early_backout = min(time_early_backout, na.rm=T),
      time_first_late_backout = min(time_late_backout, na.rm=T),
      time_first_review_ask = min(time_review_ask, na.rm=T),
      time_first_review_plus = min(time_review_plus, na.rm=T),
      time_first_review_minus = min(time_review_minus, na.rm=T),
      num_backouts = sum(label == "backout"),
      num_fixes = sum(label == "fix"),
      num_reopens = sum(label == "reopen"),
      num_buildoks = sum(label == "buildok")) %>%
    mutate(
      hours_to_fix = as.numeric(time_first_fix - time_create, units='hours'),
      hours_to_refix = as.numeric(time_first_refix - time_first_backout, units='hours'),
      hours_to_buildok = as.numeric(time_first_buildok - time_create, units='hours'),
      hours_to_rebuildok = as.numeric(time_first_rebuildok - time_first_reopen, units='hours'),
      #
      hours_to_reopen = as.numeric(time_first_reopen - time_first_buildok, units='hours'),
      hours_to_backout = as.numeric(time_first_backout - time_first_fix, units='hours'),
      #
      has_reopen = !is.na(time_first_reopen),
      has_fix = !is.na(time_first_fix),
      has_backout = !is.na(time_first_backout),
      has_early_backout = !is.na(time_first_early_backout),
      has_late_backout = !is.na(time_first_late_backout),
      has_review_ask = !is.na(time_first_review_ask),
      has_review_plus = !is.na(time_first_review_plus),
      has_review_minus = !is.na(time_first_review_minus)) %>%
    filter(
      is.na(hours_to_fix) | hours_to_fix > 0,
      is.na(hours_to_refix) | hours_to_refix > 0,
      is.na(hours_to_buildok) | hours_to_buildok > 0,
      is.na(hours_to_rebuildok) | hours_to_rebuildok > 0,
      is.na(hours_to_reopen) | hours_to_reopen > 0,
      is.na(hours_to_backout) | hours_to_backout > 0)
}

mean(!is.na(bug_data$time_first_review_minus))
mean(!is.na(bug_data$time_first_backout))
mean(!is.na(bug_data$time_first_reopen))
#
mean(!is.na(bug_data$time_first_early_backout))
mean(!is.na(bug_data$time_first_late_backout))

###

saveRDS(bug_data, "../data/firefox-bug-data.rds")
