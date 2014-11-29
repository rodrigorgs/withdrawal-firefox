rm(list=ls())
library(dplyr)
source("../lib/carry-events.R", chdir=T)

bugs <- readRDS("../data/firefox-bugs.rds")
events <- readRDS("../data/firefox-events.rds")
commit_data <- readRDS("../data/firefox-commit-data.rds")

###

events$resolution <- carry.events(events, field == "resolution")

# Mix events, bugs, and commits

event_data__commitlog <- commit_data %>%
  filter(bug %in% events$bug) %>%
  mutate(event = NA, source = "commitlog", field = type, previousvalue = NA, currentvalue = message, resolution = NA) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue, resolution)

event_data__bugreports <- events %>%
	mutate(source = "bugreports", commit = NA) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue, resolution)

event_data__bugcreation <- bugs %>%
	mutate(event = NA, source = "bugreports", commit = NA, user = reporter, time = initial.time, field = "creation", previousvalue = NA, currentvalue = description, resolution = NA) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue, resolution)



event_data <- rbind(event_data__commitlog, event_data__bugreports, event_data__bugcreation) %>%
	arrange(bug, time)

# Now let's assign an event id to commits and bug creation

last_event_id <- max(event_data$event, na.rm=T)
num_na <- sum(is.na(event_data$event))
event_data$event[is.na(event_data$event)] <- seq(from = last_event_id + 1, length.out = num_na)

###

saveRDS(event_data, "../data/firefox-event-data.rds")
