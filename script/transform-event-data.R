rm(list=ls())
library(dplyr)

bugs <- readRDS("../data/firefox-bugs.rds")
events <- readRDS("../data/firefox-events.rds")
commit_data <- readRDS("../data/firefox-commit-data.rds")

###

# Mix events and commits

event_data__commitlog <- commit_data %>%
	mutate(event = NA, source = "commitlog", field = type, previousvalue = NA, currentvalue = message) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue)

event_data__bugreports <- events %>%
	mutate(event = NA, source = "bugreports", commit = NA) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue)

event_data <- rbind(event_data__commitlog, event_data__bugreports) %>%
	arrange(time) %>%
	mutate(event = sequence(n()), label = NA)

# Label is review-, backout, reopen etc.

###

saveRDS(event_data, "../data/firefox-event-data.rds")
