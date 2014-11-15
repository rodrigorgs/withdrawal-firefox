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

event_data__bugcreation <- bugs %>%
	mutate(event = NA, source = "bugreports", commit = NA, user = reporter, time = initial.time, field = "creation", previousvalue = NA, currentvalue = description) %>%
	select(event, source, bug, commit, user, time, field, previousvalue, currentvalue)


event_data <- rbind(event_data__commitlog, event_data__bugreports, event_data__bugcreation) %>%
	arrange(time) %>%
	mutate(event = sequence(n()))

###

saveRDS(event_data, "../data/firefox-event-data.rds")
