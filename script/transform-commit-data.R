rm(list=ls())
library(dplyr)

commits <- readRDS("../data/firefox-commits.rds")
backouts <- readRDS("../data/firefox-backouts.rds")
fixes <- readRDS("../data/firefox-fixes.rds")

###

# Augment commits with info about backouts and fixes

backouts$type <- "backout"
fixes$type <- "fix"
commit_data <- rbind(backouts, fixes) %>%
	inner_join(commits, by="commit") %>%
	arrange(time) %>%
	select(commit, type, bug, time, user, message)

###

saveRDS(commit_data, "../data/firefox-commit-data.rds")