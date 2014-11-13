rm(list=ls())
library(dplyr)
library(stringr)

commits <- readRDS("../data/firefox-commits.rds")
bugs <- readRDS("../data/firefox-bugs.rds")

###

commits$bug.fixed <- str_match(commits$message, "(?i)^ *bug *([0-9]{5,6})\\b")[,2]

fixes <- commits %>% 
  select(commit, bug=bug.fixed) %>%
  mutate(bug = as.integer(bug)) %>%
	arrange(commit, bug)

###

saveRDS(fixes, "../data/firefox-fixes.rds")

