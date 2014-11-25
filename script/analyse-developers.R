rm(list=ls())
library(dplyr)
source('../lib/analysis-functions.R', chdir=T)

events <- readRDS("../data/firefox-event-labels.rds")

a <- events %>%
  filter(label == 'fix') %>%
  group_by(user, month = strftime(time, "%Y-%m")) %>%
  summarise(
    n_bugs_fixed = n_distinct(bug)) %>%
  filter(n_bugs_fixed >= 3) %>%
  group_by(month) %>%
  summarise(n_fixers = n_distinct(user))

saveRDS(a, "../data/firefox-developers-month.rds")
