rm(list = ls())
library(dplyr)

bug_data <- readRDS("../data/firefox-bug-data.rds")

##

bugs_by_creation <- bug_data %>%
  filter(time_create >= as.POSIXct('2009-02-01') & time_create < as.POSIXct('2013-08-01'))

##

saveRDS(bugs_by_creation, "../data/firefox-bugs-by-creation.rds")