rm(list=ls())
source("../lib/shift.R")
library(dplyr)
library(stringr)

events <- readRDS("../data/firefox-events.rds")

###

flag_events <- subset(events, grepl("\\b(flags|flagtypes.name)\\b", field))
flag_events$review_status <- str_match(flag_events$currentvalue, "review(.)")[,2]

flagged_bugs <- flag_events %>%
  group_by(bug) %>%
  summarise(
    reviewed = any(review_status %in% c("+", "-"), na.rm=T) ,
    reviewed.minus = any(review_status == "-", na.rm=T),
    review.cleared = any(prevv(review_status == "?") &
      is.na(review_status) & prevv(user) != user),
    feedback.after.review = any(prevv(review_status) == "?" &
      grepl("feedback", currentvalue), na.rm=T),
    rejected = reviewed.minus | review.cleared | feedback.after.review)

###

saveRDS(flagged_bugs, "../data/firefox-reviews.rds")
