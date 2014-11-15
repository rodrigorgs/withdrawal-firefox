# TODO: should we treat "review+ => review?" (clearing review flag) and "review? => feedback"?

rm(list=ls())
library(dplyr)
library(stringr)

events <- readRDS("../data/firefox-events.rds")

###

events <- subset(events, grepl("\\b(flags|flagtypes.name)\\b", field))

# Matches review, superreview etc.
# Sometimes matches ux-review@mozilla.com, but we discard these cases

reviews <- events %>%
	mutate(review_status = str_match(events$currentvalue, "review(.)")[,2]) %>%
	filter(!is.na(review_status) & review_status != '@') %>%
	select(event, review_status)

# review_status is either ?, +, or -:
#   ? (request review),
#   + (patch accepted)
#   - (patch rejected)

###

saveRDS(reviews, "../data/firefox-reviews.rds")
