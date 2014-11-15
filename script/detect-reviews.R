# TODO: should we treat "review+ => review?" (clearing review flag) and "review? => feedback"?

rm(list=ls())
library(dplyr)
library(stringr)

event_data <- readRDS("../data/firefox-event-data.rds")

###

events <- subset(event_data, 
	source == "bugreports"
	& grepl("\\b(flags|flagtypes.name)\\b", field))

# Matches review, superreview etc.
# Sometimes matches ux-review@mozilla.com, but we discard these cases

reviews <- events %>%
	mutate(label = str_match(events$currentvalue, "review(.)")[,2]) %>%
	filter(!is.na(label) & label != '@') %>%
	select(event, label)

reviews$label[reviews$label == "?"] <- "review?"
reviews$label[reviews$label == "+"] <- "review+"
reviews$label[reviews$label == "-"] <- "review-"

###

saveRDS(reviews, "../data/firefox-reviews.rds")
