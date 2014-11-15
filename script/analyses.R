rm(list=ls())
library(dplyr)
library(stringr)

event_data <- readRDS("../data/firefox-event-data.rds")
event_labels <- readRDS("../data/firefox-event-labels.rds")

stopifnot(nrow(event_data) == nrow(event_labels))

###

events <- event_data %>% inner_join(event_labels, by="event")

###

xtabs(~ label + I(prev_bug_status), data=events)
xtabs(~ label + I(prev_commit_status), data=events)
xtabs(~ label + I(prev_review_status), data=events)


##

z <- events %>%
  group_by(bug) %>%
  summarise(
    reopened = any(label == 'reopen' & prev_bug_status == "buildok"),
    backed_out = any(label == "backout" & prev_commit_status == "fix"))

t <- xtabs(~reopened + backed_out, data=z)
t

a <- subset(z, reopened & !backed_out)
head(a)


# All REOPENED bugs contain a backout!

##

z <- events %>%
  group_by(bug) %>%
  summarise(
    builtok = any(label == 'buildok'),
    fixed = any(label == "fix"))

t <- xtabs(~builtok + fixed, data=z)
t

a <- subset(z, fixed & !builtok)
head(a)

# All resolution=FIXED bugs contain a commit!