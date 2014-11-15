rm(list=ls())
library(dplyr)
library(stringr)
source("../lib/carry-events.R", chdir=T)

event_data <- readRDS("../data/firefox-event-data.rds")
reviews <- readRDS("../data/firefox-reviews.rds")

###

data <- event_data %>% left_join(reviews, by="event")

data$label[data$field == "resolution" & data$currentvalue == "FIXED"] <- "buildok"
data$label[data$field == "bug_status" & data$currentvalue == "REOPENED"] <- "reopen"
data$label[data$field == "creation"] <- "create"

data$label[data$field == "backout"] <- "backout"
data$label[data$field == "fix"] <- "fix"

data$label[data$review_status == "?"] <- "review?"
data$label[data$review_status == "-"] <- "review-"
data$label[data$review_status == "+"] <- "review+"

# check if data is ok

stopifnot(any(data$label == "review?"))
stopifnot(any(data$label == "review+"))
stopifnot(any(data$label == "review-"))
stopifnot(any(data$label == "backout"))
stopifnot(any(data$label == "fix"))
stopifnot(any(data$label == "buildok"))
stopifnot(any(data$label == "reopen"))
table(data$label)

###

# Now, more sophisticated labeling

complete <- data %>% arrange(bug, time)

complete$prev_bug_status <- carry.events(complete, label %in% c("buildok", "reopen", "create"), "label") %>% prevv()
complete$prev_commit_status <- carry.events(complete, label %in% c("backout", "fix"), "label") %>% prevv()
complete$prev_review_status <- carry.events(complete, label %in% c("review?", "review+", "review-"), "label") %>% prevv()

complete$prev_bug_status[complete$label == "create"] <- NA
complete$prev_commit_status[complete$label == "create"] <- NA
complete$prev_review_status[complete$label == "create"] <- NA

###

saveRDS(complete, "../data/firefox-events-complete.rds")

#####################################

xtabs(~ label + I(prev_bug_status), data=complete)
xtabs(~ label + I(prev_commit_status), data=complete)
xtabs(~ label + I(prev_review_status), data=complete)


##

z <- complete %>%
  group_by(bug) %>%
  summarise(
    reopened = any(label == 'reopen' & prev_bug_status == "buildok"),
    backed_out = any(label == "backout" & prev_commit_status == "fix"))

t <- xtabs(~reopened + backed_out, data=z)
t

# All REOPENED bugs contain a backout!

##

z <- complete %>%
  group_by(bug) %>%
  summarise(
    builtok = any(label == 'buildok'),
    fixed = any(label == "fix"))

a <- subset(z, fixed & !builtok)
head(a)
subset(event_data, bug == 10209)

t <- xtabs(~builtok + fixed, data=z)
t
# All resolution=FIXED bugs contain a commit!