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

table(data$label)

###

# Now, more sophisticated labeling

final <- data %>% arrange(bug, time)

final$prev_bug_status <- carry.events(final, label %in% c("buildok", "reopen", "create"), "label") %>% prevv()
final$prev_commit_status <- carry.events(final, label %in% c("backout", "fix"), "label") %>% prevv()
final$prev_review_status <- carry.events(final, label %in% c("review?", "review+", "review-"), "label") %>% prevv()

final$prev_bug_status[final$label == "create"] <- NA
final$prev_commit_status[final$label == "create"] <- NA
final$prev_review_status[final$label == "create"] <- NA

##

xtabs(~ label + I(prev_bug_status), data=final)
xtabs(~ label + I(prev_commit_status), data=final)
xtabs(~ label + I(prev_review_status), data=final)


##

z <- final %>%
  group_by(bug) %>%
  summarise(
    reopened = any(label == 'reopen' & prev_bug_status == "buildok"),
    backed_out = any(label == "backout" & prev_commit_status == "fix"))

t <- xtabs(~reopened + backed_out, data=z)
t

# All REOPENED bugs contain a backout!

##

z <- final %>%
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