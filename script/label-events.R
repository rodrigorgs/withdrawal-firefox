rm(list=ls())
library(dplyr)
library(stringr)
source("../lib/carry-events.R", chdir=T)
source("../lib/na.R", chdir=T)

# bugs <- read_RDS("../data/firefox-bugs.rds")
event_data <- readRDS("../data/firefox-event-data.rds")
reviews <- readRDS("../data/firefox-reviews.rds")

###

data <- event_data %>% left_join(reviews, by="event")
data$label <- ""

data$label[data$field == "resolution" & data$currentvalue == "FIXED"] <- "buildok"
data$label[data$field == "bug_status" & data$currentvalue == "REOPENED"] <- "reopen"
data$label[data$field == "creation"] <- "create"

data$label[data$field == "backout"] <- "backout"
data$label[data$field == "fix"] <- "fix"

data$label[data$review_status == "?"] <- "review?"
data$label[data$review_status == "-"] <- "review-"
data$label[data$review_status == "+"] <- "review+"

# check if data is ok
{
  stopifnot(any(data$label == "review?"))
  stopifnot(any(data$label == "review+"))
  stopifnot(any(data$label == "review-"))
  stopifnot(any(data$label == "backout"))
  stopifnot(any(data$label == "fix"))
  stopifnot(any(data$label == "buildok"))
  stopifnot(any(data$label == "reopen"))
  table(data$label)
}
###

# Now, more sophisticated labeling

complete <- data %>% 
  filter(label != "") %>%
  arrange(bug, time)

complete$prev_bug_status <- carry.events(complete, label %in% c("buildok", "reopen", "create"), "label") %>% prevv()
complete$prev_commit_status <- carry.events(complete, label %in% c("backout", "fix"), "label") %>% prevv()
complete$prev_review_status <- carry.events(complete, label %in% c("review?", "review+", "review-"), "label") %>% prevv()
#
complete$prev_bug_status[is.na(complete$prev_bug_status)] <- ""
complete$prev_commit_status[is.na(complete$prev_commit_status)] <- ""
complete$prev_review_status[is.na(complete$prev_review_status)] <- ""
#
complete$prev_bug_status[complete$label == "create"] <- ""
complete$prev_commit_status[complete$label == "create"] <- ""
complete$prev_review_status[complete$label == "create"] <- ""


complete$time_prev_bug_status <- carry.events(complete, label %in% c("buildok", "reopen", "create"), "time") %>% prevv()
complete$time_prev_commit_status <- carry.events(complete, label %in% c("backout", "fix"), "time") %>% prevv()
complete$time_prev_review_status <- carry.events(complete, label %in% c("review?", "review+", "review-"), "time") %>% prevv()
#
attr(complete$time_prev_bug_status, "tzone") <- "UTC"
attr(complete$time_prev_commit_status, "tzone") <- "UTC"
attr(complete$time_prev_review_status, "tzone") <- "UTC"

###

event_labels <- complete %>% 
  select(event, bug, time, 
    label, 
    prev_bug_status, time_prev_bug_status, 
    prev_commit_status, time_prev_commit_status, 
    prev_review_status, time_prev_review_status)

# bugs_status_fixed <- bugs$bug[bugs$resolution == "FIXED"]
# filtered <- complete %>%
#   filter(bug %in% bugs_status_fixed) %>%
#   group_by(bug) %>%
#   filter(any(label == "fix"))

# nrow(complete)
# nrow(filtered)

# y <- filtered %>%
#   group_by(bug) %>%
#   summarise(has_bugreport = any(label == 'buildok'))
# stopifnot(all(y$has_bugreport))

###

saveRDS(event_labels, "../data/firefox-event-labels.rds")

