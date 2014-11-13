rm(list=ls())
library(dplyr)
library(stringr)
source("../lib/unlist-column.R")

commits <- readRDS("../data/firefox-commits.rds")

###

commits$bug_fixed <- str_match(commits$message, "(?i)^ *bug *([0-9]{5,6})\\b")[,2]

#' # Find the bugs backed out by each backout commit

#' First, let's threat the case where the commit message specifies the bugs it backs out.

backout_commits <- commits
backout_commits <- cbind(backout_commits, str_locate(commits$message, "(?i)\\bback.{0,5}out\\b"))
backout_commits <- subset(backout_commits, !is.na(start))
backout_commits <- subset(backout_commits, !grepl("(?i)merg(e|ing)", message))
backout_commits <- subset(backout_commits, !grepl("(?i)re.?land", message))

#' Extract part of message that refers to bugs being backed out

msg <- substring(backout_commits$message, backout_commits$end + 1)
# ignore bug references after those expressions
msg <- gsub("caus.*", "", msg, ignore.case=T)
msg <- gsub("\\bfix.*", "", msg, ignore.case=T)
msg <- gsub("due to .*", "", msg, ignore.case=T)
msg <- gsub("\\bsee .*", "", msg, ignore.case=T)
msg <- gsub("\\bresolv.*", "", msg, ignore.case=T)
msg <- gsub("\\bsuspic.*", "", msg, ignore.case=T)
backout_commits$msg_after_backout <- msg

backout_commits$bugs_backedout <- str_match_all(backout_commits$msg_after_backout, "\\b[0-9]{6}\\b")

###########################################

#' Now, the case where the commit message specifies the changesets (commits) it backs out (so we have to look further to the bug fixed in the changesets that were backed out).

backout_commits$commits_backedout <- str_match_all(backout_commits$msg_after_backout, "\\b[0-9a-f]{7,12}\\b")

bch <- backout_commits %>%
  select(commit, commits_backedout) %>%
  unlist.column(commit, commits_backedout, "id", "commit_backedout") %>%
	merge(commits, by.x="commit_backedout", by.y="commit") %>%
	select(commit = id, bug = bug_fixed)

#' Mix the two

backouts <- unlist.column(backout_commits, commit, bugs_backedout, "commit", "bug") %>%
	rbind(bch) %>%
	arrange(commit, bug) %>%
  mutate(bug = as.integer(bug)) %>%
  unique()

###

saveRDS(backouts, "../data/firefox-backouts.rds")
