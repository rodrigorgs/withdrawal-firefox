library(DBI)
library(RMySQL)
library(reshape)
library(yaml)
library(dplyr)

#' # Load data

bugs <- readRDS("../data/firefox-bugs.rds")

config <- yaml.load_file("../config/mozilla.yml")

db <- src_mysql(dbname = config$database$dbname, 
	host = config$database$host,
	port = config$database$port,
	user = config$database$user,
	password = config$database$password)

bug.numbers <- bugs$bug

ev <- tbl(db, "bugs_activity") %>%
	filter(bug_id %in% bug.numbers) %>%
	collect()

fielddefs <- tbl(db, "fielddefs") %>%
	collect() %>%
	select(fieldid = id, name)

events <- ev %>%
	inner_join(fielddefs, by="fieldid") %>%
	select(
		bug = bug_id,
		user = who,
		time = bug_when,
		field = name,
		previousvalue = removed,
		currentvalue = added) %>%
	arrange(bug, time)

time.str <- events$time

# Bugzilla times are in PST (Pacific Standard Time, -0800).
events$time <- as.POSIXct(paste0(time.str, " -0800"),
	format="%Y-%m-%d %H:%M:%s %z", tz="UTC")

#########

saveRDS(events, "../data/firefox-events.rds")
