rm(list=ls())

library(stringr)

#' # Load data

# hint for gen-makefile
# - readRDS("../data/mozilla-hg-log.bz2")

file <- bzfile("../data/mozilla-hg-log.bz2")
lines <- readLines(file)
close(file)

#' # Transform data

fields <- str_split_fixed(lines, ' \\| ', n=4)

commit <- fields[, 1]
time.str <- fields[, 2]
user <- fields[, 3]
message <- fields[, 4]

#' Convert dates

time <- as.POSIXct(time.str, format="%Y-%m-%d %H:%M %z", tz="UTC")
# Handle +1300 timezone, which isn't handled by R
times <- substring(time.str, 1, 16)
timezones <- substring(time.str, 18, 22)
indices <- which(timezones == "+1300")
time[indices] <- as.POSIXct(paste0(times[indices], " +1200"), format="%Y-%m-%d %H:%M %z", tz="UTC")
time[indices] <- time[indices] - (60*60)
stopifnot(sum(is.na(time)) == 0)

#' Extract user e-mails

# mails <- str_match(user, '[< ]([^< ]+@[^ ]+\\.[^ >]+)[> ]')[, 2]
# View(unique(sort(mails)))
mailusers <- str_match(user, '[< ]([^< ]+)@[^ ]+\\.[^ >]+[> ]')[, 2]
mailusers <- gsub('[.+]bugzilla', '', mailusers)
# View(unique(sort(mailusers)))
# names <- str_match(user, '^(.+)<')[, 2]
# View(unique(sort(names)))
# x <- data.frame(name=names, mail=mails)
# View(unique(x[order(x$name), ]))

df <- data.frame(commit=commit, time=time, user=mailusers, message=message, stringsAsFactors=F)

#' # Output data

saveRDS(df, "../data/firefox-commits.rds")

