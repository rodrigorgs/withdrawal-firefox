library(DBI)
library(RMySQL)
library(reshape)
library(yaml)
library(dplyr)

#' # Load data

config <- yaml.load_file("../config/mozilla.yml")

db <- src_mysql(dbname = config$database$dbname, 
  host = config$database$host,
  port = config$database$port,
  user = config$database$user,
  password = config$database$password)

bugs <- tbl(db, "bugs") %>%
  filter(delta_ts >= config$dates$initial,
    creation_ts <= config$dates$final,
    product_id %in% c(1, 21),
    resolution == "FIXED") %>%
  collect()

#' # Transform data

bugs <- reshape::rename(bugs, c(
  "bug_id" = "bug",
  "creation_ts" = "initial.time",
  "delta_ts" = "final.time",
  "bug_status" = "status",
  "short_desc" = "description"))

bugs$initial.time <- as.POSIXct(bugs$initial.time, tz="UTC")
bugs$final.time <- as.POSIXct(bugs$final.time, tz="UTC")

#' # Summary

nrow(bugs)
min(bugs$initial.time)
max(bugs$final.time)

#' # Save data

saveRDS(bugs, "../data/firefox-bugs.rds")
