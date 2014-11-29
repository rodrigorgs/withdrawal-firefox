########################################
rm(list=ls())
library(gplots)
library(VennDiagram)

bug_data <- readRDS("../data/firefox-bug-data.rds")
events <- readRDS("../data/firefox-event-labels.rds")

head(bug_data)
data <- subset(bug_data, time_create >= '2011-06-1')

# Withdrawal types
x <- data[, c("has_review_minus", "has_backout", "has_reopen")]
v <- venn(x)
v[,1] <- sprintf("%.1f%%", 100 * v[,1] / nrow(x))
plot(v)

# Backout types
x <- data[, c("has_early_backout", "has_backout", "has_late_backout")]
v <- venn(x)
v[,1] <- sprintf("%.1f%%", 100 * v[,1] / nrow(x))
plot(v)

# Why are there backouts that are not early nor late???
subset(data, has_backout & !has_early_backout & !has_late_backout)$bug
subset(events, bug == "92264")[, c("label", "prev_bug_status")]

# Proportion of bugs with withdrawn bug fixes (any type of withdrawal)
mean(data$has_review_minus | data$has_backout | data$has_reopen)

# List for typing on http://www.eulerdiagrams.org/eulerAPE/
l <- list(A=sum(data$has_review_minus), B=sum(data$has_backout), C=sum(data$has_reopen), "A&B"=sum(data$has_review_minus & data$has_backout), "A&C"=sum(data$has_review_minus & data$has_reopen), "B&C"=sum(data$has_backout & data$has_reopen), "A&B&C"=sum(data$has_review_minus & data$has_backout & data$has_reopen))
l

# List of early and late backouts
l <- list(A=sum(data$has_early_backout), B=sum(data$has_backout), C=sum(data$has_late_backout), "A&B"=sum(data$has_early_backout & data$has_backout), "A&C"=sum(data$has_early_backout & data$has_late_backout), "B&C"=sum(data$has_backout & data$has_late_backout), "A&B&C"=sum(data$has_early_backout & data$has_backout & data$has_late_backout))
l

