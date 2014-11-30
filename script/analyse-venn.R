########################################
rm(list=ls())
library(gplots)
library(VennDiagram)
library(limma)

bugs_by_creation <- readRDS("../data/firefox-bugs-by-creation.rds")
data <- bugs_by_creation

##

mean(data$has_backout & !data$has_early_backout & !data$has_late_backout)
mean(data$has_early_backout & !data$has_late_backout & data$has_reopen)

# Withdrawal types (limma)
x <- data[, c("has_review_minus", "has_backout", "has_reopen")]
a <- vennCounts(x)
a[, 4] <- sprintf("%.1f%%", 100 * a[, 4] / sum(a[, 4]))
vennDiagram(a, cex=1, names=c("rejections", "backouts", "reopenings"))

# Early backouts, late backouts, and reopening
x <- data[, c("has_early_backout", "has_late_backout", "has_reopen")]
a <- vennCounts(x)
a[, 4] <- sprintf("%.1f%%", 100 * a[, 4] / sum(a[, 4]))
vennDiagram(a, cex=1, names=c("early backouts", "late backouts", "reopenings"))

events <- readRDS("../data/firefox-event-labels.rds")
subset(data, has_early_backout & !has_late_backout & has_reopen)$bug
subset(events, bug == "477708")$label


# Withdrawal types (gplots)
x <- data[, c("has_review_minus", "has_backout", "has_reopen")]
has_withdrawal <- x$has_review_minus | x$has_backout | x$has_reopen
v <- venn(x)
v[,1] <- sprintf("%.1f%% (%.1f%%)", 100 * v[,1] / nrow(x), 100 * v[,1] / sum(has_withdrawal))
plot(v)

# Backout types
x <- data[, c("has_early_backout", "has_backout", "has_late_backout")]
v <- venn(x)
v[,1] <- sprintf("%.1f%%", 100 * v[,1] / nrow(x))
plot(v)

# Proportion of bugs with withdrawn bug fixes (any type of withdrawal)
mean(data$has_review_minus | data$has_backout | data$has_reopen)

# List for typing on http://www.eulerdiagrams.org/eulerAPE/
l <- list(A=sum(data$has_review_minus), B=sum(data$has_backout), C=sum(data$has_reopen), "A&B"=sum(data$has_review_minus & data$has_backout), "A&C"=sum(data$has_review_minus & data$has_reopen), "B&C"=sum(data$has_backout & data$has_reopen), "A&B&C"=sum(data$has_review_minus & data$has_backout & data$has_reopen))
l

# List of early and late backouts
l <- list(A=sum(data$has_early_backout), B=sum(data$has_backout), C=sum(data$has_late_backout), "A&B"=sum(data$has_early_backout & data$has_backout), "A&C"=sum(data$has_early_backout & data$has_late_backout), "B&C"=sum(data$has_backout & data$has_late_backout), "A&B&C"=sum(data$has_early_backout & data$has_backout & data$has_late_backout))
l

############

saveEls <- function(a, b, c, file) {
  values = c(sum(a), sum(b), sum(c), sum(a&b), sum(a&c), sum(b&c), sum(a&b&c))

  line1 <- c("//a | b | c | ab | ac | bc | abc")  
  line2 <- paste(values, collapse=' | ')

  lines <- rbind(line1, line2)
  writeLines(lines, file)
}

if (1 == 0) {
  a <- data$has_review_minus
  b <- data$has_backout
  c <- data$has_reopen
  x <- saveEls(a, b, c, file="/tmp/x.els")

  system("java -jar /tmp/eulerAPE_3.0.0.jar -i /tmp/x.els -o /tmp")
  system("cp /tmp/x.svg")
}