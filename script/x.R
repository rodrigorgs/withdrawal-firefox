# TODO: should we treat "review+ => review?" (clearing review flag) and "review? => feedback"?

rm(list=ls())
library(dplyr)
library(stringr)
# library(tabplot)
library(PerformanceAnalytics)

event_data <- readRDS("../data/firefox-event-data.rds")
reviews <- readRDS("../data/firefox-reviews.rds")

event_data$label <- NULL
event_data <- event_data %>% left_join(reviews, by="event")

###

head(event_data)

event_data$period <- as.yearmon(event_data$time) %>% as.numeric()
minperiod <- min(event_data$period)
maxperiod <- max(event_data$period)
{
  x <- event_data %>%
    group_by(period) %>%
    summarise(
      num_fixes = sum(field == "fix"),
      num_backouts = sum(field == "backout"),
      p_backouts = num_backouts / num_fixes,

      num_reviewminus = sum(label == "review-", na.rm=T),
      num_reviewask = sum(label == "review_ask", na.rm=T),
      p_reviewminus = num_reviewminus / num_reviewask,

      num_statusfix = sum(currentvalue == "FIXED"),
      num_statusreopened = sum(currentvalue == "REOPENED"),
      p_reopened = num_statusreopened / num_statusfix
      ) %>%
    arrange(period) %>%
    filter(period != minperiod & period != maxperiod)
}
fixers <- event_data %>%
  filter(field == "fix") %>%
  group_by(period) %>%
  summarise(
    num_fixers = n_distinct(user)
    ) %>%
  arrange(period) %>%
  filter(period != minperiod & period != maxperiod)


plot(num_fixes ~ period, data=x, type='l', col=1, ylim=c(0, max(num_fixes)))
lines(I(num_backouts*10) ~ period, data=x, col=2)
lines(I(num_fixers*2.5) ~ period, data=fixers, col=3)
lines(I(num_reviewminus*2) ~ period, data=x, col=4)
lines(I(num_statusreopened*10) ~ period, data=x, col=5)
legend('topleft', c('fixes', 'backouts', 'fixers', 'review-', 'reopened'), col=1:5, lwd=1)

plot(I(p_reviewminus) ~ period, data=x, type='l', col=1, ylim=c(0, 0.2))
lines(I(p_backouts*2) ~ period, data=x, col=2)
lines(I(p_reopened* 0.7) ~ period, data=x, col=3)
legend('topleft', c('review-', 'backout', 'reopened'), col=c(1, 2, 3), lwd=1)

plot(I((num_backouts + num_statusreopened) / num_fixes) ~ period, data=x, type='l')
plot(I((num_backouts + num_statusreopened)) ~ period, data=x, type='l')


  # x <- month_data; x$month <- rank(x$month); x <- x[, !sapply(x, is.character)]
  # x <- x %>% select(
  #   month,
  #   nfixers,
  #   fixes_per_day,
  #   overall_backout_rate,
  #   late_backout_rate,
  #   early_backout_rate,
  #   median_time_to_backout)
x %>% select(num_statusfix, num_statusreopened, num_fixes, num_backouts, num_reviewminus, num_reviewask) %>%
  chart.Correlation(method='spearman')


############################################

# How many bugs were reopened but not backed out -- and vice-versa?
# Oops! we should only look for reopen after fix (or even after commit)

z <- event_data %>%
  group_by(bug) %>%
  summarise(
    reopened = any(currentvalue == 'REOPENED'),
    backed_out = any(field == "backout"))

t <- xtabs(~reopened + backed_out, data=z)
t