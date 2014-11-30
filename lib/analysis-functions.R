library(Hmisc)
library(zoo)
source('na.R', chdir=T)

#
# `groupped_bug_data`: the result of 
#
#     bug_data %>% group_by(some_criteria...) %>% mutate(num_days = ...)
#
# Please note that we require a column called `num_days` with the number of
# days within each group, or a column called `month` in the format "2014-12".
#
compute_summary <- function(groupped_bug_data) {
  ret <- groupped_bug_data %>%
    summarise(
      fixes = n(),
      num_days = ifelse(
        exists("num_days", groupped_bug_data),
        max(num_days),
        sum(monthDays(unique(as.Date(paste0(month, "-01")))))
        ),
      fixes_per_day = fixes / num_days,
      #
      reopen_count = sum(has_reopen),
      reopen_rate = mean(has_reopen),
      reopens_per_day = reopen_count / num_days,
      #
      backout_count = sum(has_backout),
      backout_rate = mean(has_backout),
      backouts_per_day = backout_count / num_days,
      #
      early_backout_count = sum(has_early_backout),
      early_backout_rate = mean(has_early_backout),
      early_backouts_per_day = early_backout_count / num_days,
      #
      late_backout_count = sum(has_late_backout),
      late_backout_rate = mean(has_late_backout),
      late_backouts_per_day = late_backout_count / num_days,
      #
      review_minus_count = sum(has_review_minus),
      review_minus_rate = sum(has_review_ask & has_review_minus) / sum(has_review_ask),
      review_minus_per_day = review_minus_count / num_days,
      review_plus_rate = sum(has_review_ask & has_review_plus) / sum(has_review_ask),
      review_ask_rate = mean(has_review_ask),
      #
      median_days_to_fix = median(days_to_fix, na.rm=T),
      median_days_to_refix = median(days_to_refix, na.rm=T),
      median_p_days_to_refix = median(days_to_refix / days_to_fix, na.rm=T),
      median_days_to_backout = median(days_to_backout, na.rm=T),
      median_days_to_early_backout = median(days_to_early_backout, na.rm=T),
      median_days_to_late_backout = median(days_to_late_backout, na.rm=T),
      #
      median_days_to_reopen = median(days_to_reopen, na.rm=T),
      median_days_to_buildok = median(days_to_buildok, na.rm=T),
      median_p_days_to_rebuildok = median(days_to_rebuildok / days_to_buildok, na.rm=T),
      median_days_to_rebuildok = median(days_to_rebuildok, na.rm=T),
      #
      # Bad fix etc.
      median_days_to_badfix = median(ifelse(has_backout, days_to_fix, NA), na.rm=T),
      median_p_days_to_badfix = median_days_to_badfix / median_days_to_fix,
      #
      median_days_to_badbuildok = median(ifelse(has_reopen, days_to_buildok, NA), na.rm=T),
      median_p_days_to_badbuildok = median_days_to_badbuildok / median_days_to_buildok,
      #
      median_days_to_badbuildok = median(ifelse(has_reopen, days_to_buildok, NA), na.rm=T),
      median_p_days_to_badbuildok = median_days_to_badbuildok / median_days_to_buildok,      
      #
      # review
      median_days_to_review_ask = median(days_to_review_ask, na.rm=T),
      median_days_to_review_plus = median(days_to_review_plus, na.rm=T),
      median_days_to_review_minus = median(days_to_review_minus, na.rm=T),
      #
      #
      mean_days_to_fix = mean(days_to_fix, na.rm=T),
      mean_days_to_backout = mean(days_to_backout, na.rm=T),
      mean_days_to_reopen = mean(days_to_reopen, na.rm=T),
      mean_days_to_buildok = mean(days_to_buildok, na.rm=T))

  invisible(ret)
}

between <- function(x, min, max) {
  x >= min & x <= max
}
group_by_release_type <- function(df) {
  ret <- df %>%
    mutate(
      month = strftime(time, "%Y-%m"),
      release_type = ifelse(
        month %>% between('2009-02', '2011-02'), 'planned',
        ifelse(month %>% between('2011-07', '2013-07'), 'rapid',
        NA))) %>%
    group_by(release_type) %>%
    filter(!is.na(release_type))
  invisible(ret)
}

stars_for_pvalue <- function(pvalue) {
  stars <- ""
  if (pvalue < 0.001) {
    stars <- "***"
  } else if (pvalue < 0.01) {
    stars <- "**"
  } else if (pvalue < 0.05) {
    stars <- "*"
  }
  stars
}
rowify_binary <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type()
  planned <- z[z$release_type == 'planned',][[column]]
  rapid <- z[z$release_type == 'rapid',][[column]]
  pvalue <- fisher.test(z$release_type, z[[column]])$p.value

  strvalues <- c(
    title,
    "",
    sprintf("%3.2f%%", mean(z[[column]], na.rm=T) * 100),
    "",
    sprintf("%3.2f%%", mean(planned, na.rm=T) * 100),
    sprintf("%3.2f%%", mean(rapid, na.rm=T) * 100),
    stars_for_pvalue(pvalue))
  strvalues
}
rowify_continuous <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type()
  planned <- z[z$release_type == 'planned',][[column]]
  rapid <- z[z$release_type == 'rapid',][[column]]
  pvalue <- wilcox.test(planned, rapid)$p.value

  strvalues <- c(
    title,
    sprintf("%3.2f", quantile(z[[column]], 0.25, na.rm=T)),
    sprintf("%3.2f", quantile(z[[column]], 0.50, na.rm=T)),
    sprintf("%3.2f", quantile(z[[column]], 0.75, na.rm=T)),
    sprintf("%3.2f", median(planned, na.rm=T)),
    sprintf("%3.2f", median(rapid, na.rm=T)),
    stars_for_pvalue(pvalue)
    )
  strvalues
}
rowify_count <- function(title, x, reference_time_column, column) {
  z <- x %>% mutate_("time" = reference_time_column) %>% group_by_release_type() %>% compute_summary()
  planned <- z[z$release_type == 'planned',]
  rapid <- z[z$release_type == 'rapid',]
  overall <- (planned[[column]] * planned$num_days + rapid[[column]] * rapid$num_days) / (planned$num_days + rapid$num_days)

  strvalues <- c(
    title,
    "",
    sprintf("%3.2f", overall),
    "",
    sprintf("%3.2f", planned[[column]]),
    sprintf("%3.2f", rapid[[column]]),
    "(N/A)")
  strvalues
}

myplot <- function(data, y, x, ylim=NULL, ...) {
  if (is.null(ylim)) {
    ylim <- c(0, max(data[[y]]))
  }

  plot(data[[x]], data[[y]], type='l', ylim=ylim, xlab=x, ylab=y, ...)

  abline(v=as.yearmon('2011-03-22'), lty=2) # start development of version 5
  abline(v=as.yearmon('2011-06-08'), lty=2) # start of mozilla-inbound

  lo <- loess(data[[y]] ~ seq(data[[x]]))
  lines(y=predict(lo), x=data[[x]], lwd=0.5, lty=2, ...)
}
mylines <- function(data, y, x, ...) {
  lines(data[[x]], data[[y]], ...)
  lo <- loess(data[[y]] ~ seq(data[[x]]))
  lines(y=predict(lo), x=data[[x]], lwd=0.5, lty=2, ...)
}
