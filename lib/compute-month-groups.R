{
  min_date <- '2009-01-01'
  max_date <- '2013-07-01'

# group_by_month <- function(df) {
#   ret <- df %>%
#     mutate(month = strftime(time, "%Y-%m")) %>%
#     filter(month %>% between('2009-02', '2013-07')) %>%
#     group_by(month)
#   invisible(ret)
# }
# TODO: adapta code below to use function group_by_month

  data_month_first_fix <- bug_data %>%
    filter(time_first_fix >= min_date & time_first_fix < max_date) %>%
    mutate(month = strftime(time_first_fix, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_buildok <- bug_data %>%
    filter(time_first_buildok >= min_date & time_first_buildok < max_date) %>%
    mutate(month = strftime(time_first_buildok, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_reopen <- bug_data %>%
    filter(time_first_reopen >= min_date & time_first_reopen < max_date) %>%
    mutate(month = strftime(time_first_reopen, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))
  
  data_month_first_backout <- bug_data %>%
    filter(time_first_backout >= min_date & time_first_backout < max_date) %>%
    mutate(month = strftime(time_first_backout, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_create <- bug_data %>%
    filter(time_create >= min_date & time_create < max_date) %>%
    mutate(month = strftime(time_create, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))

  data_month_first_review_ask <- bug_data %>%
    filter(time_first_review_ask >= min_date & time_first_review_ask < max_date) %>%
    mutate(month = strftime(time_first_review_ask, "%Y-%m")) %>%
    group_by(month) %>%
    mutate(num_days = monthDays(as.Date(paste0(month, "-01")))) %>%
    compute_summary() %>%
    mutate(month = as.yearmon(month))
}