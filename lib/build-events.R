# Only consider events for bugs that match the following criteria:
#
# * bug's last resolution is FIXED
# * bug received at least one bug fix commit
#
build_events <- function(bugs, event_data, event_labels) {
  stopifnot(nrow(event_data) == nrow(event_labels))
  
  complete <- event_data %>% inner_join(event_labels, by="event")

  bugs_status_fixed <- bugs$bug[bugs$resolution == "FIXED"]
  filtered <- complete %>%
    filter(bug %in% bugs_status_fixed) %>%
    filter(label != '') %>%
    group_by(bug) %>%
    filter(any(label == "fix")) %>%
    arrange(bug, time)

  invisible(filtered)
}