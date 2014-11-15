na.as.false <- function(x) {
  x[is.na(x) | is.null(x)] <- FALSE
  x
}

na.as.true <- function(x) {
  x[is.na(x) | is.null(x)] <- TRUE
  x
}