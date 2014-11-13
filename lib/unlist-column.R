# Generalized version of http://stackoverflow.com/questions/22128300/normalize-data-frame-with-list-column
unlist.column <- function(df, k, v, key.name=substitute(k), value.name=substitute(v)) {
  key <- eval(substitute(k), df, parent.frame())
  value <- eval(substitute(v), df, parent.frame())
  ret <- data.frame(x = rep(key, sapply(value, length)),
      y = unlist(value), stringsAsFactors=F)
  ret <- reshape::rename(ret, c("x" = key.name, "y" = value.name))
  ret
}