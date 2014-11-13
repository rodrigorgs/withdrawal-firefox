# Adapted from package delftfews to support factors
shift.vector <- function (v, by)  {
  class.v <- class(v)
  if (by == 0 || length(v) == 0)
    return(v)
  
  if (by >= length(v)) {
    x <- rep.int(NA, length(v))
  }
  else {  
    x <- NULL
    if (by >= 0)
      x <- c(NA + 1:by, v[1:(length(v) - by)])
    else 
      x <- c(v[(-by + 1):(length(v) - by)])
    
    if (is.factor(v))
      x <- factor(x, labels=levels(v))
  }
  
  if (!is.factor(x))
    mode(x) <- mode(v)
  class(x) <- class.v
  x
}

# helpers
nextv <- function(x, step=1) { shift.vector(x, by=-step) }
prevv <- function(x, step=1) { shift.vector(x, by=step)}

shift.data.frame <- function(d, by) {
  if (by == 0)
    d
  else if (by >= 0)
    rbind(NA + 1:by, d[1:(nrow(d) - by), ])
  else rbind(d[(-by + 1):(nrow(d) - by), ])
}