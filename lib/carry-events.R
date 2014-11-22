library(Rcpp)
source("na.R")
source("shift.R")

if (!exists("carryForward", mode="function")) {
  cppFunction('
  CharacterVector carryForward(CharacterVector vec, LogicalVector which, LogicalVector restart) {
    int n = vec.size();
    CharacterVector res(n);
    
    String last = NA_STRING;
    
    for (int i = 0; i < n; i++) {
      String current;
      
      if (which[i]) {
        current = vec[i];
      }
      else if (restart[i]) {
        current = NA_STRING;
      }
      else {
        current = last;
      }
  
      res[i] = current;
      last = current;
    }
    
    return res;
  }
  ')
}

if (!exists("carryForwardDate", mode="function")) {
  cppFunction('
  DateVector carryForwardDate(DateVector vec, LogicalVector which, LogicalVector restart) {
    int n = vec.size();
    DateVector res(n);
    
    Date last = NA_REAL;
    
    for (int i = 0; i < n; i++) {
      Date current;
      
      if (which[i]) {
        current = vec[i];
      }
      else if (restart[i]) {
        current = NA_REAL;
      }
      else {
        current = last;
      }
  
      res[i] = current;
      last = current;
    }
    
    return res;
  }
  ')
}

carry.events <- function(events, valid, column="currentvalue") {
  env <- as.environment(events)
  parent.env(env) <- parent.frame()
  
  vec <- column
  if (mode(column) == "character" && length(column) == 1)
    vec <- events[[column]]
  class.vec <- class(vec)
  
  x <- NULL
  if (is.character(vec)) {
    x <- carryForward(vec, eval(substitute(valid), env, parent.frame()), na.as.true(prevv(events$bug) != events$bug))
  } else if ("POSIXct" %in% class(vec)) {
    x <- carryForwardDate(vec, eval(substitute(valid), env, parent.frame()), na.as.true(prevv(events$bug) != events$bug))
  } else {
    stop(paste("carry.events: Vector has non supported class(es): ", class(vec)))
  }
  class(x) <- class(vec)
  x
}

################

# Creates a new function by replacing a word in the source code of the original function.
# Obs.: original is a regular expression. See gsub.
#
# Ex.:
# plus <- function(a, b) { return(a+b) }
# minus <- .meta.fn.by.replacing(plus, "\\+", "-")
.meta.fn.by.replacing <- function(fn, original, replacement, envir=parent.frame()) {
  eval(parse(text=gsub(original, replacement, deparse(fn))), envir=envir)
}

# For each bug, selects the last event that matches the condition,
# compute columns and merge to the bugs data.frame.
#
# By default, this function returns the bugs data.frame augmented by the
# new columns. This behavior can be modified:
# * if include.orig.cols=FALSE, only the new columns are returned
# * if all.rows=FALSE, only the bugs that match the condition are returned
#
# Ex.: within.bug.last(bugs, events, field == 'resolution', x=time, y=user)
# will add two columns (x and y) with information gathered from the last resolution
# of each bug.
# within.bug.last <- function(bugs, events, condition, ...) {
#   .within.generic.first.or.last(bugs, events, condition, substitute(list(...)), first.or.last="last", id.column="bug")
# }
within.bug.last <- function(bugs, events, condition, ..., include.orig.cols=T, all.rows=T, check.unique=F) {
  class.bugs <- class(bugs)
  rows <- eval(substitute(condition), events, parent.frame())
  all.with.condition <- events[rows, ]
  if (check.unique && anyDuplicated(all.with.condition$bug)) {
    ids <- all.with.condition$bug[duplicated(all.with.condition$bug)]
    print(ids)
    stop("Event is not unique!")
  }
  last.with.condition <- subset(all.with.condition, na.as.true(
    nextv(all.with.condition$bug) != all.with.condition$bug))
  new.columns <- eval(substitute(list(...)), last.with.condition, parent.frame())
  x <- cbind.data.frame(bug=last.with.condition$bug, new.columns)
  for (name in names(new.columns))
    bugs[, name] <- NULL
  m <- merge(bugs, x, all.x=all.rows)
  class(m) <- class.bugs
  
  if (include.orig.cols)
    return(m)
  else
    return(m[, c("bug", names(new.columns))])
}

within.bug.first <- .meta.fn.by.replacing(within.bug.last, "nextv", "prevv")

within.episode.last <- .meta.fn.by.replacing(within.bug.last, "\\$bug", "$bug.episode")
within.episode.last <- .meta.fn.by.replacing(within.episode.last, "bug =", "bug.episode =")

within.episode.first <- .meta.fn.by.replacing(within.episode.last, "nextv", "prevv")