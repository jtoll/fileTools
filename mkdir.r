# Copyright © 2012 James Toll

# source noclobber.r
if (!exists("noclobber")) {
  source("~/src/r/fileTools/noclobber.r")
}

mkdir <- function(path, find = TRUE, ...) {
  # make a directory, defaulting to today's date
  
  # if no path is specified, use today's date
  if (missing(path)) {
    path <- as.character(Sys.Date())
  }

  # avoid collisions
  x <- noclobber(path)
  
  # create the directory
  dir.create(x, ...)

  # return path invisibly
  invisible(x)

}
