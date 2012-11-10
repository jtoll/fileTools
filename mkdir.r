# Copyright © 2012 James Toll

mkdir <- function(path, find = TRUE, ...) {
  # make a directory, defaulting to today's date
  
  # if no path is specified, use today's date
  if (missing(path)) {
    path <- as.character(Sys.Date())
  }
  
  if (file.exists(path)) {
    if (find == TRUE) {
      ext <- try(which(file.exists(paste(path, "-", 1:100, sep = "")) == FALSE)[1])
      mkdir(paste(path, "-", ext, sep = ""), find = TRUE, ...)
    } else {
      stop(paste("mkdir: ", path, ": File exists", sep = ""))
    }
  } else {
    # create the directory
    dir.create(path, ...)

    # return path invisibly
    invisible(path)
  }

}


