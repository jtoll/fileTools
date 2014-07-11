# Copyright © 2013 James Toll

timestamp <- function (filepath) {
  # rename a file (or directory), prepending a timestamp of the modification
  # time to the name of the file 
  
  if (file.exists(filepath)) {
    
  filename <- basename(filepath)
  filedir <- dirname(filepath)
  
  newfilepath <- file.path(filedir,
                           paste(format(file.info(filepath)$mtime,
                                        "%Y-%m-%dT%H-%M-%S"),
                                 filename,
                                 sep = "-"))
  
  file.rename(filepath, newfilepath)
  } else {
    return(FALSE)
  }
  
}


timestampr <- function (path = ".") {
  # add timestamps to all the non-hidden filenames in the recursive path
  
  # the following pattern filters out hidden dot files (i.e. .hiddenfile)
  targets <- list.files(path,
                        pattern = "^[^\\.]",
                        all.files = TRUE,
                        full.names = TRUE,
                        recursive = TRUE)
  
  # iterate through the list of targets adding the timestamp
  result <- sapply(targets, timestamp)
  
  # return a vector of any failures
  return(targets[!result])
  
}


