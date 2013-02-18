# Copyright © 2012, 2013 James Toll

noclobber <- function(filename, behavior = c("ask", "increment", "timestamp", "stop")) {
  # Try to prevent accidental overwriting of files 
  
  if (file.exists(filename)) {
  
    behavior <- match.arg(tolower(behavior), c("ask", "increment", "timestamp", "stop"))
    
    if ((behavior == "ask")) {
      
      cat(paste("Warning: '", filename, "' already exists!\n", sep = ""))
      answer <- match.arg(tolower(readline("Overwrite: [yes/no/increment/timestamp]? ")),
                          c("yes", "no", "increment", "timestamp"))
      
      if (answer == "yes") {
        return(filename)
      } else if (answer == "no") {
        stop("File exists, nothing done!")
      } else {
        noclobber(filename, behavior = answer)
      }

    } else if ((behavior == "increment")) {
      return(.fIncrement(filename))
    } else if ((behavior == "timestamp")) {
      return(.fTimestamp(filename))
    } else {
      stop("File exists, nothing done!")
    } 
    
  } else {
    return(filename)
  }
}

.fIncrement <- function(filename) {
  # while filename exists increment the filename
  
  part1 <- unlist(strsplit(filename, "\\.\\w{1,4}$"))
  part2 <- sub(part1, "", filename)
  
  ext <- 1
  
  while (file.exists(paste(part1, "-", ext, part2, sep = ""))) {
    ext <- ext + 1
  }
  
  return(paste(part1, "-", ext, part2, sep = ""))
}

.fTimestamp <- function (filename) {
  # add timestamp prefix to filename
  # Mac OS X has issues with : (colons) in the time, therefore use - instead
  return(paste(format(Sys.time(), "%Y-%m-%dT%H-%M-%S"), filename, sep = "-"))
}

