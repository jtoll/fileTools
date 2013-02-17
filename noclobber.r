# Copyright © 2012 James Toll

noclobber <- function(filename, behavior = c("ask", "increment", "timestamp", "stop")) {
  # Try to prevent accidental overwriting of files 
  
  if (file.exists(filename)) {
  
    behavior <- match.arg(tolower(behavior), c("ask", "increment", "timestamp", "stop"))
    
    if ((behavior == "ask")) {
      
      cat(paste("Warning: '", filename, "' already exists!\n", sep = ""))
#       cat("Overwrite? [yes/no/increment]")
#       answer <- match.arg(tolower(scan(n = 1, what = "character", quiet = TRUE)),
#                           c("yes", "no", "increment"))
      answer <- match.arg(tolower(readline("Overwrite: [yes/no/increment/timestamp]? ")),
                          c("yes", "no", "increment", "timestamp"))
      
      
      if (answer == "yes") {
        return(filename)
      } else if (answer == "no") {
        stop("Nothing done!", call. = FALSE)
      } else if (answer == "increment") {
        noclobber(filename, behavior = "increment")
      } else if (answer == "timestamp") {
        noclobber(filename, behavior = "timestamp")
      }

    } else if ((behavior == "increment")) {
      
      part1 <- unlist(strsplit(filename, "\\.\\w{1,4}$"))
      part2 <- sub(part1, "", filename)
      
      ext <- 1
      
      while (file.exists(paste(part1, "-", ext, part2, sep = ""))) {
        ext <- ext + 1
      }
      
      noclobber(paste(part1, "-", ext, part2, sep = ""), behavior = "increment")
      
    } else if ((behavior == "timestamp")) {
      return(paste(format(Sys.time(), "%Y-%m-%dT%H-%M-%S"), filename, sep = "-"))
    } else if (behavior == "stop") {
      stop("File already exists!")
    } 
    
  } else {
    return(filename)
  }
}


