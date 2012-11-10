# Copyright © 2012 James Toll
# normalizePath("..")
# path.expand("~")

lsdir <- function(path, names = "relative", all = FALSE, recursive = FALSE) {
  # a function to list directories that actually works
  
  # set a path if necessary
  if (missing(path)) {
    path <- "."
  }

#   # isolate the first character of the path for testing
#   pathID <- strsplit(path, "")[[1]][1]
#   
#   # test whether the path is absolute or relative
#   if (pathID == "/" || pathID == "~"){
#     absolute <- TRUE
#   } else {
#     absolute <- FALSE
#   }
  
  # recursion
  if (recursive == FALSE) {
    argRecursive <- "-maxdepth 1"
  } else if (recursive) {
    argRecursive <- ""
  }
  
  # piece together system command
  execFind <- paste("find", path, "-type d", argRecursive, "-mindepth 1 -print", sep = " ")
  
  # execute system command 
  tmp <- system(execFind, intern = TRUE)
  
  # remove hidden files if all == FALSE
  if (all == FALSE) {
    tmp <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)]
  }
  
  # match names argument
  names <- match.arg(tolower(names), c("fullpath", "relative", "basename"))
  
  # format output based upon names argument
  if (names == "basename") {
    out <- basename(tmp)
  } else if (names == "fullpath") {     #  && absolute == FALSE
#    out <- paste(getwd(), gsub("\\./", "", tmp), sep = "/")
    out <- normalizePath(tmp)
  } else {
    out <- tmp
  }
  
  # clean up redundant "/" and return
  return(gsub("/+", "/", out))
  
}