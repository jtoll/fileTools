# Copyright © 2012 James Toll

lsdir <- function(path, names = "relative", all = FALSE, recursive = FALSE) {
  
  if (missing(path)) {
    path <- "."
  }
  
  if (recursive == FALSE) {
    argRecursive <- "-maxdepth 1"
  } else if (recursive) {
    argRecursive <- ""
  }

  execFind <- paste("find", path, "-type d", argRecursive, "-mindepth 1 -print", sep = " ")
  
  tmp <- system(execFind, intern = TRUE)
  
  names <- match.arg(tolower(names), c("full", "relative", "base"))
  
  if (names == "relative" && all == TRUE) {
    out <- tmp
  } else if (names == "relative" && all == FALSE) {
    out <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)]
  } else if (names == "base" && all == TRUE) {
    out <- basename(tmp)
  } else if (names == "base" && all == FALSE) {
    out <- basename(tmp)
    out <- out[grep("^\\..*", out, invert = TRUE)]
  } else if (names == "full" && all == TRUE) {
    out <- paste(getwd(), gsub("\\./", "", tmp), sep = "/")
  } else if (names == "full" && all == FALSE) {
    out <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)]
    out <- paste(getwd(), gsub("\\./", "", out), sep = "/")
  }
  
  return(gsub("/+", "/", out))
  
}