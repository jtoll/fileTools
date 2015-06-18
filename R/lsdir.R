lsdir <- function(path, format = "relative", recursive = FALSE, all = FALSE) {
  # list directories
  # format is any part of "fullpath", "relative", or "basename"

  # set a path if necessary
  if (missing(path)) {
    path <- "."
  }

  # recursion
  if (recursive == FALSE) {
    argRecursive <- "-maxdepth 1"
  } else if (recursive) {
    argRecursive <- ""
  }

  # piece together system command
  execFind <- paste("find", path, "-type d", argRecursive,
                    "-mindepth 1 -print", sep = " ")

  # execute system command
  tmp <- system(execFind, intern = TRUE)

  # remove .hidden files if all == FALSE
  if (all == FALSE) {
    tmp <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)]
  }

  # match format argument
  format <- match.arg(tolower(format), c("relative", "fullpath", "basename"))

  # format output based upon format argument
  if (format == "basename") {
    out <- basename(tmp)
  } else if (format == "fullpath") {
    out <- normalizePath(tmp)
  } else {
    out <- tmp
  }

  # clean up any duplicate "/"
  out <- gsub("/+", "/", out)

  # return
  return(out)

}


