# Copyright © 2012 James Toll

lsdir <- function(path, full.names = FALSE, recursive = FALSE, all.files = FALSE){
  
  if (missing(path)) {
    path <- "."
  }
  
  tmp <- list.dirs(path, full.names = full.names, recursive = recursive)
  
  if (full.names == FALSE && all.files == FALSE) {
    out <- basename(tmp)
    out <- out[grep("^\\..*", out, invert = TRUE)]
  } else if (full.names == FALSE && all.files == TRUE) {
    out <- basename(tmp)
  } else if (full.names == TRUE && all.files == FALSE) {
    out <- tmp[grep("^\\..*", basename(tmp), invert = TRUE)]
    out <- paste(getwd(), gsub("\\./", "", out), sep = "/")
  } else if (full.names == TRUE && all.files == TRUE) {
    out <- paste(getwd(), gsub("\\./", "", tmp), sep = "/")
  }

  return(gsub("/+", "/", out))
}