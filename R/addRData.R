addRData <- function(robj, filename, ...) {
  # A simple pseudo-append function that loads all the
  # file contents, adds the object, and saves the result.
  # See also ?readRDS

  tmpEnv <- new.env()

  oldObjNames <- load(filename, envir = tmpEnv)

  newObjName <- deparse(substitute(robj))

  # quick check for name collisions
  stopifnot(!(newObjName %in% oldObjNames))

  allObjNames <- c(oldObjNames, newObjName)

  save(list = allObjNames), file = filename, envir = tmpEnv, ...)

  cat("Saved: ")
  cat(allObjNames)

  rm(tmpEnv)
}
