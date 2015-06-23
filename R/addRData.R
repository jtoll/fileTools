#' Append to an R Data File
#'
#' A pseudo-append function that loads all the contents of an R data file,
#' adds the object, and saves the result as a new R data file.
#'
#' @param robj The R object to be added to the file.
#' @param filename The RData file to be appended.
#' @param ... Additional arguments to save.
#'
#' @return None; Saves an RData file
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#' @seealso \code{\link[base]{readRDS}}
#'
#' @examples
#' addRData(x, "file.RData")
#'
#' @export
addRData <- function(robj, filename, ...) {

    tmpEnv <- new.env()

    oldObjNames <- load(filename, envir = tmpEnv)

    newObjName <- deparse(substitute(robj))

    # quick check for name collisions
    stopifnot(!(newObjName %in% oldObjNames))

    allObjNames <- c(oldObjNames, newObjName)

    save(list = allObjNames, file = filename, envir = tmpEnv, ...)

    cat("Saved: ")
    cat(allObjNames)

    rm(tmpEnv)
}
