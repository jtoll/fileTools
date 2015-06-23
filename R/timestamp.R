#' Timestamp
#'
#' Rename a file (or directory), prepending a timestamp of the modification
#' time to the name of the file.
#'
#' @param filepath a character vector containing a single path name.
#'
#' @return logical; indicating whether the renaming was successful.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' timestamp("filename.RData")
#'
#' @export
timestamp <- function (filepath) {

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

#' Recursive Timestamping
#'
#' Add timestamps to all the non-hidden filenames in the recursive path.
#'
#' @param path a character vector containing a single path name.
#'
#' @return logical; indicating whether the renaming was successful.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' timestampr()
#'
#' @export
timestampr <- function (path = ".") {

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
