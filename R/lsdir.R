#' List the Directories/Folders on a Path
#'
#' This function produces a character vector of the names of directories on
#' the named path.
#'
#' @param path a character vector of full path names; the default corresponds to "the working directory "."
#' @param format Should the directories be returned as "fullpath", "relative", or "basename"?
#' @param recursive logical. Should the listing recurse into directories?
#' @param all logical. Should hidden files be included?
#'
#' @return A character vector containing the names of the directories in the
#' specified path
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' lsdir(".", all = TRUE)
#'
#' @export
lsdir <- function(path, format = "relative", recursive = FALSE, all = FALSE) {

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
