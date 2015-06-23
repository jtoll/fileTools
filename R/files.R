#' files
#'
#' Return a vector of \strong{just} the files in a path.
#'
#' @param path a character vector containing a single path name.
#'
#' @return a vector of files
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' files()
#'
#' @export
#'
files <- function (path = ".") {

    listAllFiles <- list.files(path = path, recursive = FALSE)
    justFiles <- listAllFiles[!file.info(file.path(path, listAllFiles))$isdir]

    return(justFiles)
}
