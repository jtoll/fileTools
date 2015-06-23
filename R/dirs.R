#' dirs
#'
#' Return a vector of \strong{just} the directories in a path.
#'
#' @param path a character vector containing a single path name.
#'
#' @return a vector of directories
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' dirs()
#'
#' @export
#'
dirs <- function (path = ".") {

    listAllFiles <- list.files(path = path, full.names = TRUE, recursive = FALSE)
    justDirs <- listAllFiles[file.info(listAllFiles)$isdir]

    return(justDirs)
}
