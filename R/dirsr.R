#' dirsr: a recursive version of dirs
#'
#' Recursively return a vector of \strong{just} the directories in path.
#'
#' @param paths a character vector containing a single path name.
#'
#' @return a vector of directories
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' dirsr()
#'
#' @export
#'
dirsr <- function (paths = ".") {
    # a recursive version of dirs
    # recursively return a vector of the directories in path

    x <- unlist(lapply(paths, dirs))

    xr <- sort(c(x, unlist(lapply(x, dirsr))))

    return(xr)

}
