#' Make a Directory
#'
#' Create a new directory, possibly using a timestamp of today's date as the directory name.
#'
#' @param path The directory path to create.
#' @param find A boolean.
#' @param ... Additional arguments passed to \code{\link[base]{dir.create}}.
#'
#' @return Return the pathname that was created as a string.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' mkdir()
#' mkdir("newDirectory")
#'
#' @export
mkdir <- function(path, find = TRUE, ...) {
    # make a directory, defaulting to today's date

    # if no path is specified, use today's date
    if (missing(path)) {
        path <- as.character(Sys.Date())
    }

    # avoid collisions
    x <- noclobber(path)

    # create the directory
    dir.create(x, ...)

    # return path invisibly
    invisible(x)

}
