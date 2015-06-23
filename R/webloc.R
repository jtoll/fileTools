#' Webloc
#'
#' Read the .webloc files created by web browsers as URL bookmarks into a
#' dataframe. NOTE: This does not work with XML formatted .webloc files.
#'
#' @param path a character vector containing a single path name.
#' @param recursive logical.
#'
#' @return A dataframe of URLs.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' webloc()
#'
#' @export
webloc <- function(path = ".", recursive = FALSE) {

    do <- function(filename) {
        # this function reads and returns the data from
        # each .webloc file

        X <- scan(filename, what = "", sep = "\n", quiet = TRUE, encoding = "UTF-8")

        if (length(X) > 1) { return(NA) }

        findBeg <- regexpr("http", X)
        begin <- as.integer(findBeg) - 1

        findEnd <- regexpr("\b\v\017", X)
        end <- as.integer(findEnd) - attr(findEnd, "match.length") + 1

        substr(X, begin, end)

    }

    # list all webloc files
    x <- list.files(path = path, pattern = "*.webloc", recursive = recursive)

    # iterate through each file and collect data into a data.frame
    robj <- data.frame(DESCRIPTION = sub(".webloc", "", basename(x)),
                       URL = unlist(lapply(x, do)),
                       stringsAsFactors = FALSE)

    # return
    return(robj)
}
