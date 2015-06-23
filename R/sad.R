#' SAD (i.e Seek And Destroy)
#'
#' Find objects matching the pattern in the specified environment and remove.
#'
#' @param pattern a character vector containing a regular expression to be matched.
#' @param envir the global environment is the default.
#' @param all.names whether to include hidden (.) files in the search.
#'
#' @return A vector of object names matching the pattern.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' sad("*-tmp")
#'
#' @export
sad <- function (pattern, envir = .GlobalEnv, all.names = FALSE, functions = FALSE, ...) {

    # find objects matching pattern in specified environment
    matches <- ls(envir = envir, all.names = all.names)[grep(pattern,
                                                             ls(envir = envir, all.names = all.names), ...)]

    # filter out functions
    if (!functions) {
        matches <- matches[!(sapply(matches, function(y) is.function(get(y))))]
    }

    # print matches and confirm whether to remove objects
    answer <- match.arg(tolower(readline(cat("Remove (y/n)?", matches, sep = "  "))), c("yes", "no"))

    # interpret input and execute accordingly
    if (answer == "no") {
        invisible(matches)
    } else if (answer == "yes") {
        rm(list = matches, envir = envir)
        invisible(matches)
    } else {
        cat("Sorry, unable to interpret your input.\n")
        sad(pattern = pattern, envir = envir, all.names = all.names)
    }

}
