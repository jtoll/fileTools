#' Get Gists
#'
#' Get all of the public gists for a particular github username.
#'
#' @param username A Github username.
#'
#' @return A list of the Gist names and whether they were cloned or pulled.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#' @references \url{https://gist.github.com/1622504}
#'
#' @examples
#' getGists("jtoll")
#'
#' @export
#' @importFrom RCurl getURL
#' @importFrom RJSONIO fromJSON
#'
getGists <- function(username) {

    # if (missing(username)) {
    #   username <- as.character(Sys.info()['login'])
    # }

    URL <- paste("https://api.github.com/users/", username, "/gists", sep = "")

    stopifnot(url.exists(URL))

    document <- fromJSON(getURL(URL))
    stopifnot(length(document) > 0)

    all_id <- sapply(document, function(x) x$id)
    # all_html_url <- sapply(document, function(x) x$html_url)
    # all_git_pull_url <- sapply(document, function(x) x$git_pull_url)

    do <- function (x) {

        if (file.exists(paste(x, ".git/config", sep = "/"))) {
            setwd(x)
            system(paste0("git pull git://gist.github.com/", x, ".git"))
            setwd("../")
            return("pull")
        } else {
            system(paste0("git clone git://gist.github.com/", x, ".git"))
            return("clone")
        }
    }

    return(sapply(all_id, do))

}
