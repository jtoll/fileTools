#' No Clobber
#'
#' A function which attempts to prevent the errant overwriting of files by
#' checking for their existence and if they already exist, either asking how to
#' proceed, timestamping, or incrementing the filename.
#'
#' @param filename a character vector containing a single filename.
#' @param behavior either "ask", "auto", or "stop".
#' @param form either "timestamp" or "increment".
#'
#' @return A character vector representing a unique filename.
#'
#' @author James C. Toll, \email{james@@jtoll.com}
#'
#' @examples
#' noclobber("filename.csv")
#'
#' @export
noclobber <- function(filename, behavior = c("ask", "auto","stop"), form = c("timestamp", "increment")) {
    # Try to prevent accidental overwriting of files

    behavior <- match.arg(tolower(behavior), c("ask", "auto", "stop"))
    form <- match.arg(tolower(form), c("timestamp", "increment"))

    if ((behavior == "ask")) {

        if (file.exists(filename)) {
            cat(paste("Warning: '", filename, "' already exists!\n", sep = ""))
            answer <- match.arg(tolower(readline("Overwrite: [yes/no/increment/timestamp]? ")),
                                c("yes", "no", "increment", "timestamp"))
            if (answer == "yes") {
                return(filename)
            } else if (answer == "no") {
                stop("File exists, nothing done!")
            } else {
                noclobber(filename, behavior = "auto", form = answer)
            }
        } else {
            return(filename)
        }

    } else if ((behavior == "auto")) {
        if ((form == "timestamp")) {
            return(.fTimestamp(filename))
        } else if ((form == "increment")) {
            return(.fIncrement(filename))
        }
    } else if ((behavior == "stop")) {
        if (file.exists(filename)) {
            stop("File exists, nothing done!")
        } else {
            return(filename)
        }
    }
}

.fIncrement <- function(filename) {
    # while filename exists increment the filename

    part1 <- unlist(strsplit(filename, "\\.\\w{1,4}$"))
    part2 <- sub(part1, "", filename)

    ext <- 1

    while (file.exists(paste(part1, "-", ext, part2, sep = ""))) {
        ext <- ext + 1
    }

    return(paste(part1, "-", ext, part2, sep = ""))
}

.fTimestamp <- function (filename) {
    # add timestamp prefix to filename
    # Mac OS X has issues with : (colons) in the time, therefore use - instead
    return(paste(format(Sys.time(), "%Y-%m-%dT%H-%M-%S"), filename, sep = "-"))
}
