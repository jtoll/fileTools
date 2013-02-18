# Copyright © 2011 James Toll

sad <- function (pattern, envir = .GlobalEnv, all.names = FALSE, functions = FALSE, ...) {
  # (i.e Seek And Destroy) EXTREME WARNING: This function could make you very SAD!
  #
  # Find objects matching pattern in the specified environment and remove.
  #
  # Args:
  #   pattern: A character string containing a regular expression to be matched.
  #   envir: The global environment is the default.
  #   all.names: Whether to include hidden (.) files in the search.
  #
  # Returns:
  #   A vector of object names matching pattern. 
  
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


