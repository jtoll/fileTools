# Copyright © 2012 James Toll
# https://gist.github.com/1622504

library(RJSONIO)
library(RCurl)

getGists <- function(login) {
  # pull or clone ALL the public gists for a particular github login
  
  # if (missing(login)) {
  #   login <- as.character(Sys.info()['login'])
  # }
  
  URL <- paste("https://api.github.com/users/", login, "/gists", sep = "")
  
  stopifnot(url.exists(URL))
  
  document <- fromJSON(getURL(URL))
  stopifnot(length(document) > 0)
  
  all_id <- sapply(document, function(x) x$id)
  # all_html_url <- sapply(document, function(x) x$html_url)
  # all_git_pull_url <- sapply(document, function(x) x$git_pull_url)
  
  do <- function (x) {
    
    if (file.exists(x)) {
      setwd(x)
      system(paste("git pull git://gist.github.com/", x, ".git", sep = ""))
      setwd("../")
      return("pull")
    } else {
      system(paste("git clone git://gist.github.com/", x, ".git", sep = ""))
      return("clone")
    }
  }
  
  return(sapply(all_id, do))
  
}
