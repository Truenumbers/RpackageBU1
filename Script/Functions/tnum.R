#' Truenumber utility functions

require("httr")
require("jsonlite")

tnum.var.nspace <- ""
tnum.var.nspaces <- ""
tnum.var.token = ""
tnum.var.ip = ""

#' Title
#'
#' @param ip 
#'
#' @return
#' @export
#'
#' @examples
tnum.authorize <- function(ip = "54.166.186.11") {
  assign("tnum.ip", ip, envir = .GlobalEnv)
  result <- POST(
    paste0("http://", ip, "/v1/gateway/"),
    body = paste0('{"email":"admin@truenumbers.com"}'),
    accept("application/json"),
    content_type("application/json")
  )
  
  token <- content(result)$data$token
  assign("tnum.var.token", token, envir = .GlobalEnv)
  ## get list of numberspaces
  result <- GET(paste0("http://", ip, "/v1/numberspace/"),
                add_headers(Authorization = paste0("Bearer ", token)))
  nspaces <- list()
  
  for (x in content(result)$data) {
    nspaces <- append(nspaces, x[[2]])
  }
  assign("tnum.var.nspace", nspaces[[1]], envir = .GlobalEnv)
  assign("tnum.var.nspaces", nspaces, envir = .GlobalEnv)
  returnValue(nspaces)
  
}

#' Title
#'
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
tnum.setspace <- function(name = "testspace") {
  if (name %in% tnum.nspaces) {
    assign("tnum.var.nspace", name, envir = .GlobalEnv)
  } else {
    stop("server has no such numberspace")
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tnum.getspace <- function() {
  returnValue(tnum.var.nspace)
}


#' Title
#'
#' @param query 
#' @param max 
#' @param start 
#'
#' @return
#' @export
#'
#' @examples
tnum.query <- function(query = "* has *",
                       max = 10,
                       start = 0) {
  args <-
    list(
      numberspace = tnum.var.nspace,
      limit = max,
      offset = start,
      tnql = query
    )
  
  result <-
    content(GET(
      paste0("http://", tnum.var.ip, "/v1/numberspace/numbers"),
      query = args,
      add_headers(Authorization = paste0("Bearer ", tnum.var.token))
    ))
  message(paste0("Returned ", result$data$meta$records, " truenumbers"))
  returnValue(result$data$truenumbers)
  
}
