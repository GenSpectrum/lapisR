runQuery <- function(endpoint, session, args) {

  url <- paste0(session$host, endpoint)
  response <- httr::POST(url, body = body, httr::content_type_json(), encode = "json")

  if (httr::status_code(response) != 200) {
    # TODO improve error handling
    stop("Request failed with status code: ", httr::status_code(response))
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)

  if (session$expireOnUpdate) {
    previousDataVersion <- session$dataVersion
    currentDataVersion <- parsedContent$info$dataVersion
    if (is.null(previousDataVersion)) {
      session$dataVersion <<- currentDataVersion
    } else if (previousDataVersion != currentDataVersion) {
      stop(paste0("The session has expired due to an update of data. The data version was changed from ", previousDataVersion, " to ", currentDataVersion, ". Please create a new session."))
    }
  }

  return(parsedContent$data)
}

getAggregated <- function(session, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/aggregated", session, args))
}

getDetails <- function(session, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/details", session, args))
}

getNucleotideMutations <- function(session, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/nucleotide", session, args))
}

initialize <- function(host, expireOnUpdate) {
  session <- list(
    host = host,
    expireOnUpdate = expireOnUpdate
  )
  return(session)
}


session <- initialize("https://s1.int.genspectrum.org/gisaid", expireOnUpdate = TRUE)

getAggregated(session, country = "Switzerland")
getDetails(session, country = "Switzerland", limit = 10)


