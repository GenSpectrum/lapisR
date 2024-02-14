library(dplyr)

runQuery <- function(endpoint, client, args) {
  body <- c(args, accessKey = "")

  url <- paste0(client$host, endpoint)
  response <- httr::POST(url, body = body, httr::content_type_json(), encode = "json")

  if (httr::status_code(response) != 200) {
    # TODO improve error handling
    stop("Request failed with status code: ", httr::status_code(response))
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)

  if (client$expireOnUpdate) {
    previousDataVersion <- client$dataVersion
    currentDataVersion <- parsedContent$info$dataVersion
    if (is.null(previousDataVersion)) {
      client$dataVersion <<- currentDataVersion
    } else if (previousDataVersion != currentDataVersion) {
      stop(paste0("The session has expired due to an update of data. The data version was changed from ", previousDataVersion, " to ", currentDataVersion, ". Please create a new session."))
    }
  }

  return(parsedContent$data)
}

getAggregated <- function(client, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/aggregated", client, args))
}

getDetails <- function(client, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/details", client, args))
}

getNucleotideMutations <- function(client, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/nucleotide", client, args))
}

initialize <- function(host, expireOnUpdate) {
  client <- list(
    host = host,
    expireOnUpdate = expireOnUpdate
  )
  return(client)
}


session <- initialize("https://s1.int.genspectrum.org/gisaid", expireOnUpdate = TRUE)

# With pipe
session %>% getAggregated(country = "Switzerland", orderBy = "count")
session %>% getDetails(country = "Switzerland", limit = 10)

# Without pipe
getAggregated(session, country = "Switzerland")


