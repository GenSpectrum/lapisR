accessKey <- "9Cb3CqmrFnVjO3XCxQLO6gUnKPd"

#' GET request to LAPIS
#'
#' Makes a GET request to a LAPIS endpoint and parses the response
#' @param url Request URL
#' @param ... Additional filters
#' @return The parsed content of the response
#' @examples 
#' agg_data <- getRequest("https://lapis.cov-spectrum.org/gisaid/v2/sample/aggregated", country='Switzerland')
#' @export
getRequest <- function(url, ...) {
  query<-list("accessKey" = accessKey, ...)
  response <- httr::GET(url, query=query)
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)

  if (httr::status_code(response) != 200) {
    stop(paste0(httr::status_code(response), ' ', parsedContent$error$title, ':\n', parsedContent$error$detail))
  }

  return(parsedContent)
}

#' Querying LAPIS data
#'
#' Makes a POST request to a LAPIS endpoint and parses the response
#' @param session The current session
#' @param endpoint The endpoint where the request should be made
#' @param args Arguments to be included in the request body
#' @return The parsed content of the response
#' @examples 
#' data <- runQuery(session, "/sample/nucleotideMutations")
#' @export
runQuery <- function(session, endpoint, args) {
  body <- c(args, accessKey = accessKey)
  url <- paste0(session$host, endpoint)
  response <- httr::POST(url, body = body, httr::content_type_json(), encode = "json")
  
  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)
  
  if (httr::status_code(response) != 200) {
    stop(paste0(httr::status_code(response), ' ', parsedContent$error$title, ':\n', parsedContent$error$detail))
  }
  
  if (session$expireOnUpdate) {
    previousDataVersion <- session$dataVersion
    currentDataVersion <- parsedContent$info$dataVersion
    if (is.null(previousDataVersion)) {
      session$dataVersion <<- currentDataVersion
    } else if (previousDataVersion != currentDataVersion) {
      stop(paste0("The session has expired due to an update of data. The data version was changed from ",
                  previousDataVersion, " to ", currentDataVersion, ". Please create a new session."))
    }
  }
  
  return(parsedContent$data)
}

#' Get aggregated data from LAPIS
#'
#' Makes a request to the /sample/aggregated endpoint of LAPIS and parses the response
#' @param session The current session
#' @param ... Additional filters
#' @return The parsed content of the response
#' @examples 
#' agg_data <- getAggregated(session, country = "Switzerland")
#' @export
getAggregated <- function(session, ...) {
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(getRequest(paste0(session$host, "/sample/aggregated"),  ...))
}

#' Get metadata from LAPIS
#'
#' Makes a request to the /sample/details endpoint of LAPIS and parses the response
#' @param session The current session
#' @param fields Fields to include
#' @param limit Maximal number of entries to return
#' @param ... Additional filters
#' @return Metadata table
#' @examples 
#' metadata <- getDetails(session, fields=c("country"), limit = 10, country = "Switzerland")
#' @export
getDetails <- function(session, fields = NULL, limit = NULL, ...) {
  filters <- parseFilters(...)
  combined <- c(
    filters$metadata,
    filters$mutation,
    fields=list(as.list(fields)),
    limit=limit
  )
  return(runQuery(session, "/sample/details", combined))
}

#' Get nucleotide mutations from LAPIS
#'
#' Makes a request to the /sample/nucleotideMutations endpoint of LAPIS and parses the response
#' @param session The current session
#' @param ... Filters
#' @return Nucleotide mutations table
#' @examples 
#' mutations <- getNucleotideMutations(session, region = "Europe", minProportion = 0.1)
#' @export
getNucleotideMutations <- function(session, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery(session, "/sample/nucleotideMutations", args))
}

parseFilters <- function(...) {
  parsed <- ... |>
    parseEllipsis() |>
    parseMetadataFilters(session) |>
    parseMutationFilters()
  if (length(parsed$unparsed) > 0) {
    stop(paste0("Unknown arguments: ", paste0(parsed$unparsed, collapse = ", ")))
  }
  return(parsed)
}

parseEllipsis <- function(...) {
  args <- list(...)
  argNames <- names(args)
  if (any(argNames == "")) {
    stop("Unnamed argument are not allowed.")
  }
  duplicatedNames <- argNames[duplicated(tolower(argNames))]
  if (length(duplicatedNames) > 0) {
    stop(paste0("Duplicated arguments: ", paste0(duplicatedNames, collapse = ", ")))
  }
  return(list(unparsed=args))
}

parseMetadataFilters <- function(args, session) {
  metadataArgs <- names(args$unparsed) %in% session$metadata$name
  args$metadata <- args$unparsed[metadataArgs]
  args$unparsed <- args$unparsed[!metadataArgs]
  return(args)
}

parseMutationFilters <- function(args) {
  # TODO If sarsCoV2VariantQuery is enabled, variantQuery should be supported
  mutationArgs <- names(args$unparsed) %in% c(
    "nucleotideMutations", "aminoAcidMutations",
    "nucleotideInsertions", "aminoAcidInsertions"
  )
  args$mutation <- lapply(args$unparsed[mutationArgs], as.list)
  args$unparsed <- args$unparsed[!mutationArgs]
  return(args)
}

#' Initialize LAPIS session
#'
#' Creates a session
#' @param host URL of the host
#' @param expireOnUpdate If TRUE, the session expires as soon as the database receives an update
#' @return A LAPIS session
#' @examples 
#' session <- initialize("https://lapis.cov-spectrum.org/gisaid/v2", expireOnUpdate = TRUE)
#' @export
initialize <- function(host, expireOnUpdate) {
  databaseConfig <- getRequest(paste0(host, "/sample/databaseConfig"))
  referenceGenomeConfig <- getRequest(paste0(host, "/sample/referenceGenome"))
  session <- list(
    instanceName = databaseConfig$schema$instanceName,
    primaryKey = databaseConfig$schema$primaryKey,
    metadata = databaseConfig$schema$metadata,
    nucleotideSequences = referenceGenomeConfig$nucleotideSequences$name,
    genes = referenceGenomeConfig$genes$name,
    multiSegmented = length(referenceGenomeConfig$nucleotideSequences$name) > 1,
    host = host,
    expireOnUpdate = expireOnUpdate
  )
  return(session)
}

