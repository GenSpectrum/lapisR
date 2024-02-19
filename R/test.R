accessKey <- ""

getRequest <- function(url) {
  url <- paste0(url, "?accessKey=", accessKey)
  response <- httr::GET(url)

  if (httr::status_code(response) != 200) {
    # TODO improve error handling
    stop("Request failed with status code: ", httr::status_code(response))
  }

  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)

  return(parsedContent)
}

runQuery <- function(endpoint, session, args) {
  body <- c(args, accessKey = accessKey)

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
      stop(paste0("The session has expired due to an update of data. The data version was changed from ",
                  previousDataVersion, " to ", currentDataVersion, ". Please create a new session."))
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

getDetails <- function(session, fields = NULL, limit = NULL, ...) {
  filters <- parseFilters(...)
  combined <- c(
    filters$metadata,
    filters$mutation,
    fields=list(as.list(fields)),
    limit=limit
  )
  return(runQuery("/sample/details", session, combined))
}

getNucleotideMutations <- function(session, ...) {
  args <- list(...)
  # TODO validate the arguments
  # TODO convert arguments as needed: e.g., fields must be an array even it has only one element

  return(runQuery("/sample/nucleotideMutations", session, args))
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

session <- initialize("https://s1.int.genspectrum.org/gisaid", expireOnUpdate = TRUE)

getAggregated(session, country = "Switzerland")
getDetails(session, country = "Switzerland", limit = 10, fields=c("country"))
getNucleotideMutations(session, region = "Europe", minProportion = 0.1)

