library(purrr)

#' Get available sequence filters
#' @param session The current session
#' @return Array of available filter keys
#' @examples
#' session <- initialize("https://lapis.cov-spectrum.org/gisaid/v2")
#' filters <- getFilters(session)
#' @export

getFilters <- function(session) {
  filters <- list_c(apply(session$metadata, 1, function(x) if (x["type"] %in% c("int", "float", "date")) c(x["name"], paste0(x["name"], c("From", "To"))) else x["name"]))
  names(filters) <- NULL
  union(filters, c("nucleotideMutations", "nucleotideInsertions", "aminoAcidMutations", "aminoAcidInsertions"))
}

getRequest <- function(url, accessKey = NULL, args = NULL) {
  query <- c("accessKey" = accessKey, args)
  response <- httr::GET(url, query = query)

  content <- httr::content(response, "text", encoding = "UTF-8")
  parsedContent <- jsonlite::fromJSON(content)

  if (httr::status_code(response) != 200) {
    stop(paste0(httr::status_code(response), " ", parsedContent$error$title, ":\n", parsedContent$error$detail))
  }

  return(parsedContent)
}

runQuery <- function(session, endpoint, args, response_type = "json", compression = NULL, out = NULL) {
  body <- c(args, accessKey = session$accessKey)
  url <- paste0(session$host, endpoint)
  if (response_type == "file") {
    body <- c(body, downloadAsFile = T, compression = compression)
    response <- httr::POST(url, body = body, httr::content_type_json(), encode = "json", httr::write_disk(out, overwrite = T), httr::progress())
  } else {
    response <- httr::POST(url, body = body, httr::content_type_json(), encode = "json")
  }

  content <- httr::content(response, "text", encoding = "UTF-8")

  if (httr::status_code(response) != 200) {
    parsedContent <- jsonlite::fromJSON(content)
    stop(paste0(httr::status_code(response), " ", parsedContent$error$title, ":\n", parsedContent$error$detail))
  }

  if (response_type == "json") content <- jsonlite::fromJSON(content)$data

  if (session$expireOnUpdate) {
    previousDataVersion <- session$dataVersion
    currentDataVersion <- httr::headers(response)[["lapis-data-version"]]
    if (is.null(previousDataVersion)) {
      session$dataVersion <<- currentDataVersion
    } else if (previousDataVersion != currentDataVersion) {
      stop(paste0(
        "The session has expired due to an update of data. The data version was changed from ",
        previousDataVersion, " to ", currentDataVersion, ". Please create a new session."
      ))
    }
  }

  if (response_type != "file") {
    return(content)
  }
}

parseArguments <- function(..., method = c("GET", "POST")) {
  args <- list(...)
  argNames <- names(args)
  if (any(argNames == "")) {
    stop("Unnamed argument are not allowed.")
  }
  duplicatedNames <- unique(argNames[duplicated(tolower(argNames))])
  if (length(duplicatedNames) > 0) {
    stop(paste0("Duplicated arguments: ", paste0(duplicatedNames, collapse = ", ")))
  }

  if (method == "GET") {
    for (arg in argNames) {
      if (length(args[[arg]]) > 1) {
        values <- as.list(args[[arg]])
        names(values) <- rep(arg, length(values))
        args[[arg]] <- NULL
        args <- c(args, values)
      }
    }
  } else {
    if (!is.null(args[["fields"]])) args[["fields"]] <- as.array(args[["fields"]])
    if (!is.null(args[["orderBy"]])) args[["orderBy"]] <- as.array(args[["orderBy"]])
    for (f in c("nucleotideMutations", "nucleotideInsertions", "aminoAcidMutations", "aminoAcidInsertions")) {
      if (!is.null(args[[f]])) args[[f]] <- as.list(args[[f]])
    }
  }

  return(args)
}

#' Get aggregated data from LAPIS
#'
#' Makes a request to the /sample/aggregated endpoint of LAPIS and parses the response
#' @param session The current session
#' @param fields The fields to aggregate by. Valid field names can be found in `session$metadata$name`. If NULL, only the total count is returned.
#' @param orderBy The fields to order by; must be either "count" or included in `fields`
#' @param limit Maximum number of sequences to include
#' @param offset Number of sequences to skip
#' @param ... Filters. Valid filter keys can be found with `getFilters(session)`
#' @return Aggregated data
#' @examples
#' agg_data <- getAggregated(session, fields = "dateMonth", country = "Switzerland")
#' @export
getAggregated <- function(session, fields = NULL, orderBy = NULL, limit = NULL, offset = NULL, ...) {
  if (!is.null(fields) && any(!fields %in% session$metadata$name)) {
    stop(paste0("Invalid fields: ", paste0(fields[!fields %in% session$metadata$name], collapse = ", ")))
  }
  if (!is.null(orderBy) && any(!orderBy %in% c("count", fields))) {
    stop('orderBy values must be either "count" or in fields')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  # params<-  parseArguments(fields=fields, orderBy=orderBy, limit=limit, offset=offset, method='GET')
  params <- parseArguments(fields = fields, orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  # filters <- parseArguments(..., method='GET')
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }

  # return(getRequest(paste0(session$host, "/sample/aggregated"), accessKey = session$accessKey, c(params, filters))$data)
  return(runQuery(session, "/sample/aggregated", c(params, filters)))
}

#' Get metadata from LAPIS
#'
#' Makes a request to the /sample/details endpoint of LAPIS and parses the response
#' @param session The current session
#' @param fields The fields to include in the response. Valid field names can be found in `session$metadata$name`. If NULL, all fields are returned.
#' @param orderBy The fields to order by; must be present in `fields`
#' @param limit Maximum number of sequences to include
#' @param offset Number of sequences to skip
#' @param ... Filters. Valid filter keys can be found with `getFilters(session)`
#' @return Sequence metadata
#' @examples
#' metadata <- getDetails(session, fields = c("host", "age"), limit = 10, country = "Switzerland")
#' @export
getDetails <- function(session, fields = NULL, orderBy = NULL, limit = NULL, offset = NULL, ...) {
  if (!is.null(fields) && any(!fields %in% session$metadata$name)) {
    stop(paste0("Invalid fields: ", paste0(fields[!fields %in% session$metadata$name], collapse = ", ")))
  }
  if (!is.null(orderBy) && any(!orderBy %in% fields)) {
    stop("orderBy values must be present in fields")
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  params <- parseArguments(fields = fields, orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }


  return(runQuery(session, "/sample/details", c(params, filters)))
}

#' Get nucleotide mutations from LAPIS
#'
#' Makes a request to the /sample/nucleotideMutations endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields to order by. Available values: "mutation", "count", "proportion", "random"
#' @param limit Maximum number of mutations to include
#' @param offset Number of mutations to skip
#' @param minProportion Minimal proportion required to include a mutation in the response
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Nucleotide mutations together with their counts and proportions among the sequences passing the filters
#' @examples
#' mutations <- getNucleotideMutations(session, region = "Europe", minProportion = 0.1, limit = 10)
#' @export
getNucleotideMutations <- function(session, orderBy = NULL, limit = NULL, offset = NULL, minProportion = 0.05, ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("mutation", "count", "proportion", "random"))) {
    stop('orderBy values must be in `c("mutation", "count", "proportion", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  if (!(is.numeric(minProportion) && minProportion >= 0 && minProportion <= 1)) stop("minProportion must be a number between 0 and 1")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, minProportion = minProportion, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }


  return(runQuery(session, "/sample/nucleotideMutations", c(params, filters)))
}

#' Get nucleotide insertions from LAPIS
#'
#' Makes a request to the /sample/nucleotideInsertions endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields to order by. Available values: "insertion", "count", "random"
#' @param limit Maximum number of insertions to include
#' @param offset Number of insertions to skip
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Nucleotide insertions together with their counts among the sequences passing the filters
#' @examples
#' insertions <- getNucleotideInsertions(session, region = "Europe", limit = 10)
#' @export
getNucleotideInsertions <- function(session, orderBy = NULL, limit = NULL, offset = NULL, ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("insertion", "count", "random"))) {
    stop('orderBy values must be in `c("insertion", "count", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }


  return(runQuery(session, "/sample/nucleotideInsertions", c(params, filters)))
}

#' Get amino acid mutations from LAPIS
#'
#' Makes a request to the /sample/aminoAcidMutations endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields to order by. Available values: "mutation", "count", "proportion", "random"
#' @param limit Maximum number of mutations to include
#' @param offset Number of mutations to skip
#' @param minProportion Minimal proportion required to include a mutation in the response
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Amino acid mutations together with their counts and proportions among the sequences passing the filters
#' @examples
#' mutations <- getAminoAcidMutations(session, region = "Europe", minProportion = 0.1, limit = 10)
#' @export
getAminoAcidMutations <- function(session, orderBy = NULL, limit = NULL, offset = NULL, minProportion = 0.05, ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("mutation", "count", "proportion", "random"))) {
    stop('orderBy values must be in `c("mutation", "count", "proportion", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  if (!(is.numeric(minProportion) && minProportion >= 0 && minProportion <= 1)) stop("minProportion must be a number between 0 and 1")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, minProportion = minProportion, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }


  return(runQuery(session, "/sample/aminoAcidMutations", c(params, filters)))
}

#' Get amino acid insertions from LAPIS
#'
#' Makes a request to the /sample/aminoAcidInsertions endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields to order by. Available values: "insertion", "count", "random"
#' @param limit Maximum number of insertions to include
#' @param offset Number of insertions to skip
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Amino acid insertions together with their counts among the sequences passing the filters
#' @examples
#' insertions <- getAminoAcidInsertions(session, region = "Europe", limit = 10)
#' @export
getAminoAcidInsertions <- function(session, orderBy = NULL, limit = NULL, offset = NULL, ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("insertion", "count", "random"))) {
    stop('orderBy values must be in `c("insertion", "count", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }


  return(runQuery(session, "/sample/aminoAcidInsertions", c(params, filters)))
}

#' Get amino acid sequence alignment from LAPIS
#'
#' Makes a request to the /sample/alignedAminoAcidSequences/{gene} endpoint of LAPIS and parses the response
#' @param session The current session
#' @param gene The gene of interest. Valid values can be found in `session$genes`
#' @param orderBy The fields by which the sequences should be ordered. Available values: "gisaidEpiIsl", "random" and the `gene`
#' @param limit Maximum number of sequences to align
#' @param offset Number of sequences to skip
#' @param downloadAsFile If the alignment should be written to file. If FALSE, the aligment is returned as string
#' @param compression Compression type. Available compression types: "gzip" and "zstd". Ignored if `downloadAsFile == FALSE`. If NULL, no compression
#' @param out Name of the output file (without file extension)
#' @param ... Sequence filters.Valid filter keys can be found with `getFilters(session)`
#' @return Amino acid sequence alignment in FASTA format
#' @examples
#' getAminoAcidAlignment(session, "ORF1a", country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, out = "aa_alignment")
#' @export
getAminoAcidAlignment <- function(session, gene, orderBy = NULL, limit = NULL, offset = NULL, downloadAsFile = T, compression = NULL, out = "aa_alignment", ...) {
  if (!(gene %in% session$genes)) {
    stop("Unsupported gene name")
  }
  if (!is.null(orderBy) && any(!orderBy %in% c("gisaidEpiIsl", "random", gene))) {
    stop(paste0('orderBy values can be "gisaidEpiIsl", "random", or ', gene))
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  if (!is.null(compression) && !(compression %in% c("zstd", "gzip"))) stop("Unsupported compression type")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }
  if (downloadAsFile) {
    if (!is.null(compression)) {
      if (compression == "zstd") {
        suffix <- ".zst"
      } else {
        suffix <- ".gz"
      }
    } else {
      suffix <- ""
    }
    return(runQuery(session, paste0("/sample/alignedAminoAcidSequences/", gene), c(params, filters), response_type = "file", compression = compression, out = paste0(out, ".fasta", suffix)))
  } else {
    return(runQuery(session, paste0("/sample/alignedAminoAcidSequences/", gene), c(params, filters), response_type = "string"))
  }
}

#' Get nucleotide sequence alignment from LAPIS
#'
#' Makes a request to the /sample/alignedNucleotideSequences endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields by which the sequences should be ordered. Available values: "main", "gisaidEpiIsl", "random"
#' @param limit Maximum number of sequences to align
#' @param offset Number of sequences to skip
#' @param downloadAsFile If the alignment should be written to file. If FALSE, the aligment is returned as string
#' @param compression Compression type. Available compression types: "gzip" and "zstd". Ignored if `downloadAsFile == FALSE`. If NULL, no compression
#' @param out Name of the output file (without file extension)
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Nucleotide sequence alignment in FASTA format
#' @examples
#' getNucleotideAlignment(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, out = "nuc_alignment")
#' @export
getNucleotideAlignment <- function(session, orderBy = NULL, limit = NULL, offset = NULL, downloadAsFile = T, compression = NULL, out = "nuc_alignment", ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("main", "gisaidEpiIsl", "random"))) {
    stop('orderBy values must be in `c("main", "gisaidEpiIsl", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  if (!is.null(compression) && !(compression %in% c("zstd", "gzip"))) stop("Unsupported compression type")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }

  if (downloadAsFile) {
    if (!is.null(compression)) {
      if (compression == "zstd") {
        suffix <- ".zst"
      } else {
        suffix <- ".gz"
      }
    } else {
      suffix <- ""
    }
    return(runQuery(session, "/sample/alignedNucleotideSequences", c(params, filters), response_type = "file", compression = compression, out = paste0(out, ".fasta", suffix)))
  } else {
    return(runQuery(session, "/sample/alignedNucleotideSequences", c(params, filters), response_type = "string"))
  }
}

#' Get nucleotide sequences from LAPIS
#'
#' Makes a request to the /sample/unalignedNucleotideSequences endpoint of LAPIS and parses the response
#' @param session The current session
#' @param orderBy The fields by which the sequences should be ordered. Available values: "main", "gisaidEpiIsl", "random"
#' @param limit Maximum number of sequences to include
#' @param offset Number of sequences to skip
#' @param downloadAsFile If the sequences should be written to file. If FALSE, the sequences are returned as string
#' @param compression Compression type. Available compression types: "gzip" and "zstd". Ignored if `downloadAsFile == FALSE`. If NULL, no compression
#' @param out Name of the output file (without file extension)
#' @param ... Sequence filters. Valid filter keys can be found with `getFilters(session)`
#' @return Nucleotide sequences in FASTA format
#' @examples
#' getNucleotideSequences(session, country = "Poland", dateDay = 5, dateMonth = 5, limit = 3, out = "lapis_sequences")
#' @export
getNucleotideSequences <- function(session, orderBy = NULL, limit = NULL, offset = NULL, downloadAsFile = T, compression = NULL, out = "sequences", ...) {
  if (!is.null(orderBy) && any(!orderBy %in% c("main", "gisaidEpiIsl", "random"))) {
    stop('orderBy values must be in `c("main", "gisaidEpiIsl", "random")`')
  }
  if (!is.null(limit) && !(is.numeric(limit) && round(limit) == limit)) stop("Limit must be integer")
  if (!is.null(offset) && !(is.numeric(offset) && round(offset) == offset)) stop("Offset must be integer")
  if (!is.null(compression) && !(compression %in% c("zstd", "gzip"))) stop("Unsupported compression type")
  params <- parseArguments(orderBy = orderBy, limit = limit, offset = offset, method = "POST")
  filters <- parseArguments(..., method = "POST")
  if (any(!names(filters) %in% getFilters(session))) {
    stop(paste0("Unknown arguments: ", paste0(names(filters)[!names(filters) %in% getFilters(session)], collapse = ", ")))
  }

  if (downloadAsFile) {
    if (!is.null(compression)) {
      if (compression == "zstd") {
        suffix <- ".zst"
      } else {
        suffix <- ".gz"
      }
    } else {
      suffix <- ""
    }
    runQuery(session, "/sample/unalignedNucleotideSequences", c(params, filters), response_type = "file", compression = compression, out = paste0(out, ".fasta", suffix))
  } else {
    return(runQuery(session, "/sample/unalignedNucleotideSequences", c(params, filters), response_type = "string"))
  }
}

#' Initialize LAPIS session
#'
#' Creates a session
#' @param host URL of the host
#' @param accessKey Access key (optional)
#' @param expireOnUpdate If TRUE, the session expires as soon as the database receives an update
#' @return A LAPIS session
#' @examples
#' session <- initialize("https://lapis.cov-spectrum.org/gisaid/v2", expireOnUpdate = TRUE)
#' @export
initialize <- function(host, accessKey = NULL, expireOnUpdate = F) {
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
    expireOnUpdate = expireOnUpdate,
    accessKey = accessKey
  )
  return(session)
}
