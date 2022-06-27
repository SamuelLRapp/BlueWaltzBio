import(utils)
import(shiny)
import(rentrez)
import(taxize)
import(RSQLite)
import(dplyr)
import(modules)
import(future)
import(promises)
import(ipc)

orgListHelper <- modules::use("orgListHelper.R")


# parse a csv file and append the list from column column.header
# to what's already in the text box. Returns this value as a vector.
parseCsvColumnForTxtBox <- function(input, file.index, column.header, textbox.id) {
  req(input[[file.index]], file.exists(input[[file.index]]$datapath))
  uploadinfo <- read.csv(input[[file.index]]$datapath)
  
  # preserve any organism names already in the text box
  if (input[[textbox.id]][[1]] == "") {
    uploadinfo[[column.header]][uploadinfo[[column.header]] != ""]
  } else{
    c(
      input[[textbox.id]], 
      head(uploadinfo[[column.header]][uploadinfo[[column.header]] != ""]))
  }
}

# returns the search parameters depending on which genome
# is selected and whether search for reference sequences
# is selected in the tool.
getNcbiSearchParameters <- function(selectedOption, refSeqChecked) {
  if (selectedOption == 'Full mitochondrial genomes in NCBI Nucleotide') {
    parameters <- 
      (" AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN]")
    if (refSeqChecked) {
      parameters <- paste(parameters, "AND srcdb_refseq[PROP]", sep=" ")
    }
  } else if (selectedOption == 'Full chloroplast genomes in NCBI Nucleotide') {
    parameters <- (" AND Chloroplast[TITL] AND 120000:170000[SLEN]")
    if (refSeqChecked) {
      parameters <- paste(parameters, "AND srcdb_refseq[PROP]", sep=" ")
    }
  } else if (selectedOption == "Number of entries per taxa in NCBI Genome") {
    parameters <- ""
  } else {
    stop("Attempted to get search parameters for unknown genome type")
  }
  parameters
}

# returns a vector of column names for a dataframe
# based on which search genome is selected in the tool.
getColumnNames <- function(selectedOption){
  switch(
    selectedOption,
    "Full mitochondrial genomes in NCBI Nucleotide" = 
      c('Num_Mitochondrial_Genomes_in_NCBI_Nucleotide', 'SearchStatements'),
    "Full chloroplast genomes in NCBI Nucleotide" = 
      c('Num_RefSeq_Chloroplast_Genomes_in_NCBI_Nucleotide', 
        'SearchStatements'),
    "Number of entries per taxa in NCBI Genome" = 
      c('present_in_NCBI_Genome', 'GenomeDB_SearchStatements'))
}

# NCBI still rate limits with a key, to 10/s
sleep <- function() {
  if (ncbiKeyIsValid) {
    Sys.sleep(0.1)
  } else {
    Sys.sleep(0.34)
  }
}

# returns either "nucleotide" or "genome,"
# based on which option is selected in tool,
# to pass to the entrez_search function.
getDbToSearch <- function(selectedOption){
  switch(
    selectedOption,
    "Full mitochondrial genomes in NCBI Nucleotide" = "nucleotide",
    "Full chloroplast genomes in NCBI Nucleotide" = "nucleotide",
    "Number of entries per taxa in NCBI Genome" = "genome")
}


# Takes a db name from getDbToSearch, the genomeList
# containing the genomes to search for, a parameters string
# containing the parameters to pass to the entrez_search function
# and a vector of columnNames for results dataframe.
# Returns a two element list where the first element
# is a dataframe containing the results of all NCBI searches. 
# The columns are labeled with parameter columnNames. 
# The second list item is a vector of uids retrieved from the
# NCBI search.
getNcbiSearchResults <- 
  function(dbToSearch, genomeList, parameters, columnNames) {
    Results <- data.frame(matrix(0, ncol = 2, nrow = length(genomeList)))
    names(Results) <- columnNames
    uids <- c()
    if (0 < length(genomeList)) {
      for (i in 1:length(genomeList)){
        sleep()
        searchTerm <- paste0('', genomeList[i], '[ORGN]', parameters, '')
        searchResult <- tryCatch({            
          genome_result <- entrez_search(
            db = dbToSearch,
            term = searchTerm,
            retmax = 5)
          Results[i, 1] <- genome_result$count 
          Results[i, 2] <- searchTerm
          for (id in genome_result$ids) {
            uids <- c(uids, id) 
          }
        }, error = function(err) {
          Results[i, 1] <<- "Error"
          Results[i, 2] <<- "Error"
        })
      }


    }
    list(Results, uids)
  }

# Takes dbOption, the selection from the dropdown menu
# "choose which genome to search for;" orgList, the raw
# list from the organism names text box; taxizeOption, the 
# logical value of the "check spelling..." checkbox; and 
# refSeqChecked, the logical value of the "search for 
# reference sequences" checkbox. 
# Returns a two element list. First value is another two
# element list containing the matrix of search results and 
# a vector of uids (see getNcbiSearchResults), and the second 
# value is the vector of organism names processed from the 
# organism name text box.
getGenomeSearchFullResults <- function(dbOption, orgList, taxizeOption, refSeqChecked) {
  databaseOption <- getDbToSearch(dbOption)
  genomeList <- orgListHelper$taxizeHelper(orgList, taxizeOption)
  dfColumnNames <- getColumnNames(dbOption)
  parameters <- getNcbiSearchParameters(dbOption, refSeqChecked)
  list(getNcbiSearchResults(
    databaseOption,
    genomeList,
    parameters,
    dfColumnNames),
    genomeList)
}

# Sets local variable ncbiKeyIsValid.
# Does not check if the provided key
# is valid, this should be done 
# before calling this function.
ncbiKeyIsValid <- FALSE
setNcbiKeyIsValid <- function(validity = TRUE) {
  ncbiKeyIsValid <<- validity
}


# Downloads the file of filetype for each id
# in uids and amalgamates them into a single file
# at filepath. Increments the progress indicator with
# each file downloaded, closes it when finished. 
fullGenomeDownload <- function(filetype, uids, filepath, progressIndicator) {
  future_promise({
    fileVector <- c()
    for (uid in uids) {
      sleep()
      fileVector <- c(fileVector, downloadFileFromNcbi(uid, filetype))
      progressIndicator$inc(amount = 1)
    }
    write(fileVector, filepath)
    progressIndicator$set(value = length(uids))
    progressIndicator$close()
  })
}

# helper function to ping the database for the file
downloadFileFromNcbi <- function(uid, filetype, db="nucleotide") {
  entrez_fetch(
    db = db,
    id = uid,
    rettype = filetype)
}


# maximum number of homonyms to return to the user
maxHomonyms <- 5L

# the list of database tables in the crux database. 
cruxDbList <- list(
  "MB18S", "MB16S", "MBPITS", "MBCO1", "MBFITS", "MBtrnL", "MB12S")

# Takes orgList, the raw list from the organism names text box,
# and taxizeOption, the logical value of the "check spelling..." 
# checkbox.
# Returns a Five element list. First value is a list
# containing the organism names, including homonyms. 
# The second value is the matrix of search results. The
# remaining three values are empty vectors, left in place
# in case the previous homonym failure notification scheme
# is wanted. 
getCruxSearchFullResults <- function(orgList, taxizeOption) {
  genomeList <- orgListHelper$taxizeHelper(orgList, taxizeOption)
  nameUidList <- getHomonyms(genomeList)
  nameList <- nameUidList[[1]]
  uidList <- nameUidList[[2]]
  results <- c()
  for (i in 1:length(nameList)) {
    searchTerm <- getSearchTerm(nameList[i], uidList[i])
    results <- cruxOrgSearch(
      results, searchTerm, nameList[i])
  }
  results <- getCruxResultsMatrix(
    results, length(nameList))
  results <-
    list(
      organismList = nameList,
      data = results,
      popupinfo = c(),
      errorPopupList = c(),
      errorPopupListFound = c()
    )
  results
}


# Loops through and queries the tables in the database for organism.
# searchTerm is used in the params argument to the dbGetQuery call in 
# getTaxaDbQueryResults.
# The database responses are appended to the results parameter,
# then results is returned to the caller.
cruxOrgSearch <- function(results, searchTerm, organism) {
  taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite")
  for (table in cruxDbList) {
    queryStatement <- paste(
      "SELECT * from ",
      table,
      " where regio= :x or phylum= :x or classis= :x or ordo= :x
        or familia= :x or genus= :x or genusspecies= :x")
    results <- c(
      results,
      getTaxaDbQueryResults(
        taxaDB, 
        queryStatement, 
        organism, 
        searchTerm))
  }
  dbDisconnect(taxaDB)
  browser()
  results
}

# transforms the vector of crux results into a matrix
getCruxResultsMatrix <- function(resultsVector, numOrganisms) {
  matrix(
    resultsVector, 
    nrow = numOrganisms, 
    ncol = length(cruxDbList), 
    byrow = TRUE)
}

# asks NCBi for a list of homonyms for each
# organism in organismList. 
# Returns list(newOrganismNamesList, newOrganismUidsList)
# where newOrganismNamesList is a list of the scientific
# names for the organism. If no homonyms were found for an 
# organism, the corresponding uid will be the empty string,
# since the uid is unknown here in that case.
getHomonyms <- function(organismList) {
  validate(
    need(length(organismList) > 0, 'Please name at least one organism'))
  newOrganismNamesList <- c()
  newOrganismUidsList <- c()
  for (organism in organismList) {
    
    # get_uid_ calls function get_uid_help,
    # which calls Sys.sleep(0.33). Explicitly sleeping
    # here may not be necessary.
    # https://rdrr.io/cran/taxize/src/R/get_uid.R
    sleep()
    
    # get_uid_ returns null or a data.frame.
    # source at https://rdrr.io/cran/taxize/src/R/get_uid.R
    search <- get_uid_(sci_com=organism, messages=FALSE)
    if (is.null(search[[1]])) {
      newOrganismNamesList <- c(newOrganismNamesList, organism)
      newOrganismUidsList <- c(newOrganismUidsList, "")
    } else {
      for (i in 1:nrow(search[[1]])){
        newOrganismNamesList <- c(
          newOrganismNamesList, search[[1]][["scientificname"]][[i]])
        newOrganismUidsList <- c(
          newOrganismUidsList, search[[1]][["uid"]][[i]])
        if (i > maxHomonyms) {
          break
        }
      }
    }
  }
  list(newOrganismNamesList, newOrganismUidsList)
}

# Checks all taxonomic ranks on the taxa db for a match,
# lowest taxonomic level first.
# Returns the number of rows in the results object as string,
# or the name of the taxonomic rank where a result was found.
getTaxaDbQueryResults <- function(taxaDb, query, organism, searchTerm) {
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = organism))
  if (nrow(location) != 0) {
    return(toString(nrow(location)))
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 3]))
  if (nrow(location) != 0) {
    return("genus")
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 4]))
  if (nrow(location) != 0) {
    return("family")
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 5]))
  if (nrow(location) != 0) {
    return("order")
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 6]))
  if (nrow(location) != 0) {
    return("class")
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 7]))
  toString(nrow(location))
}


# Set up a dataframe to use in the params argument in a call
# to dbGetQuery.
getSearchTerm <- function(organismUid, organismName) {
  sleep()
  if (organismUid == "") {
    tax_name(
      query = organismName,
      get = c("genus", "family", "order", "class", "phylum", "domain"),
      db = "ncbi",
      messages = FALSE)
  } else {
    hierarchy <- classification(organismUid, db = "ncbi")[[1]]
    query <- c(
      "db", 
      "query", 
      "genus", 
      "family", 
      "order", 
      "class", 
      "phylum",
      "domain") 
    match <- hierarchy$name[
      match(
        tolower(c("genus","family","order","class","phylum","domain")), 
        tolower(hierarchy$rank))]
    match <- c("ncbi", organismName, match)
    searchTerm <- stats::setNames(
      data.frame(t(match), stringsAsFactors = FALSE), query)
  }
}

