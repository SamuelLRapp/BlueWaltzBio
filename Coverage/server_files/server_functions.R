# -----------------------------------------------------------------------------#
# Name: server_functions.R
# Last Date Updated : September 22, 2024
#
# This code was build by Sriram Ramesh and Jorge Tapias Gomez
# With help from Dickson Chung and Zia Truong
#
# In collaboration with Samuel Rapp, Benjamine Levine, and Daniel Tapias Gomez
# Also, a big thanks to all those that helped us along the project.
# -----------------------------------------------------------------------------#

suppressPackageStartupMessages({
  import(utils)
  import(shiny)
  import(shinyWidgets)
  import(rentrez)
  import(taxize)
  import(RSQLite)
  import(dplyr)
  import(modules)
  import(future)
  import(promises)
  import(ipc)
})

orgListHelper <- tryCatch({modules::use("./server_files/orgListHelper.R")},
                          error = function(err){modules::use("Coverage/server_files/orgListHelper.R")})

# General ----------------------------------------------------------------------

## ----#
# Parse CSV Column For Txt Box
# Parse a csv file and append the list from column column.header
# to what's already in the text box. Returns this value as a vector.
##
#   - Input: 
#         input: The input from the server where everything is stored
#         file.index: The ID of the file
#         column.head: The name of the column. to upload
#         textbox.id: The ID of the textbox to update in the UIs
#   - Output:
#         The data in one of the columns of the CSV as a list 
## ----#
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

## ----#
# Sleep Function
# Since we are being rate limited by NCBI make sure to not go over it
##
#   - Input: 
#         No Inputs
#   - Output:
#         If the NCBI key is valid we only sleep for 0.1 (10 queries per second limit)
#         else we sleep for 0.34 (3 queries per second limit with no key)
## ----#
sleep <- function() {
  if (ncbiKeyIsValid) {
    Sys.sleep(0.1)
  } else {
    Sys.sleep(0.34)
  }
}


# Sets local variable ncbiKeyIsValid to True or False
# Depending on whether the provided key is valid or not.
ncbiKeyIsValid <- FALSE
setNcbiKeyIsValid <- function(validity = TRUE) {
  ncbiKeyIsValid <- validity
}


## ----#
# Summary Report Dataframe
# Converts the dataframe into the right format for displaying
##
#   - Input: 
#         Dataframe: The dataframe with all the Summary Data 
#   - Output:
#         Dataframe that is ready to be rendered in the UI
## ----#
summary_report_dataframe <- function(dataframe)
{
  # colnames dataframe are all the barcodes that come from the search:
  # For CRUX that is: "18S"     "16S"     "PITS"    "CO1"     "FITS"    "trnL"    "Vert12S"
  # For NCBI: Depends on User Input
  # For BOLD: Depends on the barcodes found during the search
  new_row_names <- c("Total", colnames(dataframe))

  # Convert all non-integer entries into 0's
  dataframe <- dataframe %>%
    mutate_all(~ ifelse(is.na(as.integer(.)), 0, as.integer(.)))
  statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(statistics_df) <- c(
    "Barcodes",
    "Number of Sequences Found",
    "Percent of Total Sequences Found",
    "Number of Organisms with at Least one Sequence",
    "Number of Organisms with no Sequences"
  )
  
  # Add up each column representing a barcode and add up every barcode
  barcodeSums <- colSums(dataframe) 
  total_seq_found <- sum(barcodeSums)
  
  # Manually enter known values, total sequences found and total percentage
  statistics_df[1, 1] <- "Total" # Set the Total barcodeName Row
  statistics_df[1, 2] <- total_seq_found
  statistics_df[1, 3] <- 100
  organismPresence <- orgamismPresence(dataframe, -1)
  statistics_df[1, 4] <- length(organismPresence[[1]])
  statistics_df[1, 5] <- length(organismPresence[[2]])

  
  for (i in 2:length(new_row_names))
  {
    x <- i - 1
    statistics_df[i, 1] <- new_row_names[i] # Set the barcodeName Row
    statistics_df[i, 2] <- barcodeSums[x]
    if(total_seq_found == 0){
      statistics_df[i, 3] <- 0
    } else {
      statistics_df[i, 3] <- round(((barcodeSums[x] / total_seq_found) * 100), digits=2)
    }
    organismPresence <- orgamismPresence(dataframe, x)
    statistics_df[i, 4] <- length(organismPresence[[1]])
    statistics_df[i, 5] <- length(organismPresence[[2]])
  }
  statistics_df
}



## ----#
# Organism Presence
# Check for presence of organism i.e. does it have some sequences or not
##
#   - Input: 
#         Dataframe: The dataframe with all the Summary Data 
#         column: Column to check in the dataframe (every column is a barcode)
#   - Output:
#         List of those that have sequences and those who dont
## ----#
orgamismPresence <- function(dataframe, column){
  total <- 0
  ncols <- ncol(dataframe)
  nrows <- nrow(dataframe)
  haveSomeSeq <- c()
  haveZeroSeq <- c()
  for (i in 1:nrows)
  {
    total <- 0
    # Check presence for every barcode/column
    if (column < 0) {
      for (j in 1:ncols)
      {
        total <- total + dataframe[i, j]
      }
    }
    # Only need to check presence one barcode/column
    else {
      total <- total + dataframe[i, column]
    }
    if (total > 0)
    {
      # Add species name to list
      haveSomeSeq <- c(haveSomeSeq, total)
    } else
    {
      # Add species name to list
      haveZeroSeq <- c(haveZeroSeq, total)
    }
  }
  # Return organism presence lists
  results <- list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs = haveZeroSeq)
  results <- as.matrix(results)
}

# CRUX Tab ---------------------------------------------------------------------

# Maximum number of homonyms to return to the user
maxHomonyms <- 5L

# The list of database tables in the crux database. 
cruxDbList <- list(
  "MB18S", "MB16S", "MBPITS", "MBCO1", "MBFITS", "MBtrnL", "MB12S")

# Takes orgList, the raw list from the organism names text box,
# and taxizeOption, the logical value of the "check spelling..." checkbox.
# Returns a five element list. First value is a list
# containing the organism names, including homonyms. 
# The second value is the matrix of search results. The
# remaining three values are empty vectors, left in place
# in case the previous homonym failure notification scheme
# is wanted. Used to have pop-ups for the user.
getCruxSearchFullResults <- function(organismList, progress, homonymFlag) {
  nameUidList <- getHomonyms(organismList, homonymFlag)
  nameList <- nameUidList[[1]]
  uidList <- nameUidList[[2]]
  results <- c()
  nameListLength <- length(nameList)
  progress$set(detail = paste0("0","/",nameListLength))
  for (i in 1:nameListLength) {
    progress$set(message = paste0("Retrieving barcodes for ", nameList[i]))
    progress$inc(amount = 0.5/nameListLength)
    
    searchTerm <- getSearchTerm(nameList[i], uidList[i])
    results <- cruxOrgSearch(
      results, searchTerm, nameList[i])
    
    progress$set(message = paste0("Retrieved barcodes for ", nameList[i]),
                 detail = paste0(i,"/",nameListLength))
    progress$inc(amount = 0.5/nameListLength)
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
  progress$close()
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

# Transforms the vector of crux results into a matrix
getCruxResultsMatrix <- function(resultsVector, numOrganisms) {
  matrix(
    resultsVector, 
    nrow = numOrganisms, 
    ncol = length(cruxDbList), 
    byrow = TRUE)
}

# Asks NCBi for a list of homonyms for each organism in organismList. 
# Returns list(newOrganismNamesList, newOrganismUidsList)
# where newOrganismNamesList is a list of the scientific
# names for the organism. If no homonyms were found for an 
# organism, the corresponding uid will be the empty string,
# since the uid is unknown here in that case.
getHomonyms <- function(organismList, homonymFlag) {
  validate(
    need(length(organismList) > 0, 'Please name at least one organism'))
  newOrganismNamesList <- c()
  newOrganismUidsList <- c()
  for (organism in organismList) {
    
    if (homonymFlag) {
      # get_uid_ calls function get_uid_help,
      # Which calls Sys.sleep(0.33). Explicitly sleeping
      # https://rdrr.io/cran/taxize/src/R/get_uid.R
      sleep()
      
      # get_uid_ returns a data.frame.
      # Source at https://rdrr.io/cran/taxize/src/R/get_uid.R
      search <- get_uid_(sci_com=organism, messages=FALSE)
      if (!is.null(search) && !is.null(search[[1]]) && nrow(search[[1]])>1) {
        newOrganismNamesList <- c(newOrganismNamesList, organism)
        newOrganismUidsList <- c(newOrganismUidsList, "")
        for (i in 1:nrow(search[[1]])){
          newOrg <- paste(organism, search[[1]]$division[i], sep = " ")
          newOrganismNamesList <- c(
            newOrganismNamesList, newOrg)
          newOrganismUidsList <- c(
            newOrganismUidsList, search[[1]][["uid"]][[i]])
          if (i > maxHomonyms) {
            break
          }
        }
      } else {
        newOrganismNamesList <- c(newOrganismNamesList, organism)
        newOrganismUidsList <- c(newOrganismUidsList, "")
      }
    }
    else {
      newOrganismNamesList <- c(newOrganismNamesList, organism)
      newOrganismUidsList <- c(newOrganismUidsList, "")
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
  if (nrow(location) != 0) {
    return("phylum")
  }
  
  location <-
    dbGetQuery(
      taxaDb,
      query,
      params = list(x = searchTerm[1, 8]))
  if (nrow(location) != 0) {
    return("kingdom")
  } else {
    return("0")
  }
}


# Set up a dataframe to use in the params argument in a call to dbGetQuery.
getSearchTerm <- function(organismName, organismUid) {
  sleep()
  if (organismUid == "") {
    match <- tax_name(
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

# Take a crux output matrix and  turn the characters "genus, spp, etc" 
# into  0s/1s. This function is used by orgamismPresence()
convert_CRUX <-
  function(crux_output) 
  {
    crux_without_taxonomic_names <- crux_output
    
    non_number_values <-
      c('genus', 'family', 'class', 'order', 'phylum', 'kingdom', 'error')
    
    ncols <- ncol(crux_output)
    nrows <- nrow(crux_output)
    
    for (i in 1:ncols)
    {
      for (j in 1:nrows)
      {
        boolean <- 
          crux_without_taxonomic_names[j, i] %in% non_number_values
        # If true, ie it matches genus, family, class, order
        if (isTRUE(boolean))
        {
          crux_without_taxonomic_names[j, i] <- as.numeric(0)
        } else {
          crux_without_taxonomic_names[j, i] <- 
            as.numeric(crux_output[j, i])
        }
      }
    }
    
    firstcolumn <- crux_without_taxonomic_names[, 1]
    
    crux_without_taxonomic_names <-
      as.matrix(crux_without_taxonomic_names)
    if (nrows > 1) {
      crux_without_taxonomic_names <-
        as.data.frame(apply(crux_without_taxonomic_names, 2, as.numeric)) 
    } else {
      crux_without_taxonomic_names <-
        as.data.frame(t(as.numeric(crux_without_taxonomic_names)))
      
      # Gets the column names for the matrix
      columns <-
        list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      # Adds the column names to the matrix
      colnames(crux_without_taxonomic_names) <- columns
    }
    crux_without_taxonomic_names
  }


# NCBI Tab ---------------------------------------------------------------------


# Takes a list of barcode markers and 
# returns a list of numeric ranges
# by marker to solicit the 
# user for input on the min/max sequence
# length for that marker.
getRangeList_MarkerSequenceLength <- function(barcodeList, ...){
  textList <- list()
  for (marker in barcodeList) {
    rangeInput <- numericRangeInput(
                      inputId = marker,
                      label = paste("Min/max sequence length for", marker),
                      value = c(0, 2000))
    # Add a numeric input
    textList <- list(
      textList, rangeInput)
  }
  # Return the list of numeric inputs
  textList
}

# Setup a list of sequence lengths based on the selections in the ui
getSeqLenList <- function(barcodeList, input) {
  seq_len_list <- list()
  for (code in barcodeList) {
    seq_len_list[[code]] <- input[[code]]
  }
  seq_len_list
}



# Creates a list of barcodes if 'code' is of the form b1+ b2+ b3+...,
# else it's just a single element list
splitBarcode <- function(barcode) {
  code <- trimws(barcode)
  code <- gsub("\\s+", "", code)  
  strsplit(code, "\\+")
}



# Pings the database db with searchTerm and downloadNumber.
# Returns a two element list containing the uids in the first
# Position and the count in the second position.
getNcbiSearchFullResults <- function(db, searchTerm, downloadNumber) {
  sleep()
  searchResult <- entrez_search(db = "nucleotide",
                                term = searchTerm,
                                retmax = downloadNumber)
  print("NCBISearchFullResults:")
  print(searchResult$ids)
  print(searchResult$count)
  list(searchResult$ids, searchResult$count)
}


# Sets up the search term that will be sent in a query to the database
getNcbiSearchTerm <- function(organism, code, searchOptionGene, searchOptionOrgn, seqLengthOption, seqLen) {
  code <- splitBarcode(code)
  searchTerm <- ""
  replacement <- ""
  
  if (searchOptionGene) {
    replacement <- "[GENE]"
  }
  
  replacement <- paste(replacement, " AND ", organism, sep = "")
  
  if (searchOptionOrgn) {
    replacement <-
      paste(replacement, "[ORGN]", sep = "")
  }
  
  if (seqLengthOption) {
    replacement <-
      paste(
        replacement,
        " AND ",
        seqLen[1],
        ":",
        seqLen[2],
        "[SLEN]",
        sep = ""
      )
  }
  for (c in code[[1]]) {
    searchTerm <- paste(searchTerm, "(", c, replacement, ")", sep="")
    if (c != code[[1]][[length(code[[1]])]]) {
      searchTerm <- paste(searchTerm, "OR ")
    }
  }
  searchTerm
}

# Converts the results object to a R matrix
# where the rows are the organism names and 
# columns are the barcodes. Entries are the number
# of results found for that organism for that barcode.
getNcbiResultsMatrix <- function(resultsDf, codeListLength) {
  count <- c()
  for (i in resultsDf[[1]]) {
    count <- c(count, i)
  }
  
  organismList <- resultsDf[[4]]
  organismListLength <- length(organismList)
  # Convert results vector to dataframe
  data <-
    matrix(count,
           nrow = organismListLength,
           ncol = codeListLength,
           byrow = TRUE)
  data
}

# Converts the results object to a R matrix
# where the rows are the organism names and 
# columns are the barcodes. Entries are the 
# searchTerm used to ping the database.
getNcbiSearchTermsMatrix <- function(searchResults, codeListLength) {
  organismList <- searchResults[[4]]
  organismListLength <- length(organismList)
  SearchStatements <- c()
  for (i in searchResults[[3]]) {
    SearchStatements <- c(SearchStatements, i)
  }
  data <-
    matrix(
      SearchStatements,
      nrow = organismListLength,
      ncol = codeListLength,
      byrow = TRUE
    )
  data
}