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


orgListHelper <- tryCatch({modules::use("orgListHelper.R")},
                          error = function(err){modules::use("Coverage/orgListHelper.R")})

# General ----------------------------------------------------------------------

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

# NCBI still rate limits with a key, to 10/s
sleep <- function() {
  if (ncbiKeyIsValid) {
    Sys.sleep(0.1)
  } else {
    Sys.sleep(0.34)
  }
}

# Sets local variable ncbiKeyIsValid.
# Does not check if the provided key
# is valid, this should be done 
# before calling this function.
ncbiKeyIsValid <- FALSE
setNcbiKeyIsValid <- function(validity = TRUE) {
  ncbiKeyIsValid <- validity
}

# Used for both crux and NCBI tabs
summary_report_dataframe <- function(dataframe)
{
  class(dataframe)
  class(dataframe[, 1])
  options(scipen = 999) #scientific notion
  new_row_names <- "Total"
  # doesn't include column with taxa snames
  new_row_names <- c(new_row_names, colnames(dataframe))
  
  statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
  new_col_names <-
    c(
      "Barcodes",
      "Number of Sequences Found",
      "Percent of Total Sequences Found",
      "Number of Organisms with at Least one Sequence",
      "Number of Organisms with no Sequences"
    )
  colnames(statistics_df) <- new_col_names
  #get list of columns + a column called "total"
  
  #add row names
  for (i in 1:length(new_row_names))
  {
    statistics_df[i, 1] <- new_row_names[i]
  }
  
  #doesn't include column with taxa snames
  barcodeSums <- colSums(dataframe) 
  
  Total_seq_found <- sum(barcodeSums)
  
  #hard code in the totals
  statistics_df[1, 2] <- Total_seq_found
  statistics_df[1, 3] <- 100
  
  for (i in 2:length(new_row_names))
  {
    x <- i - 1
    statistics_df[i, 2] <- barcodeSums[x]
    if(Total_seq_found == 0){
      statistics_df[i, 3] <- 0
    } else {
      statistics_df[i, 3] <- round(((barcodeSums[x] / Total_seq_found) * 100), digits=2)
    }
  }
  
  #hard code in the totals
  output_of_which_rows_are_empty_and_arenot <-
    which_rows_are_empty_and_arenot(dataframe,-1)
  #list 2 is thee species without any seqs
  statistics_df[1, 5] <-
    length(output_of_which_rows_are_empty_and_arenot[[2]])
  #we know list 1 is the species with some seqs
  statistics_df[1, 4] <-
    length(output_of_which_rows_are_empty_and_arenot[[1]])
  
  for (i in 2:length(new_row_names))
  {
    x <- i - 1
    output_of_which_rows_are_empty_and_arenot <-
      which_rows_are_empty_and_arenot(dataframe, Which_Column = x)
    #list 2 is the species without any seqs
    statistics_df[i, 5] <-
      length(output_of_which_rows_are_empty_and_arenot[[2]])
    #we know list 1 is the species with some seqs
    statistics_df[i, 4] <-
      length(output_of_which_rows_are_empty_and_arenot[[1]])
  }
  statistics_df
}


# if which_column = -1 it means do all rows, if a column number is given the 
# function will only run on said column of the dataframe returns list of 2 
# lists, one of species with seqs, and one of species without any sequences
which_rows_are_empty_and_arenot <-
  function(dataframe, Which_Column)
  {
    if (is.null(Which_Column))
    {
      Which_Column <- -1
    }
    Which_Column <- Which_Column
    #create two lists
    haveSomeSeq <- c()
    haveZeroSeq <- c()
    
    ncols <- ncol(dataframe)
    nrows <- nrow(dataframe)
    
    if (Which_Column < 0) {
      #we will skip the first column because it has names
      for (i in 1:nrows)
      {
        total <- 0
        for (j in 1:ncols)
        {
          newNum <- as.numeric(dataframe[i,j])
          if(is.na(newNum)){
            newNum <- 0
          }
          total <- total + newNum
        }
        
        if (!is.null(total) && total > 0)
        {
          #add species name to list
          haveSomeSeq <- c(haveSomeSeq, dataframe[i, 1])
        } else
        {
          #add species name to list
          haveZeroSeq <- c(haveZeroSeq, dataframe[i, 1])
        }
      }
    } else {         #if a specific columnn
      #we will skip the first column because it has names
      for (i in 1:nrows)
      {
        newNum <- as.numeric(dataframe[i, Which_Column])
        if(is.na(newNum)){
          newNum <- 0
        }
        seqs <- 0
        seqs <- 0 + newNum
        
        if (!is.null(seqs) && seqs > 0)
        {
          #add species name to list
          haveSomeSeq <- c(haveSomeSeq, dataframe[i, 1]) 
        } else
        {
          #add species name to list
          haveZeroSeq <- c(haveZeroSeq, dataframe[i, 1])
        }
      }
    }
    if (Which_Column < 0) {
      results <-
        list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs = haveZeroSeq)
      results <- as.matrix(results)
    } else
    {
      COLNam <- colnames(dataframe)
      column_name <- paste0("Have", COLNam[Which_Column], "Seq")
      results <-
        list(
          single_Barcode_haveSomeseq = haveSomeSeq,
          single_Barcode_haveZeroSeqs = haveZeroSeq
        )
      results <- as.matrix(results)
    }
    results
  }



# Full Genome Tab --------------------------------------------------------------

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
  function(dbToSearch, genomeList, parameters, columnNames, progress) {
    Results <- data.frame(matrix(0, ncol = 2, nrow = length(genomeList)))
    names(Results) <- columnNames
    uids <- c()
    genomeListLength <- length(genomeList) 
    if (0 < genomeListLength) {
      genomesDownloaded <- 0
      progress$set(detail = paste0("0","/",genomeListLength))
      for (i in 1:length(genomeList)){
        sleep()
        progress$set(message = paste0("Retrieving barcodes for ", genomeList[i]))
        progress$inc(amount = 0.5/genomeListLength)
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
          searchResult <- 0
        }, error = function(err) {
            error <- 1
        })
        if (searchResult == 1) {
          Results[i, 1] <- "Error"
          Results[i, 2] <- "Error"
        }
        genomesDownloaded <- genomesDownloaded + 1
        progress$set(message = paste0("Retrieved barcodes for ", genomeList[i]),
                     detail = paste0(genomesDownloaded,"/",genomeListLength))
        progress$inc(amount = 0.5/genomeListLength)
      }
    }
    progress$close()
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
getGenomeSearchFullResults <- function(dbOption, orgList, taxizeOption, refSeqChecked, progress) {
  databaseOption <- getDbToSearch(dbOption)
  taxize_results <- orgListHelper$taxizeHelper(orgList, taxizeOption)
  organismList <- taxize_results$results
  dfColumnNames <- getColumnNames(dbOption)
  parameters <- getNcbiSearchParameters(dbOption, refSeqChecked)
  list(getNcbiSearchResults(
    databaseOption,
    organismList,
    parameters,
    dfColumnNames,
    progress),
    organismList)
}


# CRUX Tab ---------------------------------------------------------------------

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
getCruxSearchFullResults <- function(organismList, progress) {
  nameUidList <- getHomonyms(tolower(organismList))
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
    
    #add original query in case of get_uid_ being wrong
    newOrganismNamesList <- c(newOrganismNamesList, organism)
    newOrganismUidsList <- c(newOrganismUidsList, "")
    
    if (!(is.null(search[[1]]))) {
      for (i in 1:nrow(search[[1]])){
        newOrganismName <- tolower(search[[1]][["scientificname"]][[i]])
        
        #avoid duplicates
        if (newOrganismName != organism) {
          newOrganismNamesList <- c(
            newOrganismNamesList, newOrganismName)
          newOrganismUidsList <- c(
            newOrganismUidsList, search[[1]][["uid"]][[i]])
        }
        
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


# Set up a dataframe to use in the params argument in a call
# to dbGetQuery.
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


convert_CRUX <-
  function(crux_output) 
    # Take a crux output matrix and  turn the characters "genus, spp, etc" 
    # into  0s/1s. This function is used by which_rows_are_empty_and_arenot()
  {
    crux_without_taxonomic_names <- crux_output
    #crux_without_taxonomic_names <-
    #  na.omit(crux_without_taxonomic_names)
    
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
        #if true, ie it matches genus, family, class, order
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
    #add a numeric input
    textList <- list(
      textList, rangeInput)
  }
  #return the list of numeric inputs
  textList
}

# setup a list of sequence lengths based on the selections in the ui
getSeqLenList <- function(barcodeList, input) {
  seq_len_list <- list()
  for (code in barcodeList) {
    seq_len_list[[code]] <- input[[code]]
  }
  seq_len_list
}



# creates a list of barcodes if
# 'code' is of the form (b1; b2; b3;...),
# else it's just a single element list
splitBarcode <- function(barcode) {
  code <- trimws(barcode)
  code <- gsub("[(, ,)]", "", code)
  strsplit(code, ";")
}



# pings the database db with searchTerm and downloadNumber.
# returns a two element list containing the uids in the first
# position and the count in the second position.
getNcbiSearchFullResults <- function(db, searchTerm, downloadNumber) {
  sleep()
  searchResult <- entrez_search(db = "nucleotide",
                                term = searchTerm,
                                retmax = downloadNumber)
  list(searchResult$ids, searchResult$count)
}


# sets up the search term that will be sent in a query to the database
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

# converts the results object to a R matrix
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
  #convert results vector to dataframe
  data <-
    matrix(count,
           nrow = organismListLength,
           ncol = codeListLength,
           byrow = TRUE)
  data
}

# converts the results object to a R matrix
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