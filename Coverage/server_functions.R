# parse a csv file and append the list from column column.header
# to what's already in the text box.
parseCsvColumnForTxtBox <- function(input, file.index, column.header, textbox.id) {
  req(input[[file.index]],
      file.exists(input[[file.index]]$datapath))
  # read.csv complains about there being an incomplete final line (no new line character)
  # if (readLines(input[[file.index]]$datapath, -1) != "\n"){
  #  write.table("\n", input[[file.index]]$datapath, append=TRUE, col.names = FALSE)
  #}
  uploadinfo <-
    read.csv(input[[file.index]]$datapath, header = TRUE)
  # preserve any organism names already in the text box
  if (input[[textbox.id]][[1]] == "") {
    return(uploadinfo[[column.header]][uploadinfo[[column.header]] != ""])
  }
  c(input[[textbox.id]], head(uploadinfo[[column.header]][uploadinfo[[column.header]] != ""]))
}

getNcbiSearchParameters <- function(genomeType, refSeqChecked) {
  if (genomeType == 'mitochondrial') {
    if (refSeqChecked) {
      return (" AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN] AND srcdb_refseq[PROP]")
    } else {
      return (" AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN]")
    }
  } else if (genomeType == 'chloroplast') {
    if (refSeqChecked) {
      return (" AND Chloroplast[TITL] AND 120000:170000[SLEN] AND srcdb_refseq[PROP]")
    } else {
      return (" AND Chloroplast[TITL] AND 120000:170000[SLEN]")
    }
  } else {
    stop("Attempted to get search parameters for unknown genome type")
  }
}

getNcbiSearchResults <- function(resultsDfColumnNames, databaseName, genomeList, parameters=''){
  rowCount = length(genomeList)
  Results <- data.frame(matrix(0, ncol = 2, nrow = rowCount))
  uids <- c() 
  names(Results) <- resultsDfColumnNames
  for (i in 1:rowCount){
    if (!NCBIKeyFlag) {
      # Sleep to prevent Rate Limiting
      Sys.sleep(0.34)
    }
    genomeSearchTerm <- paste0('', genomeList[i], '[ORGN]', parameters, '')
    genome_result <- future_promise(
      entrez_search(
        db = databaseName,
        term = genomeSearchTerm,
        retmax = 5))
    then(genome_result, function(value) {
      Results[i, 1] <- value$count 
      Results[i, 2] <- genomeSearchTerm 
      # Save all the UIDs, needed for downloading
      for (id in genome_result$ids) {
        uids <- c(uids, id) 
      }
    })
  }
  list(Results, uids)
}