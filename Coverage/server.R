#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Imports ----------------------------------------------------------------------

library(shiny)
library(rentrez)
library(taxize)
library(tidyverse)
library(dplyr)
library(RSQLite)
library(rlist)
library(future)
library(promises)
library(ipc)
library(mpoly)
library(modules)
server_functions <- modules::use("server_functions.R")
orgListHelper <- modules::use("orgListHelper.R")

plan(multisession)
shinyServer(function(input, output, session) {
 
  # max number of homonyms to return if any
  # are found in the homonym check.
  maxHomonyms <- 5L
  
  # Removed reactive functions, so use a
  # vector to hold the uids from the most recent
  # search.
  statefulUids <- NULL
  
  # df to hold the matrix returned from the most 
  # recent search -- reactive functions removed.
  # May make more sense as a reactive function, depending
  # on 
  resultsMatrix <- NULL
  
  # Verifies the provided api key is
  # valid by performing a search with it.
  # Sets key validity variable in server_functions.R
  observeEvent(input$SetKey, {
    keyValidity <- tryCatch({
      entrez_search(
        db = "nucleotide",
        term = "Gallus Gallus",
        api_key = input$NCBIKey)
      shinyalert("Your API key has been accepted", type = "success")
      set_entrez_key(input$NCBIKey)
    }, error = function(err) {
      shinyalert("Your API key has been rejected, 
                 please make sure it is correct",
                 type = "warning")
    })
    server_functions$setNcbiKeyIsValid(keyValidity)
  })
  
  # Full Genome ----------------------------------------------------------------
  
  # * FullGenomeSearchButton ---------------------------------------------------
  
  # Reactive on search button call.
  # Gets the full results returned from the search
  # to feed to the output datatable.
  fullGenomeSearch <- eventReactive(input$genomeSearchButton, {
    dbOption <- input$gsearch
    orgList <- input$genomeOrganismList
    taxizeOption <- input$fullGenomeTaxizeOption
    refSeqChecked <- input$refSeq
    future_promise({
      server_functions$getGenomeSearchFullResults(
        dbOption = dbOption, 
        orgList = orgList, 
        taxizeOption = taxizeOption,
        refSeqChecked = refSeqChecked)
    })
  })
  
  # * FullGenomeInputCSV -------------------------------------------------------
  
  # Parses the uploaded csv into the textbox
  inputFileFullGenome <- observeEvent(
    input$uploadGenomeButton, 
    updateTextAreaInput(
      getDefaultReactiveDomain(), 
      inputId = "genomeOrganismList", 
      label = "Organism Names",
      value = server_functions$parseCsvColumnForTxtBox(
        input = input, 
        file.index = "uploadGenomeFile",
        column.header = "OrganismNames",
        textbox.id = "genomeOrganismList")))

  # parses fullGenomeSearch return value
  # and passes the dataframe and genome list
  # into the output data table.
  output$genomeResults <- DT::renderDataTable({
    then(fullGenomeSearch(), function(searchResults){
      resultsMatrix <<- searchResults[[1]]
      statefulUids <<- c(resultsMatrix[[2]])
      genomeList <- searchResults[[2]]
      DT::datatable(
        resultsMatrix[[1]],
        rownames = genomeList,
        colnames = names(resultsMatrix[[1]]))})
  })
  
  # * Download Fastas ----------------------------------------------------------
  
  output$fullGenomeDownloadF <- downloadHandler(
    filename = function() {
      paste("Full_Genome_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      progressLength <- length(statefulUids)
      progress <-
        AsyncProgress$new(
          session,
          min = 0,
          max = progressLength,
          message = "Downloading",
          value = 0
        )
      server_functions$fullGenomeDownload(
        filetype = "fasta", 
        uids = statefulUids,
        filepath = file,
        progress = progress)
    }
  )
  
  # * Download Genbank files ---------------------------------------------------
  
  output$fullGenomeDownloadG <- downloadHandler(
    filename = function() {
      paste("Full_Genome_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      progressLength <- length(statefulUids)
      progress <-
        AsyncProgress$new(
          session,
          min = 0,
          max = progressLength,
          message = "Downloading",
          value = 0
        )
      server_functions$fullGenomeDownload(
        filetype = "gb",
        uids = statefulUids,
        filepath = file,
        progress = progress)
    }
  )

  # * Download Full Genome Results Table ---------------------------------------
  
  output$fullGenomeDownloadT <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("Full_Genome_Table", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(resultsMatrix[[1]], file) 
      }
  )

  # CRUX ----------------------------------------------------------------------
  
  # * CRUXSearchButton --------------------------------------------------------
  
  cruxOrgSearch <-
    eventReactive(input$searchButton, {
      organismList <- input$CRUXorganismList # Returns as a string
      cruxTaxizeOption <- input$CRUXtaxizeOption
      # future_promise(
      #   server_functions$getGenomeList(organismList, cruxTaxizeOption))
      future_promise(
        server_functions$getCruxSearchFullResults(
          organismList, cruxTaxizeOption))
    })
  
  # * CRUXInputCSV -------------------------------------------------------------
  
  inputFileCrux <- observeEvent(
    input$uploadCRUXButton, {
      newOrganismNamesList <- server_functions$parseCsvColumnForTxtBox(
        input = input,
        file.index = "uCRUXfile",
        column.header = "OrganismNames",
        textbox.id = "CRUXorganismList"
      )
      updateTextAreaInput(
        session = getDefaultReactiveDomain(),
        inputId = "CRUXorganismList",
        value = newOrganismNamesList)
    })
  
  # * CRUXDownload -------------------------------------------------------------
  
  # Download CRUX table
  output$downloadCrux <- downloadHandler(
    filename = "CRUX_TABLE.csv",
    contentType = "text/csv",
    content = function(file) {
      then(cruxOrgSearch(), function(cruxCoverage) {        
        rows <- cruxCoverage[[1]]
        outputMatrix <- cruxCoverage[[2]]
        colnames(outputMatrix) <- list(
          "18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
        rownames(outputMatrix) <- rows
        write.csv(outputMatrix, file)
      })
    }
  )
  
  # * CRUXSummaryReportDownload ------------------------------------------------
  
  # Download CRUX Summary Report
  output$CRUXfileDownloadSD <- downloadHandler(
    filename = function() {
      # Create the file
      paste("CRUX_Summary_Report", ".csv", sep = "")
    },
    content = function(file) {
      promise_all(data_df = summary_report(0)) %...>% with({
        # Writes the dataframe to the CSV file
        write.csv(data_df, file) 
      })
    }
  )
  
  # * CRUXOutput ---------------------------------------------------------------
  
  output$CRUXcoverageResults <- DT::renderDataTable({
    then(cruxOrgSearch(), function(coverage){
      resultsMatrix <- coverage[[2]]
      genomeList <- coverage[[1]]
      columnNames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      DT::datatable(
        resultsMatrix,
        rownames = genomeList,
        colnames = columnNames)
      })
    })

  
  # NCBI -----------------------------------------------------------------------
  
  
  # * NCBISearchButton ---------------------------------------------------------
  

  # removing reactive elements, need barcodes to be accessible
  barcodeList_ <- NULL
  
  ncbiResultsDataframe <- NULL
  
  ncbiSearch <- eventReactive(input$NCBIsearchButton, {
    barcodeList_ <<- input$barcodeList
    barcodeList_ <<- strsplit(barcodeList_[[1]], ",")
    barcodeList_ <<- barcodeList_[[1]]
    organismList <- input$NCBIorganismList
    searchOptionGene <- input$NCBISearchOptionGene
    searchOptionOrgn <- input$NCBISearchOptionOrgn
    downloadNumber <- input$downloadNum
    seqLengthOption <- input$seqLengthOption
    ncbiTaxizeOption <- input$NCBItaxizeOption
    seq_len_list <- server_functions$getSeqLenList(barcodeList_, input)
    future_promise({
      uids <- list()
      searchTerms <- list()
      countResults <- list()
      organismList <- orgListHelper$taxizeHelper(organismList, ncbiTaxizeOption)
      for (organism in organismList) {
        for (code in barcodeList_) {
          searchTerm <- server_functions$getNcbiSearchTerm(organism, code, searchOptionGene, searchOptionOrgn, seqLengthOption, seq_len_list[[code]])
          searchResult <- server_functions$getNcbiSearchFullResults("nucleotide", searchTerm, downloadNumber)
          uids <- list.append(uids, searchResult[[1]])
          countResults <- list.append(countResults, searchResult[[2]])
          searchTerms <- list.append(searchTerms, searchTerm) 
        }
      }
      browser()
      ncbiResultsDataframe <<- list(count = countResults, ids = uids, searchTermslist = searchTerms, organismList = organismList)
    })
  })
  
  
  # * NCBI_Key -----------------------------------------------------------------
  NCBIKeyFlag <- FALSE
  observeEvent(input$SetKey, {
    #When NCBIKey is inputed
    key <- 0
    NCBI_names <- tryCatch({
      searchResult <-
        entrez_search(db = "nucleotide",
                      term = "Gallus Gallus",
                      api_key = input$NCBIKey)
    }, error = function(err) {
      shinyalert("Your API key has been rejected, please make sure it is correct",
                 type = "warning")
      key <<- 1
    })
    if (key == 0) {
      set_entrez_key(input$NCBIKey)
      shinyalert("Your API key has been accepted", type = "success")
      NCBIKeyFlag <- TRUE
    }
  })
  
  
  # * NCBIStrToList ------------------------------------------------------------
  
  
  NCBIorganismList <-
    reactive({
      #Converts string from NCBIorganismList into a list of Strings
      orgString <- input$NCBIorganismList
      NCBItaxizeOption <- input$NCBItaxizeOption
      future_promise({
        orgListHelper$taxizeHelper(orgString, NCBItaxizeOption)
      })
    })
  
  
  # * NCBIBarcodeList ----------------------------------------------------------
  
  barcodeList <- reactive({
    # separate based on comma
    barcodeList <- strsplit(input$barcodeList, ",") 
    barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
    barcodeList[[1]] <- unique(barcodeList[[1]][barcodeList[[1]] != ""])
    barcodeList[[1]]
  })
  
  
  # * NCBISequenceLength -------------------------------------------------------
  
  # returns a numericRangeInput object to display the min/max
  # ui thing for the user to input sequence lengths
  seqLenList <- eventReactive(input$seqLengthOption, {
    if (input$seqLengthOption) {
      barcodeList <- strsplit(input$barcodeList, ",")
      barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
      barcodeList[[1]] <-
        unique(barcodeList[[1]][barcodeList[[1]] != ""])
      server_functions$getRangeList_MarkerSequenceLength(barcodeList[[1]])
    }
  })

  
  # * NCBIMatrix ---------------------------------------------------------------

  
  matrixGet <-
    function() {
      # creates and returns the matrix to be displayed with the count
      then(ncbiSearch(), function(value) {
        codeListLength <- length(barcodeList_)
        
        count <- c()
        for (i in value[[1]]) {
          count <- c(count, i)
        }
        
        organismList <- value[[4]]
        organismListLength <- length(organismList)
        #convert results vector to dataframe
        data <-
          matrix(count,
                 nrow = organismListLength,
                 ncol = codeListLength,
                 byrow = TRUE)
        data
      })
    }
  
  # * NCBITableOutput ----------------------------------------------------------
  
  getNcbiSearchTermsMatrix <- function() {
    organismList <- ncbiResultsDataframe[[4]]
    organismListLength <- length(organismList)
    codeListLength <- length(barcodeList_)
    # Get the results from the NCBI query
    SearchStatements <- c()
    for (i in ncbiResultsDataframe[[3]]) {
      #3 is the 3rd list in genBankCovearage aka the searchterms list
      SearchStatements <- c(SearchStatements, i)
    }
    #convert results vector to dataframe
    data <-
      matrix(
        SearchStatements,
        nrow = organismListLength,
        ncol = codeListLength,
        byrow = TRUE
      )
    data
  }
  
  matrixGetSearchTerms <-
    function(){
      then(ncbiSearch(), function(value) {
        organismList <- value[[4]]
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList_)
        # Get the results from the NCBI query
        SearchStatements <- c()
        for (i in value[[3]]) {
          #3 is the 3rd list in genBankCovearage aka the searchterms list
          SearchStatements <- c(SearchStatements, i)
        }
        #convert results vector to dataframe
        data <-
          matrix(
            SearchStatements,
            nrow = organismListLength,
            ncol = codeListLength,
            byrow = TRUE
          )
        data
      })
    }
  
  
  # *   NCBIGetIDs -------------------------------------------------------------
  
  uidsGet <-
    function(){
      # Returns the uids stored in the results from the NCBi query
      then(ncbiSearch(), function(value) {
        # Get the results from the NCBI query
        uids <- c()
        for (i in value[[2]]) {
          uids <- c(uids, i)
        }
        uids
      })
    }
  
  
  # * NCBIDownloadFASTA --------------------------------------------------------
  
  # Download Fasta Files
  output$fileDownloadF <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      uidsGet() %...>% {
        progLength <- length(.)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_Fasta <- c()
          for (uid in .) {
            err <- 1
            while (err == 1) {
              File <-
                tryCatch({
                  # Try catch for determining if homonyms exist, if they do fill
                  # up the errorPopupList and activate the errorHomonym Flag
                  if (!NCBIKeyFlag) {
                    # sleeping for 1/3 of a second each time gives us 3 queries 
                    # a second. If each user queries at this rate, we can 
                    # service 4-8 at the same time.
                    Sys.sleep(0.34) 
                  }
                  # Get the fasta file with that uid
                  File_fasta <-
                    entrez_fetch(db = "nucleotide",
                                 id = uid,
                                 rettype = "fasta") 
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            Vector_Fasta <-
              c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
            progress$inc(amount = 1)
          }
          # Writes the vector containing all the fasta file information into 
          # one fasta file
          write(Vector_Fasta, file) 
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )
  
  
  # * NCBIDownloadGenbank ------------------------------------------------------
  
  # Download NCBI Genbank Files
  output$fileDownloadG <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      uidsGet() %...>% {
        progLength <- length(.)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_genbank <- c()
          for (uid in .) {
            err <- 1
            while (err == 1) {
              File <-
                tryCatch({
                  # Try catch for determining if homonyms exist, if they do fill
                  # up the errorPopupList and activate the errorHomonym Flag
                  if (!NCBIKeyFlag) {
                    # sleeping for 1/3 of a second each time gives us 3 queries 
                    # a second. If each user queries at this rate, we can 
                    # service 4-8 at the same time.
                    Sys.sleep(0.34) 
                  }
                  
                  # Get the genbank file with that uid
                  File_genbank <-
                    entrez_fetch(db = "nucleotide",
                                 id = uid,
                                 rettype = "gb")  
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            
            # Append the genbank file to a vector
            Vector_genbank <-
              c(Vector_genbank, File_genbank) 
            progress$inc(amount = 1)
          }
          # Writes the vector containing all the genbank file information into
          # one genbank file
          write(Vector_genbank, file, append = TRUE) 
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )
  
  # * NCBISummaryReportDownload ------------------------------------------------
  output$NCBIfileDownloadSD <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Summary_Report", ".csv", sep = "")
    },
    content = function(file) {
      promise_all(data_df = summary_report(1)) %...>% with({
        # Writes the dataframe to the CSV file
        write.csv(data_df, file)
      })
    }
  )
  
  # * NCBIBarcodeButtons -------------------------------------------------------
  
  observeEvent(input$barcodeOptionCO1, {
    # Detects when the specific barcode (in this case CO1) button has been 
    # pressed
    
    # If the input barcodeList is not empty (ie. the inputtextarea is not 
    # empty) then use the paste function to the add the barcode/s to the 
    # beginning
    if (input$barcodeList[[1]] != "") {
      # Updates the text area input adds the barcode/s to the beginning of 
      # whatever is already in it
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("(CO1; COI; COX1),", input$barcodeList)
      ) 
    }
    else {
      # Here since the textarea is empty we just set its value to the barcode/s
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "(CO1; COI; COX1)") 
    }
  })
  
  
  observeEvent(input$barcodeOption16S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("16S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "16S")
    }
  })
  
  
  observeEvent(input$barcodeOptionITS2, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("ITS2,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "ITS2")
    }
  })
  
  
  observeEvent(input$barcodeOption18S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("18S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "18S")
    }
  })
  
  
  observeEvent(input$barcodeOptionITS1, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("ITS1,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "ITS1")
    }
  })
  
  
  observeEvent(input$barcodeOptiontrnl, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("trnl,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "trnl")
    }
  })
  
  
  observeEvent(input$barcodeOption12S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("12S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "12S")
    }
  })
  
  
  # * NCBIOutputTables ---------------------------------------------------------
  
  
  #outputs:
  output$seqLenInputs <- renderUI(seqLenList())
  
  output$NCBIcoverageResults <- DT::renderDataTable({
    then(ncbiSearch(), function(searchResults) {
      data_df <- getNcbiResultsMatrix(searchResults)
      rows <- searchResults[[4]]
      barcodes <- barcodeList_
      DT::datatable(
        data_df,
        rownames = rows,
        colnames = barcodes
      )
    })
    
  
  # * NCBInputFile -------------------------------------------------------------
  
  inputFileNCBI <- observeEvent(input$uploadNCBIButton, {
    isolate({
      req(input$uNCBIfile,
          file.exists(input$uNCBIfile$datapath))
      uploadinfo <-
        read.csv(input$uNCBIfile$datapath, header = TRUE)
      if (input$NCBIorganismList[[1]] != "") {
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "NCBIorganismList",
          value = c(
            head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]),
            input$NCBIorganismList
          )
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "NCBIorganismList",
                            value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
      }
      if (input$barcodeList[[1]] != "") {
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "barcodeList",
          value = c(head(uploadinfo$Barcodes[uploadinfo$Barcodes != ""]), 
                    input$barcodeList)
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "barcodeList",
                            value = uploadinfo$Barcodes[uploadinfo$Barcodes != ""])
      }
    })
  })
  
  # * NCBIDownloadTable --------------------------------------------------------
  
  # Download NCBI table
  output$download <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Table", ".csv", sep = "")
    },
    content = function(file) {
      columns <- barcodeList_
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiResultsMatrix(searchResults)
        colnames(df) <- columns
        rownames(df) <- rows
        write.csv(df, file)
      })
      # # NCBIorganismList can be replaced with ncbiResultsDataframe[[4]]
      # NCBIorganismList() %...>% {
      #   #Gets the row names for the matrix
      #   rows <- . 
      #   # matrixGet() can be replaced with getNcbiResultsMatrix()
      #   
      #   matrixGet() %...>% {
      #     # Gets the matrix for the NCBI results
      #     future_promise({
      #       # Adds the column names to the matrix
      #       colnames(.) <- columns 
      #       # Adds the row names to the matrix
      #       rownames(.) <- rows 
      #       # Writes the dataframe to the CSV file
      #       write.csv(., file) 
      #     })
      #   }
      # }
    }
  )
  
  # * NCBIDownloadSearchTerms --------------------------------------------------
  
  #Download Search Terms:
  output$downloadStatements <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Search_Statements", ".csv", sep = "")
    },
    content = function(file) {
      # Gets the column names for the matrix
      columns <- barcodeList_
      # NCBIorganismList can be replaced with ncbiResultsDataframe[[4]]
      NCBIorganismList() %...>% {
        #Gets the row names for the matrix
        rows <- . 
        matrixGetSearchTerms() %...>% {
          # Gets the matrix for the NCBI results
          future_promise({
            # Adds the column names to the matrix
            colnames(.) <- columns 
            # Adds the row names to the matrix
            rownames(.) <- rows 
            # Writes the dataframe to the CSV file
            write.csv(., file) 
          })
        }
      }
    }
  )
  
  # * SummaryReport ------------------------------------------------------------
  
  
  summary_report <- function(databaseFlag) {
    if (databaseFlag == 1) {
      columns <- barcodeList_
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiResultsMatrix(searchResults)
        colnames(df) <- columns
        rownames(df) <- rows
        summary_report_dataframe(df)
      })
      
      # # matrixGet() can be replaced with getNcbiResultsMatrix()
      # matrixGet() %...>% {
      #   NCBIdata <- .
      #   # NCBIorganismList can be replaced with ncbiResultsDataframe[[4]]
      #   NCBIorganismList() %...>% {
      #     # Gets the column names for the matrix
      #     columns <- barcodeList_
      #     # Adds the column names to the matrix
      #     colnames(NCBIdata) <- columns 
      #     # Adds the row names to the matrix
      #     rownames(NCBIdata) <- . 
      #     
      #     # Convert to Dataframe
      #     NCBIdata <- as.data.frame(NCBIdata) 
      #     dataframe <- NCBIdata
      #     summary_report_dataframe(dataframe)
      #   }
      # }
    } else {
      then(cruxOrgSearch(), function(coverage) {
        organismList <- coverage[[1]]
        cruxMatrix <- coverage[[2]]
        columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
        colnames(cruxMatrix) <- columns
        rownames(cruxMatrix) <- organismList
        dataframe <- convert_CRUX(cruxMatrix)
        summary_report_dataframe(dataframe)
      })
    }
  }
 })
