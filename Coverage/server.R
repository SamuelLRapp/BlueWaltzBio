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
library(shinyjs)
require("bold") 
require("treemap")
library(plotly)
library(treemapify)
library(ggplot2)

orgListHelper <- modules::use("orgListHelper.R")
server_functions <- modules::use("server_functions.R")
bold_functions <- modules::use("bold_functions.R")


plan(multisession)
shinyServer(function(input, output, session) {
  hideTab("BOLDpage", "Results")
  hideTab("BOLDpage", "Organism Names")
  hideTab("BOLDpage", "Plot Unique Species Per Country")
  hideTab("BOLDpage", "Plot Total Sequences Per Country")
  hideTab("BOLDpage", "Filter By Country")
  hideTab("BOLDpage", "Country Data")
  hideTab("CRUXpage", "Results")
  hideTab("CRUXpage", "Organism Names")
  hideTab("CRUXpage", "Summary Results")
  hideTab("NCBIpage", "Organism Names")
  hideTab("NCBIpage", "Barcodes of Interest")
  hideTab("NCBIpage", "Results")
  hideTab("NCBIpage", "Summary Results")
  hideTab("FullGenomePage", "Results")
  hideTab("FullGenomePage", "Organism Names")
  hideTab("FullGenomePage", "Summary Results")

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
  
  
# NCBI Key ---------------------------------------------------------------------
  
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
  })
 
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
      Sys.setenv(ENTREZ_KEY = input$NCBIKey)
      shinyalert("Your API key has been accepted", type = "success")
      NCBIKeyFlag <- TRUE
    }
    server_functions$setNcbiKeyIsValid(NCBIKeyFlag)
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
  
  # * UI pipeline updates ------------------------------------------------------
  observeEvent(input$genomeSearchButton, {
    # Go to the results page after clicking the search button
    updateTabsetPanel(session, "FullGenomePage", selected = "Results")
    showTab("FullGenomePage", "Results")
  })
  
  # * Full Genome Input CSV ----------------------------------------------------
  observeEvent(input$FullGenomeStart,
  {
    # Begin Full genome pipeline
    updateTabsetPanel(session, "FullGenomePage", selected = "Organism Names")
    showTab("FullGenomePage", "Organism Names")
    # It requires a file to be uploaded first
    req(input$uploadGenomeFile,
        file.exists(input$uploadGenomeFile$datapath)) 
    isolate({
      # It requires a file to be uploaded first
      # Read the CSV and write all the Organism Names into the Text Area Input
      uploadinfo <-
        read.csv(input$uploadGenomeFile$datapath, header = TRUE)
      updateTextAreaInput(
        getDefaultReactiveDomain(), 
        inputId = "genomeOrganismList", 
        label = "Organism Names",
        value = server_functions$parseCsvColumnForTxtBox(
          input = input, 
          file.index = "uploadGenomeFile",
          
          
          #beware, adding a space into the column header name causes problems
          column.header = "OrganismNames",
          textbox.id = "genomeOrganismList"))
    })
  })
  
  # * Output table -------------------------------------------------------------  
  
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
  
  output$FullGenomeSummaryResults <- DT::renderDataTable({
    promise_all(data_df = summary_report(0), 
                rows = organismListGet()) %...>% with({
                  DT::datatable(
                    data_df
                  )
                })
  })
  
  # * Download Fastas ----------------------------------------------------------
  mostRecentFile <- function(dirpath) {
    df <- file.info(list.files(dirpath, full.names = T))
    rownames(df)[which.max(df$mtime)]
  }
  
  # * Download tests start------------------------------------------------------
  observeEvent(input$dwntest, {
    #run JS portion of test (ui interactions)
    js$ncbiDwnFastaTest(testOrganisms = "canis lupus, cygnus")
    
    #gets reference file to see if download completed later
    reffile <- mostRecentFile(".")
    print("Waiting 40 secs for file to finish downloading...")
    #runs when search complete and download button clicked
    observeEvent(input[["ui-test-complete"]], {
      #get the most recent file in the downloads file
      dwnfile <- mostRecentFile(".")
      
      #if download has completed then filename should have changed
      if (dwnfile != reffile){
        print("Test passed")
      } else {
        print("Test failed: file did not download")
      }
    })
  })
  
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
  
  CRUXOrgList <- eventReactive(input$searchButton, {
    input$CRUXorganismList
  })
    
  organismListGet <- reactive({
    orgSearch <- CRUXOrgList()
    taxizeBool <- input$CRUXtaxizeOption
    future_promise({
      result <- orgListHelper$taxizeHelper(orgSearch, taxizeBool)
      result
    })
  })
  
  
  cruxOrgSearch <- reactive({
    organismListGet() %...>% {
      future_promise(
        server_functions$getCruxSearchFullResults(.))
    }
  })
    
  
  
  # * UI pipeline updates ------------------------------------------------------
  observeEvent(input$searchButton, {
    # Begin CRUX search
    updateTabsetPanel(session, "CRUXpage", selected = "Results")
    showTab("CRUXpage", "Results")
  })
  
  # * CRUXInputCSV -------------------------------------------------------------
  
  observeEvent(input$CruxStart,
               {
                 # Begin CRUX pipeline
                 updateTabsetPanel(session, "CRUXpage", selected = "Organism Names")
                 showTab("CRUXpage", "Organism Names")
                 isolate({
                   # It requires a file to be uploaded first
                   req(input$uCRUXfile, file.exists(input$uCRUXfile$datapath))
                   # Read the CSV and write all the Organism Names into the Text Area Input
                   uploadinfo <- read.csv(input$uCRUXfile$datapath, header = TRUE)
                   newOrganismNamesList <- server_functions$parseCsvColumnForTxtBox(
                     input = input,
                     file.index = "uCRUXfile",
                     
                     #beware, adding a space into the column header name causes problems
                     column.header = "OrganismNames",
                     textbox.id = "CRUXorganismList"
                   )
                   updateTextAreaInput(
                     session = getDefaultReactiveDomain(),
                     inputId = "CRUXorganismList",
                     value = newOrganismNamesList)
                 })
               })
  
  
  # * Summary Data Button ------------------------------------------------------
  observeEvent(input$SummaryDataButton,
  {
      # Go to summary results
      updateTabsetPanel(session, "CRUXpage", selected = "Summary Results")
      showTab("CRUXpage", "Summary Results")
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
  
  output$CRUXSummaryResults <- DT::renderDataTable({
    promise_all(data_df = summary_report(0), 
                rows = organismListGet()) %...>% with({
                  DT::datatable(
                    data_df
                  )
                })
  })
  
  # NCBI -----------------------------------------------------------------------

  # removing reactive elements, need barcodes to be accessible
  barcodeList_ <- NULL
  
  ncbiResultsDataframe <- NULL
  
  # * NCBISearchButton ---------------------------------------------------------
  ncbiSearch <- eventReactive(input$NCBIsearchButton, {
    barcodeList_ <<- input$barcodeList
    #TODO: Move these 2 lines into a single function with splitBarcode() in server_functions.R
    barcodeList_ <<- strsplit(barcodeList_[[1]], ",")
    barcodeList_ <<- barcodeList_[[1]]
    barcodeList_ <<- trimws(barcodeList_, "b")
    barcodeList  <<- unique(barcodeList_[barcodeList_ != ""])
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
      ncbiResultsDataframe <<- list(count = countResults, ids = uids, searchTermslist = searchTerms, organismList = organismList)
    })
  })
  
  
  observeEvent(input$NCBIsearchButton, {
    # Start NCBI search button
    updateTabsetPanel(session, "NCBIpage", selected = "Results")
    showTab("NCBIpage", "Results")
  })
  
  observeEvent(input$NCBIStartOver, {
    # Start NCBI search all over again
    updateTabsetPanel(session, "NCBIpage", selected = "Start Your NCBI Search")
    hideTab("NCBIpage", "Organism Names")
    hideTab("NCBIpage", "Barcodes of Interest")
    hideTab("NCBIpage", "Results")
    hideTab("NCBIpage", "Summary Results")
    updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(""))
    updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(""))
  })
  
  observeEvent(input$BarcodesNext, {
    # Go the barcodes tab to allow user to input them
     updateTabsetPanel(session, "NCBIpage", selected = "Barcodes of Interest")
     showTab("NCBIpage", "Barcodes of Interest")
   })
  
  observeEvent(input$StartNCBIButton, {
    # Begin the NCBI pipeline button
    updateTabsetPanel(session, "NCBIpage", selected = "Organism Names")
    showTab("NCBIpage", "Organism Names")
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
  
  observeEvent(input$NCBISummaryDataButton,
   {
     # Check the summary data
     updateTabsetPanel(session, "NCBIpage", selected = "Summary Results")
     showTab("NCBIpage", "Summary Results")
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
  seqLenList <- reactive({
    if (input$seqLengthOption) {
      barcodeList <- strsplit(input$barcodeList, ",")
      barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
      barcodeList[[1]] <-
        unique(barcodeList[[1]][barcodeList[[1]] != ""])
      server_functions$getRangeList_MarkerSequenceLength(barcodeList[[1]])
    }
  })
  
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
      data_df <- server_functions$getNcbiResultsMatrix(searchResults, length(barcodeList_))
      rows <- searchResults[[4]]
      barcodes <- barcodeList_
      DT::datatable(
        data_df,
        rownames = rows,
        colnames = barcodes
      )
    })
  })
  
  output$NCBISummaryResults <- DT::renderDataTable({
    promise_all(data_df = summary_report(1), 
                rows = NCBIorganismList()) %...>% with({
                  DT::datatable(data_df, rownames = FALSE)
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
        df <- server_functions$getNcbiResultsMatrix(searchResults, length(barcodeList_))
        colnames(df) <- columns
        rownames(df) <- rows
        write.csv(df, file)
      })
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
      columns <- barcodeList_
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiSearchTermsMatrix(searchResults, length(barcodeList_))
        colnames(df) <- columns
        rownames(df) <- rows
        write.csv(df, file)
      })
    }
  )
  
  # * SummaryReport ------------------------------------------------------------
  
  
  summary_report <- function(databaseFlag) {
    if (databaseFlag == 1) {
      columns <- barcodeList_
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiResultsMatrix(searchResults, length(barcodeList_))
        colnames(df) <- columns
        rownames(df) <- rows
        server_functions$summary_report_dataframe(df)
      })
    } else if (databaseFlag == 0) {
      then(cruxOrgSearch(), function(coverage) {
        organismList <- coverage[[1]]
        cruxMatrix <- coverage[[2]]
        columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
        colnames(cruxMatrix) <- columns
        rownames(cruxMatrix) <- organismList
        cleaned_cruxMatrix <- na.omit(cruxMatrix)
        dataframe <- server_functions$convert_CRUX(cleaned_cruxMatrix)
        server_functions$summary_report_dataframe(dataframe)
        })
    } else {
      BoldMatrix() %...>% {
        matrix <- .
        dataframe <- bold_functions$barcode_summary(matrix)
        print(dataframe)
        server_functions$summary_report_dataframe(dataframe)
      }
    }
  }

# BOLD --------------------------------------------------------------------


# * BOLDSearchButton ------------------------------------------------------

    BOLDOrgSearch <- eventReactive(input$BOLDsearchButton, { #When searchButton clicked, update BOLDOrgSearch to return the value input into BOLDorganismList
      input$BOLDorganismList #Returns as a string
    })
    
    observeEvent(input$BOLDsearchButton, {
      updateTabsetPanel(session, "BOLDpage", selected = "Filter By Country")
      showTab("BOLDpage", "Filter By Country")
      shinyjs::hide(id = "BOLDClearFilter")
      shinyjs::hide(id = "BOLDfilterCountries")
      shinyjs::hide(id = "BOLDSkipFilter")
      shinyjs::hide(id = "BOLDNullSpecies")
      shinyjs::hide(id = "BOLDNullSpeciesWarning")
    })

    
    observeEvent(input$BOLDSkipFilter, {
      updateTabsetPanel(session, "BOLDpage", selected = "Results")
      showTab("BOLDpage", "Results")
      showTab("BOLDpage", "Plot Unique Species Per Country")
      showTab("BOLDpage", "Plot Total Sequences Per Country")
      showTab("BOLDpage", "Country Data")
      updateSelectizeInput(inputId="selectCountry", label="Filter by Countries", choices=boldCoverage()$countries, selected = boldCoverage()$countries,options = NULL)
      click("BOLDfilterCountries")
    })
    
    observeEvent(input$BOLDfilterCountries, {
      updateTabsetPanel(session, "BOLDpage", selected = "Results")
      showTab("BOLDpage", "Plot Unique Species Per Country")
      showTab("BOLDpage", "Plot Total Sequences Per Country")
      showTab("BOLDpage", "Results")
      showTab("BOLDpage", "Country Data")
    })
    
    observeEvent(input$BOLDClearFilter, {
      updateSelectizeInput(inputId="selectCountry", selected = character(0))
    })
    
    observeEvent(input$BOLDStartButton,
   {
     updateTabsetPanel(session, "BOLDpage", selected = "Organism Names")
     showTab("BOLDpage", "Organism Names")
     # It requires a file to be uploaded first
     isolate({
       req(input$uBOLDfile, file.exists(input$uBOLDfile$datapath))
       uploadinfo <- read.csv(input$uBOLDfile$datapath, header = TRUE)
       if(input$BOLDorganismList[[1]] != "") {
         updateTextAreaInput(getDefaultReactiveDomain(), "BOLDorganismList", value = c(head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""], -1), input$BOLDorganismList))
       }
       else {
         updateTextAreaInput(getDefaultReactiveDomain(), "BOLDorganismList", value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
       }
     })
   })

    # * BOLDCoverage ------------------------------------------------------------

    
    boldCoverage <- reactive ({
        orgSearch <- BOLDOrgSearch()
        taxize_selected <- input$BOLDtaxizeOption
        future_promise({orgListHelper$taxizeHelper(orgSearch, taxize_selected)}) %...>% {
          organismList <- .
          organismListLength <- length(organismList)
          validate(
            need(organismListLength > 0, 'Please name at least one organism')
          )
          
          #puts variable in global scope
          unfound_species <<- c()
          
          results <- data.frame(matrix(ncol=0, nrow=0))
          bold_failed <- 0
          future_promise({
            countries <- c()
            for(organism in organismList){
              searchResult <- tryCatch({
                records_bold <- bold_seqspec(taxon = organism)
              }, error = function(err) {
                print("ERROR IN BOLD SEARCH")
                bold_failed <- 1
              })
              if (!is.na(records_bold)){
                for (i in 1:nrow(records_bold)) {
                  if (is.na(records_bold$country[i]) || records_bold$country[i] == "") {
                    records_bold$country[i] <- "No Country Listed"
                  }
                }
                if (!is.na(records_bold$species_name)) {
                  countries <- c(countries, records_bold$country)
                  results <- rbind(results, records_bold)
                } else {
                  unfound_species <<- c(unfound_species, organism)
                }
              }
            }
            results <- list(results=results, countries=countries)
          }) %...>% {
            if(bold_failed == 1){
              print("BOLD is down")
              # POP UP TELLING USER THAT BOLD IS DOWN
            }
            shinyjs::show(id = "BOLDClearFilter")
            shinyjs::show(id = "BOLDfilterCountries")
            shinyjs::show(id = "BOLDSkipFilter")
            shinyjs::show(id = "BOLDNullSpecies")
            shinyjs::show(id = "BOLDNullSpeciesWarning")
            . #return data matrix
          }
        }
    })
    
    BOLDOrgCountries <- eventReactive(input$BOLDfilterCountries, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList
        input$selectCountry #Returns a list of countries
    })
    
    
    # * BOLD MATRIX----------------
    
    BoldMatrix <- reactive({# creates and returns the matrix to be displayed with the count
      boldCoverage() %...>% {
        list <- .
          data <- list[["results"]]
        # remove ncbi
        if (input$removeNCBI == TRUE){
          data <- subset(data, genbank_accession == "")
        }
        country_list <- BOLDOrgCountries()
        if(length(country_list) > 0){
          data <- subset(data, country %in% country_list)
        }
        data
      }
    })
    
    # * Plot: Unique Species per Country ----------
    # country summary function
    # no. of species for all countries
    
    BoldPlotBarGraph <- function(){
      print("Entering BoldPlotBarGraph")
      print(BOLDOrgCountries())
      if (!is.null(BOLDOrgCountries())){
        vals <- c()
        countries_values <- list()
        selectCountry <- input$selectCountry
        require(data.table)
        BoldMatrix() %...>% {
          bold_matrix <- .
          present_matrix <- bold_functions$presentMatrix(bold_matrix, selectCountry)
          countries <- colnames(present_matrix)
          for (i in 1:length(countries)){
            if (countries[i] == ""){
              countries[i] <- "no country listed"
            } 
            countries_values[[countries[i]]] <- 0
          }
          
          # set vals
          ## counts # of cols for each country where cell > 0
          vals <- colSums(present_matrix != 0)
          
          print("vals = ")

          ## BOLD adds species/subspecies to search results
          ## so # of species will often be more than # from boldOrganismList()
          max_uniq_species <- max(vals)
          
          xf <- data.frame(country = countries, values = vals)
          ggplot(data=xf, aes(x = country, y = values)) +
            geom_bar(stat="identity", fill="purple") +
            labs(y = "# unique species", x = "countries", title = "Number of Unique Species in Selected Countries") +
            scale_y_continuous(limits = c(0, max_uniq_species)) +
            theme(text = element_text(size = 17), axis.ticks.length = unit(0.25, "cm")) +
            coord_flip()
        }
      }
    }
    
    
    output$species <- renderPlot ({
      BoldPlotBarGraph()
    }, height = 700, width = 1000)
    
    output$downloadBarGraph = downloadHandler(
      filename = 'bold_bargraph.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        BoldPlotBarGraph() %...>%
        ggsave(file, device = device)
      })
    
    
    # * Plot: Total sequences per Country ----------
    # for the treemap
    
    BoldPlotTreemap <- function(){
      if (!is.null(input$selectCountry)){
        BoldMatrix() %...>% {
          records_bold <- .
          countries_values <- list()
          vals <- c()
          countries = c(unique(records_bold$country))
          # replace the empty str w/ no country listed
          for (i in 1:length(countries)){
            if (countries[i] == ""){
              countries[i] = "no country listed"
            }
          }
          
          for (i in countries){
            x <- lengths(subset(records_bold, country == i))
            countries_values[[i]] = x[[1]]
            vals <- append(vals, x[[1]])
          }
          xf <- data.frame(country = countries, values = vals)
          geom.text.size = 7
          theme.size = (14/5) * geom.text.size
          #d3tree(
          #  treemap(xf, 
          #          index = "country", 
          #          vSize = "values", 
          #          vColor = "country", 
          #          type = "value",
          #          title.legend = "Legend"))
          ggplot(xf, aes(area = vals, fill = countries, label=countries)) +
            geom_treemap() +  
            #labs(title = "Distribution of Sequence Records Amongst Selected Countries") +
            geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE, reflow = TRUE) +
            ggtitle("Distribution of Sequence Records Amongst Selected Countries") +
            theme(axis.text = element_text(size = theme.size),
                  axis.title = element_text(size = 20, face = "bold")) 
          # how to change the colors + get a legend ?
        }
    }}
    
    output$treemap <- renderPlot({ 
        # from https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
        BoldPlotTreemap()
      }, height = 700, width = 1000)
  
    
    output$downloadTreeGraph = downloadHandler(
      filename = 'test.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        BoldPlotTreemap() %...>%
        ggsave(file, device = device)
      })
    
    
    output$BOLDcoverageResults <- 
      DT::renderDataTable({
        BoldMatrix() %...>% {
          bold_functions$reduce_barcode_summary(
            bold_functions$barcode_summary(.)
          )
        }
      })

    output$BOLDPresentTable <- 
      DT::renderDataTable({
        BoldMatrix() %...>% {
          bold_functions$presentMatrix(., input$selectCountry)
        }
      })
    
    output$BOLDAbsentTable <- 
      DT::renderDataTable({
        promise_all(matrix = BoldMatrix(), coverage = boldCoverage()) %...>% with({
          bold_functions$absentMatrix(matrix, 
                                      input$selectCountry, 
                                      na.omit(coverage$input$selectCountry[coverage$input$selectCountry != ""])
                                      )
        })
      })

    output$BOLDNATable <- 
      DT::renderDataTable({
        BoldMatrix() %...>% {
        bold_functions$naBarcodes(.)
        }
      })
    
    output$BOLDSummaryData <- 
      DT::renderDataTable(
        summary_report(2))
  
    output$BOLDNullSpecies <-
      renderText({
        unfound_species
        })
    
    output$BOLDNullSpeciesWarning <-
      renderText({
        "Warning: The following organisms were not found"
      })
    
    output$selectCountry <- renderUI({
      boldCoverage() %...>% {
        coverage <- .
        selectizeInput(inputId="selectCountry", label="Filter by Countries", choices=coverage$countries, selected = NULL, multiple = TRUE,options = NULL, width = 500)
      }
    })

    # * BOLDFASTADownload ------------------------------------------------------------
    
    output$downloadBoldFasta <- downloadHandler(
      filename = function() {
        paste("BOLD", ".fasta", sep="")
      },
      content = function(file) {
        fasta_vector = c()
        BoldMatrix() %...>% {
          records_bold <- .
            for(i in 1:length(records_bold$species_name)){
              #parsing data into fasta file format and storing in fasta_vector
              
              #display species name if subspecies name is not available
              
              species_name <- if(is.na(records_bold$subspecies_name[i])) records_bold$species_name[i] else records_bold$subspecies_name[i]
              
              #put data into vector
              org_vector <- c(records_bold$processid[i], species_name, records_bold$markercode[i], records_bold$genbank_accession[i])
              #remove empty data
              org_fasta <- org_vector[org_vector != '']
              org_data <- paste(org_fasta, collapse = '|')
              org_data <- paste('>', org_data, sep = '')
              
              #do not include entries with no sequences
              
              if (records_bold$nucleotides[i] != '' && !is.na(records_bold$nucleotides[i])){
                fasta_vector <- c(fasta_vector, org_data)
                fasta_vector <- c(fasta_vector, records_bold$nucleotides[i])
              }
            }
          
          write(fasta_vector, file)
        }
      }
    )
    
    # * BOLDSummaryDownload ------------------------------------------------------------
    
    output$downloadBoldSummary <- downloadHandler(
      filename = function() {
        paste("BOLD_Summary_Report", ".csv", sep="")
      },
      content = function(file) {
        summary_report %...>% {
          report <- .
          write.csv(report, file)
        }
      }
    )
    
})

