# -----------------------------------------------------------------------------#
# Name: Server.R
# Last Date Updated : September 22, 2024
#
# This code was build by Sriram Ramesh and Jorge Tapias Gomez
# With help from Dickson Chung and Zia Truong
#
# In collaboration with Samuel Rapp, Benjamine Levine, and Daniel Tapias Gomez
# Also, a big thanks to all those that helped us along the project.
# -----------------------------------------------------------------------------#


# Temporary example template (DELTE ONCE PUBLISHED)
## ----#
# Country Summary function
#   - Input: 
#         bold_coverage_df: The dataframe returned by the bold api with all the results 
#   - Output:
#         New dataframe with the number of times a species and country co-occur
#         Used for our country summary data displaying #of species per country 
## ----#


# Imports ----------------------------------------------------------------------

suppressPackageStartupMessages({
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
  library(plotly)
  library(treemapify)
  library(ggplot2) 
  library(zip)
})

# Import Modules
orgListHelper <- modules::use("./server_files/orgListHelper.R")
server_functions <- modules::use("./server_files/server_functions.R")
bold_functions <- modules::use("./server_files/bold_functions.R")

# Prepare concurrency
plan(multisession)
options(future.rng.onMisuse="ignore")

shinyServer(function(input, output, session) {
  
  # At start of execution hide all Page tabs within the pipeline

  # Hiding BOLD
  hideTab("BOLDpage", "Organism Names")
  hideTab("BOLDpage", "Plot Unique Species Per Country")
  hideTab("BOLDpage", "Plot Total Sequences Per Country")
  hideTab("BOLDpage", "Filters")
  hideTab("BOLDpage", "Coverage Matrix")
  hideTab("BOLDpage", "Summary Data")
  hideTab("BOLDpage", "Country Data")
  hideTab("BOLDpage", "Manual Data Processing Required")
  hideTab("BOLDpage", "Absent/Invalid Metadata")
  
  # Hiding CRUXpage
  hideTab("CRUXpage", "Coverage Matrix")
  hideTab("CRUXpage", "Organism Names")
  hideTab("CRUXpage", "Summary Results")
  
  # Hiding NCBI
  hideTab("NCBIpage", "Organism Names")
  hideTab("NCBIpage", "Barcodes of Interest")
  hideTab("NCBIpage", "One last step!")
  hideTab("NCBIpage", "Coverage Matrix")
  hideTab("NCBIpage", "Summary Results")
  
  # Hiding Full Genome Tab 
  hideTab("FullGenomePage", "Results")
  hideTab("FullGenomePage", "Organism Names")
  hideTab("FullGenomePage", "Summary Results")
  
# NCBI Key ---------------------------------------------------------------------
  # Verifies the provided api key by performing a search with it.
  # Very useful for CRUX and NCBI
  # Sets key validity variable in server_functions.R
  
  ## ----#
  # Set NCBI function
  #   - Input: 
  #         SetKey: Reactive variable indicating if the user the NCBI key button
  #   - Output:
  #         The function checks if the user given NCBI key is valid, if so we set
  #         the entrez key to that value and set the NCBI Key as valid. 
  #         This helps speed up searches as you won't be limited.
  #         If it key fails then we return a warning to the user in the form of a pop-up 
  ## ----#
  observeEvent(input$SetKey, {
    keyValidity <- tryCatch({
      entrez_search(
        db = "nucleotide",
        term = "Gallus Gallus",
        api_key = input$NCBIKey)
      shinyalert("Your API key has been accepted", type = "success")
      set_entrez_key(input$NCBIKey)
      server_functions$setNcbiKeyIsValid(TRUE)
    }, error = function(err) {
      shinyalert("Your API key has been rejected, 
                 please make sure it is correct",
                 type = "warning")
      server_functions$setNcbiKeyIsValid(FALSE)
    })
  })

# CRUX ----------------------------------------------------------------------
  
  # progress bar object for CRUX search
  progressCRUX <- NULL
  
  # * CRUXSearchButton --------------------------------------------------------
  
  # Button to start searches when user is ready to proceed
  CRUXOrgList <- eventReactive(input$searchButton, {
    input$CRUXorganismList
  })
    
  
  ## ----#
  # Organism List Get function for CRUX
  #   - Input: 
  #         None
  #   - Output:
  #         The user-given organism list after returning 
  #         from the taxize helper function to help with typos
  ## ----#
  organismListGet <- reactive({
    orgSearch <- CRUXOrgList()
    taxizeBool <- input$CRUXtaxizeOption
    future_promise({
      result <- orgListHelper$taxizeHelper(orgSearch, taxizeBool)
    }) %...>% {
      result <- .
      if(result$status == 1){
        shinyalert("Taxize ran into an error", 
                   text = "Unfortunately, some errors occured while running taxize. 
                   Please make sure you have added your own NCBI key to make sure you don't run into Rate Limiting errors.
                   If you have already added an NCBI key, then Taxize is most likely down and you may need to try again later.",
                   html = TRUE,
                   type = "warning")
      }
      showTab("CRUXpage", "Coverage Matrix")
      result$results
    }
    
  })
  
  ## ----#
  # Organism List Get function for CRUX
  #   - Input: 
  #         None
  #   - Output:
  #         Dataframe with the Coverage Matrix data table
  ## ----#
  cruxOrgSearch <- reactive({
    organismListGet() %...>% {
      homonymFlag <- input$CRUXhomonymOption
      future_promise(
        server_functions$getCruxSearchFullResults(., progressCRUX, homonymFlag))
    }
  })
    
  
  
  # * UI pipeline updates ------------------------------------------------------

  # Observe Event to begin CRUX search
  observeEvent(input$searchButton, {
    updateTabsetPanel(session, "CRUXpage", selected = "Summary Results")
    progressCRUX <<-
      AsyncProgress$new(
        session,
        min = 0,
        max = 1,
        message = "Retrieving...",
        value = 0
      )
    js$setLoaderAppearance("CRUX") #This may need to be changed?
    showTab("CRUXpage", "Summary Results")
  })
  
  # Observe Event when Coverage matrix is ready display the Tab
  observeEvent(input$detailsButton, {
    # Begin CRUX search
    updateTabsetPanel(session, "CRUXpage", selected = "Coverage Matrix")
    showTab("CRUXpage", "Coverage Matrix")
  })
  
  # * CRUXInputCSV -------------------------------------------------------------
  
  # Observe Event function to upload a CSV file with species
  # so that the user doesn't have to manually type them
  # Simply parse the csv and upade the text area input
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
                     
                     # Beware, adding a space into the column header name causes problems
                     column.header = "OrganismNames",
                     textbox.id = "CRUXorganismList"
                   )
                   updateTextAreaInput(
                     session = getDefaultReactiveDomain(),
                     inputId = "CRUXorganismList",
                     value = newOrganismNamesList)
                 })
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
  
  # Render CRUX Coverage Matrix
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
  
  # Render CRUX Summary Data Table
  output$CRUXSummaryResults <- DT::renderDataTable({
    promise_all(data_df = summary_report(0), 
                rows = organismListGet()) %...>% with({
                  DT::datatable(
                    data_df
                  )
                })
  })
  
  # NCBI -----------------------------------------------------------------------
  # Progress bar object
  progressNCBI <- NULL
  
  # * NCBISearchButton ---------------------------------------------------------
  
  ## ----#
  # NCBI Search function
  #   - Input: 
  #         None, it waits for the search button to be pressed
  #         But the function needs user-give 1) barcode list and
  #         2) an organism list.
  #   - Output:
  #         A list with the counts per organisms, the unique NCBI ids
  #         The search terms used for the API query and the organism List
  ## ----#
  ncbiSearch <- eventReactive(input$NCBIsearchButton, {
    barcodeList <- barcodeList()
    organismList <- input$NCBIorganismList
    
    searchOptionGene <- input$NCBISearchOptionGene
    searchOptionOrgn <- input$NCBISearchOptionOrgn
    downloadNumber <- input$downloadNum
    seqLengthOption <- input$seqLengthOption
    ncbiTaxizeOption <- input$NCBItaxizeOption
    seq_len_list <- server_functions$getSeqLenList(barcodeList, input)
    future_promise({
      uids <- list()
      searchTerms <- list()
      countResults <- list()
      taxize_results <- orgListHelper$taxizeHelper(organismList, ncbiTaxizeOption)
      organismList <- taxize_results$results
      organismsDownloaded <- 0
      organismListLength <- length(organismList)
      progressNCBI$set(detail = paste0("0","/",organismListLength))
      for (organism in organismList) {
        progressNCBI$set(message = paste0("Retrieving barcodes for ", organism))
        progressNCBI$inc(amount = 0.5/organismListLength)
        for (code in barcodeList) {
          searchTerm <- server_functions$getNcbiSearchTerm(organism, code, searchOptionGene, searchOptionOrgn, seqLengthOption, seq_len_list[[code]])
          searchResult <- server_functions$getNcbiSearchFullResults("nucleotide", searchTerm, downloadNumber)
          uids <- list.append(uids, searchResult[[1]])
          countResults <- list.append(countResults, searchResult[[2]])
          searchTerms <- list.append(searchTerms, searchTerm) 
        }
        organismsDownloaded <- organismsDownloaded + 1
        progressNCBI$set(message = paste0("Retrieved barcodes for ", organism),
                     detail = paste0(organismsDownloaded,"/",organismListLength))
        progressNCBI$inc(amount = 0.5/organismListLength)
      }
      progressNCBI$close()
      results <- list(count = countResults, ids = uids, searchTermslist = searchTerms, organismList = organismList)
    })%...>% {
      showTab("NCBIpage", "Coverage Matrix")
      results <- .
    }
  })
  
  # * NCBI pipeline step display handlers --------------------------------------
  
  # Start NCBI search button
  # Shows NCBI Summary results 
  observeEvent(input$NCBIsearchButton, {
    updateTabsetPanel(session, "NCBIpage", selected = "Summary Results")
    progressNCBI <<-
      AsyncProgress$new(
        session,
        min = 0,
        max = 1,
        message = "Retrieving...",
        value = 0
      )
    js$setLoaderAppearance("NCBI")
    showTab("NCBIpage", "Summary Results")
  })
  
  # Displays the NCBI coverage matrix
  observeEvent(input$NCBIdetailsButton, {
    updateTabsetPanel(session, "NCBIpage", selected = "Coverage Matrix")
    showTab("NCBIpage", "Coverage Matrix")
    showTab("NCBIpage", "Summary Results")
  })
  
  # When starting over we hide all the tabs again and clean up the UI
  observeEvent(input$NCBIStartOver, {
    # Start NCBI search all over again
    updateTabsetPanel(session, "NCBIpage", selected = "Start Your NCBI Search")
    hideTab("NCBIpage", "Organism Names")
    hideTab("NCBIpage", "Barcodes of Interest")
    hideTab("NCBIpage", "Coverage Matrix")
    hideTab("NCBIpage", "Summary Results")
    updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(""))
    updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(""))
  })
  
  # When starting over in the summary tab we hide all the tabs again
  observeEvent(input$NCBIStartOverSummary, {
    updateTabsetPanel(session, "NCBIpage", selected = "Start Your NCBI Search")
    hideTab("NCBIpage", "Organism Names")
    hideTab("NCBIpage", "Barcodes of Interest")
    hideTab("NCBIpage", "One last step!")
    hideTab("NCBIpage", "Coverage Matrix")
    hideTab("NCBIpage", "Summary Results")
    updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(""))
    updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(""))
  })
  
  # Go the barcodes of interest tab
  observeEvent(input$BarcodesNext, {
     updateTabsetPanel(session, "NCBIpage", selected = "Barcodes of Interest")
     showTab("NCBIpage", "Barcodes of Interest")
   })
  
  # Go the download number page
  observeEvent(input$NCBIRetMaxButton, {
    updateTabsetPanel(session, "NCBIpage", selected = "One last step!")
    showTab("NCBIpage", "One last step!")
  })
  
  # Allows the user to upload csv files with the species and barcodes
  # they are interested in and avoid manually typing.
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
          value = c(head(uploadinfo$Barcodes[uploadinfo$Barcodes != "" && !is.na(uploadinfo$Barcodes)]),
                    input$barcodeList)
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "barcodeList",
                            value = uploadinfo$Barcodes[uploadinfo$Barcodes != "" && !is.na(uploadinfo$Barcodes)])
      }
    })
  })
  
  # * NCBIStrToList ------------------------------------------------------------
  
  # NCBI Button startd the NCBI Search when the user clicking the search button
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
  
  # Gather the barcodes that the user has provided, and clean them
  barcodeList <- reactive({
    # separate based on comma
    barcodeList <- strsplit(input$barcodeList, ",") 
    barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
    barcodeList[[1]] <- unique(barcodeList[[1]][barcodeList[[1]] != ""])
    barcodeList[[1]]
  })
  
  # * NCBISequenceLength -------------------------------------------------------
  
  # Returns a numericRangeInput object to display the min/max
  # UI object for the user to input sequence lengths
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
  
  # Extract the unique IDs from the NCBI search
  uidsGet <-
    function(){
      # Returns the uids stored in the results from the NCBi query
      then(ncbiSearch(), function(value) {
        View(value)
        # Get the results from the NCBI query
        uids <- c()
        for (i in value[[2]]) {
          uids <- c(uids, i)
        }
        uids
      })
    }
  
  
  # * NCBIDownloadFASTA --------------------------------------------------------
  
  # Download Fasta Files for all the barcodes (zipped) from NCBI
  output$fileDownloadF <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Fastas", ".zip", sep = "")
    },
    content = function(downloadedFile) {
      # Good explanation of setwd zipping approach here: https://stackoverflow.com/a/43939912
      setwd(tempdir())
      ncbiSearch() %...>% {
        barcodes <- barcodeList()
        codesLen <- length(barcodes)
        uidsMatrix <- matrix(.[[2]], ncol = codesLen, byrow = TRUE)
        progress <- AsyncProgress$new(session, min=0, max=codesLen,
                                      message = "Downloading Fasta files from NCBI")
        
        future_promise({
          filenames <- lapply(barcodes, function(barcode) {
            file_path <- paste("NCBI", barcodes, "sequences.fasta", sep="_")
          })[[1]]
          i <- 0
          # Apply across columns
          apply(uidsMatrix, MARGIN = 2, function(idCol) {
            
              # Download Fasta files from NCBI
              idsList <- unlist(idCol)
              Vector_Fasta = c("")
              if (length(idsList) > 0) {
                Vector_Fasta <-
                  entrez_fetch(db = "nucleotide",
                               id = idsList,
                               rettype = "fasta")
              }
              # Writes the vector containing all the fasta file information
              # into one fasta file
              i <<- i+1
              fn <- filenames[i]
              file.create(fn)
              write(Vector_Fasta, fn)
              progress$inc(1/codesLen)
          })
          zip(zipfile = downloadedFile, files = filenames)
          progress$close()
        })
      }
    }
  )
  
  
  # * NCBIDownloadGenbank ------------------------------------------------------
  
  # Download Genbank Files for all the barcodes (zipped) from NCBI
  output$fileDownloadG <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Genbank", ".zip", sep = "")
    },
    content = function(downloadedFile) {
      # Good explanation of setwd zipping approach here: https://stackoverflow.com/a/43939912
      setwd(tempdir())
      ncbiSearch() %...>% {
        barcodes <- barcodeList()
        codesLen <- length(barcodes)
        uidsMatrix <- matrix(.[[2]], ncol = codesLen, byrow = TRUE)
        progress <- AsyncProgress$new(session, min=0, max=codesLen,
                                      message = "Downloading Genbank files from NCBI")
        
        future_promise({
          filenames <- lapply(barcodes, function(barcode) {
            file_path <- paste("NCBI", barcodes, "sequences.gb", sep="_")
          })[[1]]
          i <- 0
          # Apply across columns
          apply(uidsMatrix, MARGIN = 2, function(idCol) {
            
            # Download Genbank files from NCBI
            idsList <- unlist(idCol)
            vectorGb = c("")
            if (length(idsList) > 0) {
              vectorGb <-
                entrez_fetch(db = "nucleotide",
                             id = idsList,
                             rettype = "gb")
            }
            # Writes the vector containing all the genbank file information 
            # into one genbank file
            i <<- i+1
            fn <- filenames[i]
            file.create(fn)
            write(vectorGb, fn)
            progress$inc(1/codesLen)
          })
          zip(zipfile = downloadedFile, files = filenames)
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
  # All the observe events below are for the barcode buttons (NCBI Pipeline)
  
  observeEvent(input$barcodeOptionCO1, {
    # Detects when the specific barcode (in this case CO1) 
    # button has been pressed
    
    # If the input barcodeList is not empty (ie. the inputtextarea is not 
    # empty) then use the paste function to the add the barcode/s to the 
    # beginning
    if (input$barcodeList[[1]] != "") {
      # Updates the text area input adds the barcode/s to the beginning of 
      # the barcodes already present
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("CO1 + COI + COXI + COX1,", input$barcodeList)
      ) 
    }
    else {
      # Here since the textarea is empty we just set its value to the barcode/s
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "CO1 + COI + COXI + COX1") 
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
        value = paste("trnL,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "trnL")
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
  
  
  # Min/Max sequence length boxes rendering
  output$seqLenInputs <- renderUI(seqLenList())
  
  # Render Table for Coverage Matrix
  output$NCBIcoverageResults <- DT::renderDataTable({
    then(ncbiSearch(), function(searchResults) {
      data_df <- server_functions$getNcbiResultsMatrix(searchResults, length(barcodeList()))
      rows <- searchResults[[4]]
      barcodes <- barcodeList()
      DT::datatable(
        data_df,
        rownames = rows,
        colnames = barcodes
      )
    })
  })
  
  # Render NCBI Search Terms Table
  output$NCBIsearchQueries <- DT::renderDataTable({
    then(ncbiSearch(), function(searchResults) {
      data_df <- server_functions$getNcbiSearchTermsMatrix(searchResults, length(barcodeList()))
      data <- data_df[1,,drop=FALSE]
      barcodes <- barcodeList()
      DT::datatable(
        t(data),
        rownames = barcodes,
        colnames = c(" "),
        options = list(filters = "none")
      )
    })
  })
  
  # Render NCBI Summary Report
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
      columns <- barcodeList()
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiResultsMatrix(searchResults, length(barcodeList()))
        colnames(df) <- columns
        rownames(df) <- rows
        write.csv(df, file)
      })
    }
  )
  
  # * NCBIDownloadSearchTerms --------------------------------------------------
  
  # Download Search Terms
  output$downloadStatements <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Search_Statements", ".csv", sep = "")
    },
    content = function(file) {
      columns <- barcodeList()
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        df <- server_functions$getNcbiSearchTermsMatrix(searchResults, length(barcodeList()))
        colnames(df) <- columns
        rownames(df) <- rows
        write.csv(df, file)
      })
    }
  )
  
  # * SummaryReport ------------------------------------------------------------

  ## ----#
  # Country Summary function
  #   - Input: 
  #         databaseFlag: 0) is for CRUX, 1) for NBCI, 2) (else) Bold
  #   - Output:
  #         Format the dataframe for each database correctly to generate the table
  #         Then send it to the summary data function for processing
  #         Returns the summary report data table
  ## ----#
  summary_report <- function(databaseFlag) {
    if (databaseFlag == 1) {
      columns <- barcodeList()
      then(ncbiSearch(), function(searchResults) {
        rows <- searchResults[[4]] #organismList
        dataframe <- as.data.frame(server_functions$getNcbiResultsMatrix(searchResults, 
                                                                         length(barcodeList())))
        colnames(dataframe) <- columns
        rownames(dataframe) <- rows
        server_functions$summary_report_dataframe(dataframe)
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
        server_functions$summary_report_dataframe(dataframe)
      }
    }
  }

  # BOLD --------------------------------------------------------------------
  
  # BOLD Progress Bar
  progressBOLD <- NULL
  
  # * BOLDSearchButton ------------------------------------------------------
  
    # When searchButton clicked, update BOLDOrgSearch to 
    # return the value input into BOLDorganismList
    BOLDOrgSearch <- eventReactive(input$BOLDsearchButton, {
      input$BOLDorganismList # Returns as a string
    })
    
    # Reset the UI (in case this isn't their first search) 
    # and initilize the progress bar
    observeEvent(input$BOLDsearchButton, {
      updateTabsetPanel(session, "BOLDpage", selected = "Absent/Invalid Metadata")
      shinyjs::show(id = "BOLDNullSpecies")
      showTab("BOLDpage", "Filters")
      showTab("BOLDpage", "Absent/Invalid Metadata")
      hideTab("BOLDpage", "Plot Unique Species Per Country")
      hideTab("BOLDpage", "Plot Total Sequences Per Country")
      hideTab("BOLDpage", "Coverage Matrix")
      hideTab("BOLDpage", "Summary Data")
      hideTab("BOLDpage", "Country Data")
      hideTab("BOLDpage", "Manual Data Processing Required")
      
      shinyjs::hide(id = "BOLDClearFilter")
      shinyjs::hide(id = "BOLDfilterCountries")
      shinyjs::hide(id = "BOLDSkipFilter")
      shinyjs::hide(id = "BOLDNullSpeciesWarning")
      shinyjs::hide(id = "removeNCBICol")
      shinyjs::hide(id = "BOLDClearFilter")
      shinyjs::hide(id = "bold_species_not_found_panel")
      
      progressBOLD <<-
        AsyncProgress$new(
          session,
          min = 0,
          max = 1,
          message = "Retrieving...",
          value = 0
        )
      js$setLoaderAppearance("BOLD")
    })

    
    # If the user skips filters we move to the next part of the pipeline
    # The next part of the pipeline being the Coverage Matrix
    observeEvent(input$BOLDSkipFilter, {
      updateTabsetPanel(session, "BOLDpage", selected = "Summary Data")
      showTab("BOLDpage", "Summary Data")
      showTab("BOLDpage", "Coverage Matrix")
      if (!is.null(input$selectCountry)){
        showTab("BOLDpage", "Plot Unique Species Per Country")
        showTab("BOLDpage", "Plot Total Sequences Per Country")
      } else {
        hideTab("BOLDpage", "Plot Total Sequences Per Country")
        hideTab("BOLDpage", "Plot Unique Species Per Country")
      }
      showTab("BOLDpage", "Country Data")
      showTab("BOLDpage", "Manual Data Processing Required")
      showTab("BOLDpage", "Absent/Invalid Metadata")
      click("BOLDfilterCountries")
    })
    
    # If the user wants to filter countries then we move
    # to the next stage of the pipeline the Coverage Matrix
    observeEvent(input$BOLDfilterCountries, {
      updateTabsetPanel(session, "BOLDpage", selected = "Summary Data")
      showTab("BOLDpage", "Summary Data")
      showTab("BOLDpage", "Coverage Matrix")
      if (!is.null(input$selectCountry)){
        showTab("BOLDpage", "Plot Unique Species Per Country")
        showTab("BOLDpage", "Plot Total Sequences Per Country")
      } else {
        hideTab("BOLDpage", "Plot Total Sequences Per Country")
        hideTab("BOLDpage", "Plot Unique Species Per Country")
      }
      showTab("BOLDpage", "Country Data")
      showTab("BOLDpage", "Manual Data Processing Required")
      showTab("BOLDpage", "Absent/Invalid Metadata")
    })
    
    # This is to make filtering easy by allowing scientist
    # to add countries to the filter by just clicking on them
    observeEvent(input$BOLDClearFilter, {
      updateSelectizeInput(inputId="selectCountry", selected = character(0))
    })
    
    # BOLD Start button that allows the user to upload a file
    # and shows the next tab of the pipeline
    observeEvent(input$BOLDStartButton, {
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
    
    ## ----#
    # Bold Coverage
    #   - Input: 
    #         No inputs directly to the function, but it reads the 
    #         organisms the user provided 
    #   - Output:
    #         Result: List with all the results from the query, species, countries, etc
    ## ----#
    boldCoverage <- reactive ({
        orgSearch <- BOLDOrgSearch()
        taxize_selected <- input$BOLDtaxizeOption
        future_promise({orgListHelper$taxizeHelper(orgSearch, taxize_selected)}) %...>% {
          taxize_results <- .
          organismList <- taxize_results$results
          organismListLength <- length(organismList)
          validate(
            need(organismListLength > 0, 'Please name at least one organism')
          )
          
          # Puts variable in global scope
          unfound_species <- c()
          searchResult <- 0
          results <- data.frame(matrix(ncol=0, nrow=0))
          future_promise({
            records_bold <- NA
            countries <- c()
            organismsDownloaded <- 0
            progressBOLD$set(detail = paste0("0","/",organismListLength))
            for(organism in organismList){
              progressBOLD$set(message = paste0("Retrieving barcodes for ", organism))
              progressBOLD$inc(amount = 0.5/organismListLength)
              searchResult <- tryCatch({
              records_bold <- bold_seqspec(taxon = organism)
              if (!all(is.na(records_bold))) {
                records_bold <- subset(records_bold, select = c("country", "species_name", "markercode", "processid", "subspecies_name", "nucleotides", "genbank_accession"))
              }
              searchResult <- 1
              }, error = function(err) {
                print("ERROR IN BOLD SEARCH")
                organism <<- paste(organism, "(invalid)")
                records_bold <<- NULL
              })
              if (!is.null(records_bold) && !all(is.na(records_bold))){
                records_bold[records_bold == ''] <- NA
                records_bold <- records_bold %>% mutate(country = ifelse(is.na(country), "No Country Listed", country))
                records_bold <- records_bold %>% filter(!is.na(species_name)  & !is.na(markercode))
                countries <- c(countries, records_bold$country)
                results <- rbind(results, records_bold)
              } else {
                  unfound_species <- c(unfound_species, organism)
              }
              organismsDownloaded <- organismsDownloaded + 1
              progressBOLD$set(message = paste0("Retrieved barcodes for ", organism),
                               detail = paste0(organismsDownloaded,"/",organismListLength))
              progressBOLD$inc(amount = 0.5/organismListLength)
            }
            progressBOLD$close()
            results <- list(results=results, countries=countries, "Missing Species"=unfound_species)
          }) %...>% {
            if(searchResult == 1){
              print("BOLD is down")
              # Bold is down not much we can do
              # TODO: POP UP TELLING USER THAT BOLD IS DOWN
            }
            shinyjs::show(id = "BOLDClearFilter")
            shinyjs::show(id = "BOLDfilterCountries")
            shinyjs::show(id = "BOLDSkipFilter")
            shinyjs::show(id = "BOLDNullSpeciesWarning")
            shinyjs::show(id = "removeNCBICol")
            shinyjs::show(id = "countryFilterCol")
            shinyjs::show(id = "bold_species_not_found_panel")
            returnMatrix <- . # Return data matrix
            returnMatrix
          }
        }
    })
    
    # * BOLD MATRIX----------------
    
    ## ----#
    # Bold Matrix
    #   - Input: 
    #         No direct input, but takes n the list from BOLD coverage with all the results
    #   - Output:
    #         data: Filtered data, without any NAs, removing NCBI duplicate entries,
    #               removing the filtered countries.
    ## ----#
    BoldMatrix <- reactive({
      boldCoverage() %...>% {
        list <- .
        data <- list[["results"]]
        
        # Remove duplicate ncbi entries
        if (input$removeNCBI == TRUE){
          data <- subset(data, is.na(genbank_accession))
        }
        
        # Remove countries
        country_list <- BOLDOrgCountries()
        if(length(country_list) > 0){
          data <- subset(data, country %in% country_list)
        }
        data
      }
    })
    
    # * Plot: Unique Species per Country  ----------
    # Build a Bar Graph counting all unique species per country
    BoldPlotBarGraph <- function(){
      if (!is.null(BOLDOrgCountries())){
        vals <- c()
        countries_values <- list()
        selectCountry <- input$selectCountry
        require(data.table)
        BoldMatrix() %...>% {
          bold_matrix <- .
          present_matrix <- bold_functions$presentMatrix(bold_matrix, selectCountry)
          countries <- colnames(present_matrix)
          
          # Set vals
          # Counts number of cols for each country where cell > 0
          vals <- colSums(present_matrix != 0)
          
          # BOLD adds species/subspecies to search results
          # So number of species will often be more than number from boldOrganismList()
          max_uniq_species <- max(vals)
          
          xf <- data.frame(country = countries, values = vals)
          ggplot(data=xf, aes(x = country, y = values)) +
            geom_bar(stat="identity", fill="purple") +
            labs(y = "Number of Unique Species", x = "Countries", title = "Number of Unique Species in Selected Countries") +
            scale_y_continuous(limits = c(0, max_uniq_species)) +
            theme(text = element_text(size = 17), axis.ticks.length = unit(0.25, "cm")) +
            coord_flip()
        }
      }
    }
    
    # Render BOLD Bar Graph
    output$species <- renderPlot ({
      BoldPlotBarGraph()
    }, height = 700, width = 1000)
    
    # Download BOLD Bar Graph
    output$downloadBarGraph = downloadHandler(
      filename = 'bold_bargraph.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        BoldPlotBarGraph() %...>%{
            ggsave(file, device = device)
        }
      })
    
    
    # * Plot: Total sequences per Country ----------
    # Create the Treemap visualization for BOLD
    # Representing how many species each country has
    BoldPlotTreemap <- function(){
      if (!is.null(input$selectCountry)){
        BoldMatrix() %...>% {
          records_bold <- .
          # Table gets counts, data.frame converts table to dataframe
          # ggplot only accepts dataframes
          xf <- records_bold[c("country")] %>%
            table
          xf <- as.data.frame(xf)
          colnames(xf) <- c("country", "Freq")
          
          geom.text.size = 7
          theme.size = (14/5) * geom.text.size
          
          # Reference for changing titles: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
          # the "." column holds the countries (.data refers to xf)
          # using .data instead of xf to avoid warnings about it
          ggplot(xf, aes(area = .data[["Freq"]], fill = .data[["country"]], label = .data[["country"]])) +
            geom_treemap() +  
            geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE, reflow = TRUE) +
            ggtitle("Distribution of Sequence Records Amongst Selected Countries") +
            theme(axis.text = element_text(size = theme.size),
                  plot.title = element_text(size = 20, face = "bold")) +
            labs(fill = "Countries")
        }
      }}
    
    # Render BOLD Treemap
    # from https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
    output$treemap <- renderPlot({ 
        BoldPlotTreemap()
      }, height = 700, width = 1000)
  
    
    # Dowload the BOLD Treemap
    output$downloadTreeGraph = downloadHandler(
      filename = 'bold_treegraph.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        BoldPlotTreemap() %...>% {
            ggsave(file, device = device)
        }
      })
    
    # Render Coverage Matrix for BOLD
    output$BOLDcoverageResults <- 
      DT::renderDataTable({
        BoldMatrix() %...>% {
          bold_functions$reduce_barcode_summary(
            bold_functions$barcode_summary(.)
          )
        }
      })

    # Render Present Countries Table for BOLD
    output$BOLDPresentTable <- 
      DT::renderDataTable({
        BoldMatrix() %...>% {
          bold_functions$presentMatrix(., input$selectCountry)
        }
      })
    
    # Render Absent Countries Table for BOLD
    output$BOLDAbsentTable <- 
      DT::renderDataTable({
        promise_all(matrix = boldCoverage(), coverage = boldCoverage()) %...>% with({
          bold_functions$absentMatrix(matrix$results, 
                                      input$selectCountry
                                      )
        })
      })

    # Render table with those species that did not have Barcodes for BOLD
    output$BOLDNATable <- 
      DT::renderDataTable({
        boldCoverage() %...>% {
        bold_functions$naBarcodes(.$results)
        }
      })
    
    # Render Summary Data table for BOLD
    output$BOLDSummaryData <- 
      DT::renderDataTable(
        summary_report(2))
  
    
    # Render table with those species that were completely missing from BOLD
    output$BOLDNullSpecies <-
      DT::renderDataTable(
        boldCoverage() %...>% {
          df <- .
          bold_functions$missingSpecies(df["Missing Species"])
        })
    
    # Render warning for users so they know what they table does
    output$BOLDNullSpeciesWarning <-
      renderText({
        "Warning: The following organisms were not found"
      })
    
    # Render Select Country Filter UI
    output$selectCountry <- renderUI({
      boldCoverage() %...>% {
        coverage <- .
        div(
          style = "padding: 2px; background-color: lightgray; width: 100%; border-radius: 10px; border: 2px solid black;",
          titlePanel("Country Filter"),
          selectizeInput(inputId="selectCountry", 
                         label=HTML("Select Country(s) You Wish to Filter By <br> (Please click on the dropdown below to view all the possible countries)"), 
                         choices=coverage$countries, 
                         selected = NULL, multiple = TRUE,options = NULL, width = 500),
          div(
            actionButton('BOLDClearFilter',"Remove Countries From Filter"),
          ),
        )
      }
    })

    # * BOLDFASTADownload ------------------------------------------------------------
    
    # Download Fasta Files for all the barcodes (zipped) from BOLD
    output$downloadBoldFasta <- downloadHandler(
      filename = function() {
        paste("BOLD", ".zip", sep="")
      },
      content = function(downloadedFile) {
        # Make a temporary directory to hold files to be zipped
        setwd(tempdir())
        
        BoldMatrix() %...>% {
          records_bold <- .
          
          grouped_rb <- records_bold %>%
            select(c("processid", "subspecies_name", "species_name", "markercode", "nucleotides", "genbank_accession")) %>%
            group_by(markercode) 
          
          codesLen <- length(grouped_rb)
          progress <- AsyncProgress$new(session, min=0, max=codesLen,
                                        message="Downloading Fastas from BOLD")
 
          grouped_rb %>%
            group_walk(function(barcode_table, barcode){
              
              fasta_vector = c()
              for(i in 1:codesLen){
                # Need to create Fasta format
                # Display species name if subspecies name is not available
                species_name <- if(is.na(barcode_table$subspecies_name[i]) || barcode_table$subspecies_name[i] == "") barcode_table$species_name[i] else barcode_table$subspecies_name[i]
                
                # Put data into vector
                org_vector <- c(barcode_table$processid[i], species_name, barcode, barcode_table$genbank_accession[i])
                # Remove empty data
                org_fasta <- org_vector[org_vector != '']
                org_data <- paste(org_fasta, collapse = '|')
                org_data <- paste('>', org_data, sep = '')
                
                # Do not include entries with no sequences
                
                if (barcode_table$nucleotides[i] != '' && !is.na(barcode_table$nucleotides[i])){
                  fasta_vector <- c(fasta_vector, org_data)
                  fasta_vector <- c(fasta_vector, barcode_table$nucleotides[i])
                }
              }
              filename <- paste0("BOLD_", barcode[[1,1]], "_sequence")
              file_path <- paste0(filename, ".fasta")
              file.create(file_path)
              write(fasta_vector, file_path)
              
              progress$inc(1/codesLen)
            })
          all_codes <- grouped_rb %>% 
            group_keys()

          filenames <- unlist(lapply(all_codes[[1]], function(x){paste0("BOLD_",x,"_sequence",".fasta")}))
          zip(zipfile = downloadedFile, files = filenames)
          
          progress$close()
        }
      }
    )
    
    # * BOLDSummaryDownload ----------------------------------------------------
    
    # Download BOLD summary table
    output$downloadBoldSummary <- downloadHandler(
      filename = function() {
        paste("BOLD_Summary_Report", ".csv", sep="")
      },
      content = function(file) {
        summary_report(2) %...>% {
          report <- .
          write.csv(report, file)
        }
      }
    )
    
    # * BOLD Coverage Matrix Download ------------------------------------------
    
    # Download BOLD Coverage Matrix
    output$downloadBoldMatrix <- downloadHandler(
      filename = function() {
        paste("BOLD_Coverage_Matrix", ".csv", sep="")
      },
      content = function(file) {
        BoldMatrix() %...>% {
          results <- bold_functions$reduce_barcode_summary(
            bold_functions$barcode_summary(.)
          )
          write.csv(results,file)
        }
      }
    )
    
    # * BOLD Sequences per Country Download ------------------------------------------
    
    # Download BOLD Present Species Table
    output$downloadBoldPresent <- downloadHandler(
      filename = function() {
        paste("BOLD_Sequences_per_Country", ".csv", sep="")
      },
      content = function(file) {
        BoldMatrix() %...>% {
          results <- bold_functions$presentMatrix(., input$selectCountry)
          write.csv(results,file)
        }
      }
    )
    
    
    # * BOLD Recommended Countries Download ------------------------------------------
    
    # Download BOLD absent species Table
    output$downloadBoldAbsent <- downloadHandler(
      filename = function() {
        paste("BOLD_Recommended_Country_Filters", ".csv", sep="")
      },
      content = function(file) {
        promise_all(matrix = boldCoverage(), coverage = boldCoverage()) %...>% with({
          results <- bold_functions$absentMatrix(matrix$results, 
                                                input$selectCountry
                                                )
          write.csv(results, file)
        })
      }
    )
    
    # * BOLD NA Barcodes Download ------------------------------------------
    
    # Download BOLD NA barcodes table
    output$downloadBoldNaBarcodes <- downloadHandler(
      filename = function() {
        paste("BOLD_NA_Barcodes", ".csv", sep="")
      },
      content = function(file) {
        BoldMatrix() %...>% {
          results <- bold_functions$naBarcodes(.)
          write.csv(results,file)
        }
      }
    )
    
    # * BOLD Null Species Download ------------------------------------------
    
    # Download BOLD not found species table
    output$downloadBoldNullSpecies <- downloadHandler(
      filename = function() {
        paste("BOLD_Missing_Species", ".csv", sep="")
      },
      content = function(file) {
        boldCoverage() %...>% {
          df <- .
          results <- bold_functions$missingSpecies(df["Missing Species"])
          write.csv(results, file)
        }
      }
    )
})

