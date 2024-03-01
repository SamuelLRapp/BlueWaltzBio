#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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
})


orgListHelper <- modules::use("orgListHelper.R")
server_functions <- modules::use("server_functions.R")
bold_functions <- modules::use("bold_functions.R")


plan(multisession)
options(future.rng.onMisuse="ignore")

shinyServer(function(input, output, session) {
  
  # Hiding BOLD Page tabs
  hideTab("BOLDpage", "Organism Names")
  hideTab("BOLDpage", "Plot Unique Species Per Country")
  hideTab("BOLDpage", "Plot Total Sequences Per Country")
  hideTab("BOLDpage", "Filters")
  hideTab("BOLDpage", "Coverage Matrix")
  hideTab("BOLDpage", "Summary Data")
  hideTab("BOLDpage", "Country Data")
  hideTab("BOLDpage", "Manual Data Processing Required")
  hideTab("BOLDpage", "Species Not Found in BOLD Database")
  
  
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

  # max number of homonyms to return if any
  # are found in the homonym check.
  maxHomonyms <- 5L
  
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
  
  mostRecentFile <- function(dirpath) {
    #snippet from https://stackoverflow.com/a/50870673
    df <- file.info(list.files(dirpath, full.names = T))
    rownames(df)[which.max(df$mtime)]
  }
  
  
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
      server_functions$setNcbiKeyIsValid(TRUE)
    }, error = function(err) {
      shinyalert("Your API key has been rejected, 
                 please make sure it is correct",
                 type = "warning")
      server_functions$setNcbiKeyIsValid(FALSE)
    })
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
    progress <-
      AsyncProgress$new(
        session,
        min = 0,
        max = 1,
        message = "Retrieving...",
        value = 0
      )
    js$setLoaderAppearance("FullGenome")
    future_promise({
      server_functions$getGenomeSearchFullResults(
        dbOption = dbOption, 
        orgList = orgList, 
        taxizeOption = taxizeOption,
        refSeqChecked = refSeqChecked,
        progress = progress)
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
      resultsMatrix <- searchResults[[1]]
      statefulUids <- c(resultsMatrix[[2]])
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
  output$fullGenomeDownloadF <- downloadHandler(
    filename = function() {
      paste("Full_Genome_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      fullGenomeSearch() %...>% {
        resultsMatrix <- .[[1]]
        statefulUids <- c(resultsMatrix[[2]])
        progLength <- length(statefulUids)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          # entrez_fetch can take a list of uids, instead of iterating
          # over all uids and sleeping at each one, could provide a list
          # to get all the files at once. 
          # See https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EFetch_
          # for details
          write(entrez_fetch(db="nucleotide", id=statefulUids, rettype="fasta"), file)
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )
  
  # * Download Genbank files ---------------------------------------------------
  
  output$fullGenomeDownloadG <- downloadHandler(
    filename = function() {
      paste("Full_Genome_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      fullGenomeSearch() %...>% {
        resultsMatrix <- .[[1]]
        statefulUids <- c(resultsMatrix[[2]])
        progLength <- length(statefulUids)
        progLength <- length(statefulUids)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          # entrez_fetch can take a list of uids, instead of iterating
          # over all uids and sleeping at each one, could provide a list
          # to get all the files at once. 
          # See https://www.ncbi.nlm.nih.gov/books/NBK25499/#_chapter4_EFetch_
          # for details
          write(entrez_fetch(db="nucleotide", id=statefulUids, rettype="gb"), file)
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )

  # * Download Full Genome Results Table ---------------------------------------
  
  output$fullGenomeDownloadT <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("Full_Genome_Table", ".csv", sep = "")
    },
    content = function(file) {
      fullGenomeSearch() %...>% {
        write.csv(.[[1]], file)
      }
    }
  )

# CRUX ----------------------------------------------------------------------
  # progress bar object
  progressCRUX <- NULL
  # * CRUXSearchButton --------------------------------------------------------
  
  CRUXOrgList <- eventReactive(input$searchButton, {
    input$CRUXorganismList
  })
    
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
  
  
  cruxOrgSearch <- reactive({
    organismListGet() %...>% {
      homonymFlag <- input$CRUXhomonymOption
      future_promise(
        server_functions$getCruxSearchFullResults(., progressCRUX, homonymFlag))
    }
  })
    
  
  
  # * UI pipeline updates ------------------------------------------------------
  observeEvent(input$searchButton, {
    # Begin CRUX search
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
  
  observeEvent(input$detailsButton, {
    # Begin CRUX search
    updateTabsetPanel(session, "CRUXpage", selected = "Coverage Matrix")
    showTab("CRUXpage", "Coverage Matrix")
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
  # progress bar object
  progressNCBI <- NULL
  # * NCBISearchButton ---------------------------------------------------------
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
  observeEvent(input$NCBIsearchButton, {
    # Start NCBI search button
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
  
  observeEvent(input$NCBIdetailsButton, {
    # Start NCBI search button
    updateTabsetPanel(session, "NCBIpage", selected = "Coverage Matrix")
    showTab("NCBIpage", "Coverage Matrix")
    showTab("NCBIpage", "Summary Results")
  })
  
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
  
  observeEvent(input$NCBIStartOverSummary, {
    # Start NCBI search all over again
    updateTabsetPanel(session, "NCBIpage", selected = "Start Your NCBI Search")
    hideTab("NCBIpage", "Organism Names")
    hideTab("NCBIpage", "Barcodes of Interest")
    hideTab("NCBIpage", "One last step!")
    hideTab("NCBIpage", "Coverage Matrix")
    hideTab("NCBIpage", "Summary Results")
    updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(""))
    updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(""))
  })
  
  observeEvent(input$BarcodesNext, {
    # Go the barcodes tab to allow user to input them
     updateTabsetPanel(session, "NCBIpage", selected = "Barcodes of Interest")
     showTab("NCBIpage", "Barcodes of Interest")
   })
  
  observeEvent(input$NCBIRetMaxButton, {
    # Go the barcodes tab to allow user to input them
    updateTabsetPanel(session, "NCBIpage", selected = "One last step!")
    showTab("NCBIpage", "One last step!")
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
  
  # Download Fasta Files
  output$fileDownloadF <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI", ".zip", sep = "")
    },
    content = function(downloadedFile) {
      #good explanation of setwd zipping approach here: https://stackoverflow.com/a/43939912
      setwd(tempdir())
      ncbiSearch() %...>% {
        barcodes <- barcodeList()
        uidsMatrix <- matrix(.[[2]], ncol = length(barcodes), byrow = TRUE)
        future_promise({
          filenames <- lapply(barcodes, function(barcode) {
            file_path <- paste("NCBI", barcodes, "sequences.fasta", sep="_")
          })[[1]]
          i <- 0
          #apply across columns
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
              # Writes the vector containing all the fasta file information into
              # one fasta file
              i <<- i+1
              fn <- filenames[i]
              file.create(fn)
              write(Vector_Fasta, fn)
          })
          zip(zipfile = downloadedFile, files = filenames)
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
        future_promise({
          # Download Genbank files from NCBI
          Vector_genbank <-
              entrez_fetch(db = "nucleotide",
                           id = .,
                           rettype = "gb")  
          # Writes the vector containing all the genbank file information into
          # one genbank file
          write(Vector_genbank, file, append = TRUE) 
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
  
  
  #outputs:
  output$seqLenInputs <- renderUI(seqLenList())
  
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
  
  #Download Search Terms:
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

  # Format the dataframe for each database correctly 
  # Then send it to the summary data function for processing
  # Returns the summary report data table
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
  
  
  # * BOLDSearchButton ------------------------------------------------------
    BOLDOrgSearch <- eventReactive(input$BOLDsearchButton, { #When searchButton clicked, update BOLDOrgSearch to return the value input into BOLDorganismList
      input$BOLDorganismList #Returns as a string
    })
    
    progressBOLD <- NULL
    observeEvent(input$BOLDsearchButton, {
      updateTabsetPanel(session, "BOLDpage", selected = "Species Not Found in BOLD Database")
      shinyjs::show(id = "BOLDNullSpecies")
      showTab("BOLDpage", "Filters")
      showTab("BOLDpage", "Species Not Found in BOLD Database")
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
      showTab("BOLDpage", "Species Not Found in BOLD Database")
      click("BOLDfilterCountries")
    })
    
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
      showTab("BOLDpage", "Species Not Found in BOLD Database")
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
          taxize_results <- .
          organismList <- taxize_results$results
          organismListLength <- length(organismList)
          validate(
            need(organismListLength > 0, 'Please name at least one organism')
          )
          
          #puts variable in global scope
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
                searchResult <- 1
              }, error = function(err) {
                print("ERROR IN BOLD SEARCH")
                error <- 1
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
              # POP UP TELLING USER THAT BOLD IS DOWN
            }
            shinyjs::show(id = "BOLDClearFilter")
            shinyjs::show(id = "BOLDfilterCountries")
            shinyjs::show(id = "BOLDSkipFilter")
            shinyjs::show(id = "BOLDNullSpeciesWarning")
            shinyjs::show(id = "removeNCBICol")
            shinyjs::show(id = "countryFilterCol")
            shinyjs::show(id = "bold_species_not_found_panel")
            returnMatrix <- . #return data matrix
            returnMatrix
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
          data <- subset(data, is.na(genbank_accession))
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
      if (!is.null(BOLDOrgCountries())){
        vals <- c()
        countries_values <- list()
        selectCountry <- input$selectCountry
        require(data.table)
        BoldMatrix() %...>% {
          bold_matrix <- .
          present_matrix <- bold_functions$presentMatrix(bold_matrix, selectCountry)
          countries <- colnames(present_matrix)
          
          # set vals
          ## counts # of cols for each country where cell > 0
          vals <- colSums(present_matrix != 0)
          
          ## BOLD adds species/subspecies to search results
          ## so # of species will often be more than # from boldOrganismList()
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
        BoldPlotBarGraph() %...>%{
            ggsave(file, device = device)
        }
      })
    
    
    # * Plot: Total sequences per Country ----------
    # for the treemap
    
    BoldPlotTreemap <- function(){
      if (!is.null(input$selectCountry)){
        BoldMatrix() %...>% {
          records_bold <- .
          #table gets counts, data.frame converts table to dataframe
          #ggplot only accepts dataframes
          xf <- records_bold[c("country")] %>%
            table %>%
            data.frame
          
          geom.text.size = 7
          theme.size = (14/5) * geom.text.size
          
          #reference for changing titles: http://www.sthda.com/english/wiki/ggplot2-title-main-axis-and-legend-titles
          #the "." column holds the countries (.data refers to xf)
          #using .data instead of xf to avoid warnings about it
          ggplot(xf, aes(area = .data[["Freq"]], fill = .data[["country"]], label = .data[["country"]])) +
            geom_treemap() +  
            geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE, reflow = TRUE) +
            ggtitle("Distribution of Sequence Records Amongst Selected Countries") +
            theme(axis.text = element_text(size = theme.size),
                  plot.title = element_text(size = 20, face = "bold")) +
            labs(fill = "Countries")
        }
    }}
    
    output$treemap <- renderPlot({ 
        # from https://stackoverflow.com/questions/25061822/ggplot-geom-text-font-size-control
        BoldPlotTreemap()
      }, height = 700, width = 1000)
  
    
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
        promise_all(matrix = boldCoverage(), coverage = boldCoverage()) %...>% with({
          bold_functions$absentMatrix(matrix$results, 
                                      input$selectCountry
                                      )
        })
      })

    output$BOLDNATable <- 
      DT::renderDataTable({
        boldCoverage() %...>% {
        bold_functions$naBarcodes(.$results)
        }
      })
    
    output$BOLDSummaryData <- 
      DT::renderDataTable(
        summary_report(2))
  
    output$BOLDNullSpecies <-
      DT::renderDataTable(
        boldCoverage() %...>% {
          df <- .
          bold_functions$missingSpecies(df["Missing Species"])
        })
    
    
    output$BOLDNullSpeciesWarning <-
      renderText({
        "Warning: The following organisms were not found"
      })
    
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
    
    output$downloadBoldFasta <- downloadHandler(
      filename = function() {
        paste("BOLD", ".zip", sep="")
      },
      content = function(downloadedFile) {
        #make temporary directory to hold files to be zipped
        setwd(tempdir())
        
        BoldMatrix() %...>% {
          records_bold <- .
          grouped_rb <- records_bold %>%
            select(c("processid", "subspecies_name", "species_name", "markercode", "nucleotides", "genbank_accession")) %>%
            group_by(markercode) 
 
          grouped_rb %>%
            group_walk(function(barcode_table, barcode){
              fasta_vector = c()
              for(i in 1:length(barcode_table$species_name)){
                
                #display species name if subspecies name is not available
                species_name <- if(is.na(barcode_table$subspecies_name[i]) || barcode_table$subspecies_name[i] == "") barcode_table$species_name[i] else barcode_table$subspecies_name[i]
                
                #put data into vector
                org_vector <- c(barcode_table$processid[i], species_name, barcode, barcode_table$genbank_accession[i])
                #remove empty data
                org_fasta <- org_vector[org_vector != '']
                org_data <- paste(org_fasta, collapse = '|')
                org_data <- paste('>', org_data, sep = '')
                
                #do not include entries with no sequences
                
                if (barcode_table$nucleotides[i] != '' && !is.na(barcode_table$nucleotides[i])){
                  fasta_vector <- c(fasta_vector, org_data)
                  fasta_vector <- c(fasta_vector, barcode_table$nucleotides[i])
                }
              }
              filename <- paste0("BOLD_", barcode[[1,1]], "_sequence")
              file_path <- paste0(filename, ".fasta")
              file.create(file_path)
              write(fasta_vector, file_path)
            })
          all_codes <- grouped_rb %>% 
            group_keys()

          filenames <- unlist(lapply(all_codes[[1]], function(x){paste0("BOLD_",x,"_sequence",".fasta")}))
          zip(zipfile = downloadedFile, files = filenames)
        }
      }
    )
    
    # * BOLDSummaryDownload ----------------------------------------------------
    
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

