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
  
  
  # NCBISearch <-
  #   eventReactive(input$NCBIsearchButton, {
  #     # When searchButton clicked, update NCBIOrgSearch to return the value 
  #     # input into NCBIorganismList
  #     list(input$NCBIorganismList, input$barcodeList) #Returns as a string
  #   })
  # 

  # removing reactive elements, need barcodes to be accessible
  barcodeList_ <- NULL
  
  
  ncbiSearch <- eventReactive(input$NCBIsearchButton, {
    barcodeList_ <<- input$barcodeList
    barcodeList_ <<- str_split(barcodeList_[[1]], ",")
    organismList <- input$NCBIorganismList
    searchOptionGene <- input$NCBISearchOptionGene
    searchOptionOrgn <- input$NCBISearchOptionOrgn
    downloadNumber <- input$downloadNum
    seqLengthOption <- input$seqLengthOption
    ncbiTaxizeOption <- input$NCBItaxizeOption
    seq_len_list <- getSeqLenList(barcodeList_[[1]])
    future_promise({
      uids <- list()
      searchTerms <- list()
      countResults <- list()
      organismList <- orgListHelper$taxizeHelper(organismList, ncbiTaxizeOption)
      for (organism in organismList) {
        for (code in barcodeList_[[1]]) {
          searchTerm <- getNcbiSearchTerm(organism, code, searchOptionGene, searchOptionOrgn, seqLengthOption, seq_len_list[[code]])
          searchResult <- getNcbiSearchFullResults("nucleotide", searchTerm, downloadNumber)
          uids <- list.append(uids, searchResult[[1]])
          countResults <- list.append(countResults, searchResult[[2]])
          searchTerms <- list.append(searchTerms, searchTerm) 
        }
      }
      list(count = countResults, ids = uids, searchTermslist = searchTerms, organismList = organismList)
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
  
  # remove this 
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
      getRangeList_MarkerSequenceLength(barcodeList[[1]])
    }
  })

  
  # Takes a list of barcode markers and 
  # returns a list of numeric ranges
  # by marker to solicit the 
  # user for input on the min/max sequence
  # length for that marker.
  getRangeList_MarkerSequenceLength <- function(barcodeList){
    textList <- list()
    for (marker in barcodeList) {
      #add a numeric input
      textList <- list(
        textList, 
        numericRangeInput(
          inputId = marker,
          label = paste("Min/max sequence length for", marker),
          value = c(0, 2000)))
    }
    #return the list of numeric inputs
    textList
  }
  
  
  # * NCBICoverage -------------------------------------------------------------

  # setup a list of sequence lengths based on the selections in the ui
  getSeqLenList <- function(barcodeList) {
    seq_len_list <- list()
    for (code in barcodeList) {
      seq_len_list[[code]] <- input[[code]]
    }
  }
  
  # creates a list of barcodes if
  # 'code' is of the form (b1; b2; b3;...),
  # else it's just a single element list
  splitBarcode <- function(barcode) {
    code <- trimws(barcode)
    code <- gsub("[(, ,)]", "", code)
    str_split(code, ";")
  }
  
  # pings the database db with searchTerm and downloadNumber.
  # returns a two element list containing the uids in the first
  # position and the count in the second position.
  getNcbiSearchFullResults <- function(db, searchTerm, downloadNumber) {
    if (!NCBIKeyFlag){
      Sys.sleep(0.34) 
    }
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
  
  
  # genBankCoverage <- reactive({
  #   NCBIorganismList() %...>% {
  #     organismList <- .
  #     codeList <- barcodeList()
  #     validate(
  #       need(length(organismList) > 0, 'Please name at least one organism'),
  #       need(length(codeList) > 0, 'Please choose at least one barcode')
  #     )
  #     
  #     searchTerm <- ""
  #     countResults <- list()
  #     uids <- list()
  #     searchResult <- 0
  #     
  #     
  #     #Temp vars for search options
  #     NCBISearchOptionGene <- input$NCBISearchOptionGene
  #     NCBISearchOptionOrgn <- input$NCBISearchOptionOrgn
  #     downloadNumber <- input$downloadNum
  #     seqLengthOption <- input$seqLengthOption
  #     
  #     
  #     seq_len_list <- list()
  #     for (code in codeList) {
  #       seq_len_list[[code]] <- input[[code]]
  #     }
  #     
  #     future_promise({
  #       err <- 0              
  #       countResults <- list() 
  #       searchTerms <- list()   
  #       
  #       for (organism in organismList) {
  #         for (code in codeList) {
  #           # TODO: Add more sanitization to this
  #           
  #           # if there is a parenthesis
  #           code <- trimws(code)
  #           if (substring(code, 1, 1) == "(") {
  #             # code is in the format (loci1; loci2; loci3...)
  #             # We will make a combined query by substituting the ;s for 
  #             # other stuff
  #             
  #             # set up a replacement string
  #             replacement <- ""
  #             if (NCBISearchOptionGene) {
  #               replacement <- "[GENE]"
  #             }
  #             # Add organism info
  #             if (NCBISearchOptionOrgn) {
  #               #our query to GenBank
  #               replacement <-
  #                 paste(replacement, " AND ", organism, "[ORGN]", sep = "")
  #             }
  #             else {
  #               #our non-Metadata query to GenBank
  #               replacement <-
  #                 paste(replacement, " AND ", organism, sep = "")
  #             }
  #             # Add sequence length info
  #             if (seqLengthOption) {
  #               #if the user specified sequence length
  #               replacement <-
  #                 paste(
  #                   replacement,
  #                   " AND ",
  #                   seq_len_list[[code]][1],
  #                   ":",
  #                   seq_len_list[[code]][2],
  #                   "[SLEN]",
  #                   sep = ""
  #                 )
  #             }
  #             # Add the tail to the replacement string
  #             replacement <- paste(replacement, ") OR (", sep = "")
  #             print(replacement)
  #             # Now we finally set searchTerm by replacing the ;s.
  #             searchTerm <- gsub(";", replacement, code)
  #             # But the last synonym won't have a semicolon after it! Sub in 
  #             # one last time:
  #             
  #             # trim last parenthesis
  #             searchTerm <-
  #               substring(searchTerm, 1, nchar(searchTerm) - 1)
  #             # add in replacement string
  #             searchTerm <- paste(searchTerm, replacement, sep = "")
  #             # cut off the " OR ("
  #             searchTerm <- substring(searchTerm, 1, nchar(searchTerm) - 5)
  #             
  #           } else {
  #             print("Entering else block")
  #             if (NCBISearchOptionOrgn) {
  #               #our query to GenBank
  #               searchTerm <- paste(organism, "[ORGN] AND ", sep = "") 
  #             }
  #             else {
  #               #our non-Metadata query to GenBank
  #               searchTerm <- paste(organism, " AND ", sep = "")
  #             }
  #             print("past NCBISearchOptionOrgn")
  #             if (NCBISearchOptionGene) {
  #               #our query to GenBank
  #               searchTerm <- paste(searchTerm, code, "[GENE]", sep = "") 
  #             }
  #             else {
  #               #our query to GenBank
  #               searchTerm <- paste(searchTerm, code, sep = "") 
  #             }
  #             print("entering seqLenghtOption")
  #             
  #             #if the user specified sequence length
  #             if (seqLengthOption) {
  #               searchTerm <-
  #                 paste(
  #                   searchTerm,
  #                   " AND ",
  #                   seq_len_list[[code]][1],
  #                   ":",
  #                   seq_len_list[[code]][2],
  #                   "[SLEN]",
  #                   sep = ""
  #                 ) 
  #             }
  #             print("Past seqLengthOption")
  #           }
  #           searchResult <- tryCatch({
  #             if (!NCBIKeyFlag) {
  #               # sleeping for 1/3 of a second each time gives us 3 queries a 
  #               # second. If each user queries at this rate, we can service 4-8
  #               # at the same time.
  #               Sys.sleep(0.34) 
  #             }
  #             #only get back the number of search results
  #             searchResult <-
  #               entrez_search(db = "nucleotide",
  #                             term = searchTerm,
  #                             retmax = downloadNumber) 
  #           }, error = function(err) {
  #             countResults <<- list.append(countResults, "error")
  #             searchTerms <<- list.append(searchTerms, searchTerm)
  #             err <<- 1
  #           })
  #           
  #           if (err == 1) {
  #             err <- 0
  #             next
  #           } else {
  #             uids <- list.append(uids, searchResult$ids)
  #             searchTerms <- list.append(searchTerms, searchTerm) 
  #             #append the count to the vector of results
  #             countResults <- list.append(countResults, searchResult$count)
  #           }
  #         }
  #       }
  #       results <-
  #         list(count = countResults,
  #              ids = uids,
  #              searchTermslist = searchTerms)
  #       results
  #     })
  #   }
  # })
  
  
  # * NCBIMatrix ---------------------------------------------------------------
  
  matrixGet <-
    reactive({
      # creates and returns the matrix to be displayed with the count
      then(ncbiSearch(), function(value) {
        codeListLength <- length(barcodeList())
        
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
    })
  
  # * NCBITableOutput ----------------------------------------------------------
  
  matrixGetSearchTerms <-
    reactive({
      then(ncbiSearch(), function(value) {
        organismList <- value[[4]]
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
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
    })
  
  
  # *   NCBIGetIDs -------------------------------------------------------------
  
  uidsGet <-
    reactive({
      # Returns the uids stored in the results from the NCBi query
      then(ncbiSearch(), function(value) {
        # Get the results from the NCBI query
        uids <- c()
        for (i in value[[2]]) {
          uids <- c(uids, i)
        }
        uids
      })
    })
  
  
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
    barcodes <- barcodeList()
    promise_all(data_df = matrixGet(), rows = NCBIorganismList()) %...>% with({
      DT::datatable(data_df, rownames = rows, colnames = barcodes)
    })
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
      # Gets the column names for the matrix
      columns <- barcodeList() 
      NCBIorganismList() %...>% {
        #Gets the row names for the matrix
        rows <- . 
        matrixGet() %...>% {
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
  
  # * NCBIDownloadSearchTerms --------------------------------------------------
  
  #Download Search Terms:
  output$downloadStatements <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Search_Statements", ".csv", sep = "")
    },
    content = function(file) {
      # Gets the column names for the matrix
      columns <- barcodeList() 
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
      matrixGet() %...>% {
        NCBIdata <- .
        NCBIorganismList() %...>% {
          # Gets the column names for the matrix
          columns <- barcodeList() 
          # Adds the column names to the matrix
          colnames(NCBIdata) <- columns 
          # Adds the row names to the matrix
          rownames(NCBIdata) <- . 
          
          # Convert to Dataframe
          NCBIdata <- as.data.frame(NCBIdata) 
          dataframe <- NCBIdata
          summary_report_dataframe(dataframe)
        }
      }
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
  
  # * * DownloadDataframe ------------------------------------------------------
  
  summary_report_dataframe <- function(dataframe)
  {
    class(dataframe)
    class(dataframe[, 1])
    options(scipen = 999) #scientific notion
    new_row_names <- "total"
    # doesn't include column with taxa snames
    new_row_names <- c(new_row_names, colnames(dataframe))
    
    statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
    new_col_names <-
      c(
        "category",
        "number of sequences found",
        "percent of total sequences found",
        "num of organism with at least one sequence",
        "num of organisms with no sequences"
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
      statistics_df[i, 3] <- (barcodeSums[x] / Total_seq_found)
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
  
  
  # * * DownloadConvertCrux ----------------------------------------------
  
  convert_CRUX <-
    function(crux_output) 
      # Take a crux output matrix and  turn the characters "genus, spp, etc" 
      # into  0s/1s. This function is used by which_rows_are_empty_and_arenot()
    {
      crux_without_taxonomic_names <- crux_output
      crux_without_taxonomic_names <-
        na.omit(crux_without_taxonomic_names)
      
      non_number_values <-
        c('genus', 'family', 'class', 'order', 'error')
      
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
  
  
  
  # * * DownloadEmptyRows -----------------------------------------------------
  
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
            total <- total + as.numeric(dataframe[i, j])
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
          seqs <- 0
          seqs <- 0 + as.numeric(dataframe[i, Which_Column])
          
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
  
})
