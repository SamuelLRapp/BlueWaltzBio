#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#;lkasdjf;aldf


# Imports ------------------------------------------------------------------

library(shiny)
library(rentrez)
library(taxize)
library(tidyverse)
library(dplyr)
library(RSQLite)
library(rlist)
library(mpoly)

shinyServer(function(input, output) {
    

# CRUX --------------------------------------------------------------------


# * CRUXSearchButton --------------------------------------------------------

    cruxOrgSearch <- eventReactive(input$searchButton, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList 
        input$CRUXorganismList #Returns as a string
    })

# * CRUXStrToList -----------------------------------------------------------
    
    cruxOrganismList <- reactive({ #Converts string from cruxOrgSearch into a list of Strings
        organismList <- strsplit(cruxOrgSearch(), ",")[[1]] #separate based on commas
        if(input$CRUXtaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #help user with various naming issues (spelling, synonyms, etc.)
                row_count <- nrow(NCBI_names) # get number of rows in dataframe
                
                if(row_count > 0) #If a legitimate name was found
                {
                    for(j in 1:row_count)
                    {
                        taxa_name <- NCBI_names[[j,3]] #Store each matched name in taxa_name
                        taxize_organism_list <- c(taxize_organism_list, taxa_name) #update the vector with all the taxa_names.
                    }
                }
                else
                {
                    taxize_organism_list <- c(taxize_organism_list, organism) #just append organism to the list, and return taxize_organism_list
                }
            }
            taxize_organism_list
        } else{
            organismList #return the list as is
        }
    })
    
# * CRUXSearch --------------------------------------------------------------------
    cruxSearch <- function(results, searchTerm, organism) {
      dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
      taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
      #results <- c()
      for(table in dbList){
        #
        location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=organism))
        if(nrow(location)==0){
          location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,3]))
          if(nrow(location)==0){
            location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,4]))
            if(nrow(location)==0){
              location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,5]))
              if(nrow(location)==0){
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,6]))
                if(nrow(location)==0){
                  location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,7]))
                  results <- c(results, nrow(location))
                } else { results <- c(results, "class")}
              } else {results <- c(results, "order")}
            } else {results <- c(results, "family")}
          }else {results <- c(results, "genus") }
        } else {results <- c(results, toString(nrow(location)))}
      }
      dbDisconnect(taxaDB)
      results
    }
    
# * CRUXCoverage ------------------------------------------------------------

    cruxCoverage <- reactive({
        organismList <- cruxOrganismList()
        organismListLength <- length(organismList)
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        
        dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
        #taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
        
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        newOrgList <- c()
        popuplist <- c()
        err <- 0
        
        for(organism in organismList){
            search <- get_uid_(sci_com = organism) # Check to see if there are homonyms
            if( nrow(search[[1]]) > 1) {# There are homonyms
              popuplist <- c(popuplist, organism)
              # Process the 
              for (i in 1:nrow(search[[1]])) { # tax_name
                # if there are more than 5 homonyms then break we are not interested in more than 5
                if(i > 5) { 
                  break
                }
                # create new organism list since new organism are added
                newOrg <- paste(organism, search[[1]]$division[i], sep = " ")
                newOrgList <- c(newOrgList, newOrg)
                # Creating the same format as the other organisms so the Crux search can be performed correctly
                hierarchy <- classification(search[[1]]$uid[i], db = "ncbi")[[1]]
                match <- hierarchy$name[match(tolower(c("genus", "family", "order", "class","phylum", "domain")), tolower(hierarchy$rank))]
                query <- c("db", "query", "genus", "family", "order", "class","phylum", "domain")
                match <- c("ncbi", organism, match)
                searchTerm <- stats::setNames(
                  data.frame(t(match), stringsAsFactors = FALSE), 
                  query
                )
                # Perform the CruxSearch
                results <- cruxSearch(results, searchTerm, organism)
              }
            } else { # There are no homonyms
              newOrgList <- c(newOrgList, organism)
              searchTerm <- tryCatch({
                searchTerm <- tax_name(query= organism, get = c("genus", "family", "order", "class","phylum", "domain"), db= "ncbi", messages = FALSE)
              }, error = function(err) {
                results <- c(results, "error", "error", "error", "error", "error", "error", "error")
                err <- 1
              })
              if(err == 1) {
                next
              }
              results <- cruxSearch(results, searchTerm, organism)
            }
        }
        
        results <- list(organismList=newOrgList, data=results, popupinfo=popuplist) 
        results
    })
    
# * matrixGetCRUX ------------------------------------------------------------
    
    matrixGetCRUX <- reactive({ # creates and returns the matrix to be displayed with the count
      dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S")
      cruxCoverage <- cruxCoverage() # Get the results from the NCBI query
      results <- c()
      organismListLength <- length(cruxCoverage[[1]])
      for (i in cruxCoverage[[2]]) {
        results <- c(results, i)
      }
      data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE) #convert results vector to dataframe
      data
    })
    
# * organismListGet ------------------------------------------------------------
    
    organismListGet <- reactive({ # Returns the uids stored in the results from the NCBi query
      organismList <- c()
      cruxCoverage <- cruxCoverage() # Get the results from the NCBI query
      for (i in cruxCoverage[[1]]) {
        organismList <- c(organismList, i)
      }
      # Send the alert to the user that we have found some homonyms
      if(length(organismList) > length(cruxOrganismList())) {
        shinyalert("We have found Homonyms", cruxCoverage[[3]], type = "warning")
      }
      organismList
    })
    

# * CRUXInputCSV -----------------------------------------------------------
    
    inputFileCrux <- observeEvent(input$uploadCRUXButton,{
        isolate({
            req(input$uCRUXfile, file.exists(input$uCRUXfile$datapath))
            uploadinfo <- read.csv(input$uCRUXfile$datapath, header = TRUE)
            if(input$CRUXorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = c(head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]), input$CRUXorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
            }
        })
    })
    

# * CRUXDownload ------------------------------------------------------------
    
    output$downloadCrux <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste(input$CRUXorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") # Gets the column names for the matrix
            CRUXmatrix <- cruxCoverage() # Gets the matrix for the Crux results
            colnames(CRUXmatrix) <- columns # Adds the column names to the matrix
            rownames(CRUXmatrix) <- cruxOrganismList() # Adds the row names to the matrix
            write.csv(CRUXmatrix, file) # Writes the matrix to the CSV file
        }
    )
    

# * CRUXOutput --------------------------------------------------------------

    output$CRUXcoverageResults <- DT::renderDataTable(
      matrixGetCRUX(), rownames = organismListGet(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      
    )
    

# NCBI --------------------------------------------------------------------


# * NCBISearchButton --------------------------------------------------------
    
    NCBISearch <- eventReactive(input$NCBIsearchButton, { #When searchButton clicked, update NCBIOrgSearch to return the value input into NCBIorganismList 
        list(input$NCBIorganismList, input$barcodeList) #Returns as a string
    })
    

# * NCBIStrToList -----------------------------------------------------------

    NCBIorganismList <- reactive({ #Converts string from NCBIorganismList into a list of Strings
        organismList <- strsplit(NCBISearch()[[1]], ",")[[1]] #separate based on commas
        if(input$NCBItaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
                row_count <- nrow(NCBI_names)# get number of rows in dataframe
                
                if(row_count > 0)#If a legitimate name was found
                {
                    for(j in 1:row_count)
                    {
                        taxa_name <- NCBI_names[[j,3]] #Store each matched name in taxa_name
                        taxize_organism_list <- c(taxize_organism_list, taxa_name) #update the vector with all the taxa_names.
                    }
                }
                else
                {
                    taxize_organism_list <- c(taxize_organism_list, organism) #just append organism to the list, and return taxize_organism_list
                }
            }
            taxize_organism_list  
        } else{
            organismList #return the list as is
        }
    })
    

# * NCBIBarcodeList ---------------------------------------------------------

    barcodeList <- reactive({
        # Detect that there is a parenthesis then do not change and keep it together
        barcodeList <- strsplit(NCBISearch()[[2]], ",") #separate based on comma
        barcodeList[[1]]
    })
    

# * NCBISequenceLength ------------------------------------------------------

    seqLenList <- reactive({ #list of sequence length specifications
        if(input$seqLengthOption){ #only present if the option is selected
            textList <- list()
            for(marker in barcodeList()){ #allow the user to specify a different length for every barcode
                textList <- list(textList, numericInput(marker, paste("Minimum sequence length for", marker), 500, min= 0)) #add a numeric input
            }
            textList #return the list of numeric inputs
        }
    })
    

# * NCBICoverage ------------------------------------------------------------
    
    genBankCoverage <- reactive({
        
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        
        codeListLength <- length(barcodeList()) 
        validate( #verify that the  user has typed things into both inputs
            need(organismListLength > 0, 'Please name at least one organism'),
            need(codeListLength > 0, 'Please choose at least one barcode')
        )
        searchTerm <- ""
        searchResult <- 0
        err <- 0
        countResults <- list() #initialize empty vector
        uids <- list()
        searchTerms <- list() #list of search terms
        for(organism in organismList){
            for(code in barcodeList()){
                # TODO: Add more sanitization to this
                # if there is a parenthesis
                code <- trimws(code)
                if(substring(code, 1,1) == "("){
                    # code is in the format (loci1; loci2; loci3...)
                    # We will make a combined query by substituting the ;s for other stuff
                  
                    # set up a replacement string
                    replacement <- ""
                    if(input$NCBISearchOptionGene){
                      replacement <- "[GENE]"
                    }
                    # Add organism info 
                    if(input$NCBISearchOptionOrgn){
                      #our query to GenBank
                      replacement <- paste(replacement, " AND ", organism, "[ORGN]", sep="") 
                    }
                    else {
                      #our non-Metadata query to GenBank
                      replacement <- paste(replacement, " AND ", organism, sep="") 
                    }
                    # Add sequence length info
                    if(input$seqLengthOption){
                      #if the user specified sequence length
                      replacement <- paste(replacement, " AND ", input[[code]],":99999999[SLEN]", sep="")
                    }
                    # Add the tail to the replacement string
                    replacement <- paste(replacement, ") OR (", sep="")
                    print(replacement)
                    # Now we finally set searchTerm by replacing the ;s.
                    searchTerm <- gsub(";", replacement, code)
                    # But the last synonym won't have a ; after it! Sub in one last time:
                    # trim last parenthesis
                    searchTerm <- substring(searchTerm, 1, nchar(searchTerm)-1)
                    # add in replacement string
                    searchTerm <- paste(searchTerm, replacement, sep="")
                    # cut off the " OR ("
                    searchTerm <- substring(searchTerm, 1, nchar(searchTerm)-5)
                    
                }else {
                  if(input$NCBISearchOptionOrgn){
                    searchTerm <- paste(organism, "[ORGN] AND ", sep="") #our query to GenBank
                  }
                  else {
                    searchTerm <- paste(organism, " AND ", sep="") #our non-Metadata query to GenBank
                  }
                  if(input$NCBISearchOptionGene) {
                    searchTerm <- paste(searchTerm, code, "[GENE]", sep="") #our query to GenBank
                  }
                  else {
                    searchTerm <- paste(searchTerm, code, sep="") #our query to GenBank
                  }
                  if(input$seqLengthOption){
                    searchTerm <- paste(searchTerm, " AND ", input[[code]],":99999999[SLEN]", sep="") #if the user specified sequence length
                  }
                }
                searchResult <- tryCatch({
                  searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 5) #only get back the number of search results
                }, error = function(err) {
                  results <- c(results, "error", "error", "error", "error", "error", "error", "error")
                  err <- 1
                })
                if(err == 1) {
                  err <- 0
                  next
                } else {
                  uids <- list.append(uids, searchResult$ids)
                  searchTerms <- list.append(searchTerms, searchTerm) # 
                  countResults <- list.append(countResults, searchResult$count) #append the count to the vector of results
                }
            }
        }
        results <- list(count=countResults, ids=uids,searchTermslist = searchTerms ) #
        results
    })
    

# * NCBIMatrix --------------------------------------------------------------
    
    matrixGet <- reactive({ # creates and returns the matrix to be displayed with the count
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        results <- genBankCoverage() # Get the results from the NCBI query
        count <- c()
        for (i in results[[1]]) {
            count <- c(count, i)
        }
        data <- matrix(count, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
        data
    })

# * NCBITableOutput ---------------------------------------------------------
    
    matrixGetSearchTerms <- reactive({ # creates and returns the matrix to be displayed with the count
      organismList <- NCBIorganismList() #get species and barcode inputs
      organismListLength <- length(organismList)
      codeListLength <- length(barcodeList())
      results <- genBankCoverage() # Get the results from the NCBI query
      SearchStatements <- c()
      for (i in results[[3]]) { #3 is the 3rd list in genBankCovearage aka the searchterms list
        SearchStatements <- c(SearchStatements, i)
      }
      data <- matrix(SearchStatements, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
      data
    })
    

# *   NCBIGetIDs --------------------------------------------------------------

    uidsGet <- reactive({ # Returns the uids stored in the results from the NCBi query
        uids <- c()
        results <- genBankCoverage() # Get the results from the NCBI query
        for (i in results[[2]]) {
            uids <- c(uids, i)
        }
        uids
    })
    

# * NCBIDownloadFASTA -------------------------------------------------------
  
    # Download NCBI table
    output$fileDownloadF <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("TEST", ".fasta", sep = "")
        },
        content = function(file) {
            uids <- uidsGet()
            progLength <- length(uids)
            shiny::withProgress(message="Downloading", value=0, {
                Vector_Fasta <- c()
                for (uid in uids) {
                    File_fasta <- entrez_fetch(db = "nucleotide", id = uid, rettype = "fasta") # Get the fasta file with that uid
                    Vector_Fasta <- c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_Fasta, file) # Writes the vector containing all the fasta file information into one fasta file
                shiny::incProgress(1/progLength)
            })
            
        }
    )
    

# * NCBIDownloadGenbank -----------------------------------------------------

    # Download NCBI Genbank
    output$fileDownloadG <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("GenbankTEST", ".gb", sep = "")
        },
        content = function(file) {
            uids <- uidsGet()
            progLength <- length(uids)
            shiny::withProgress(message="Downloading", value=0,{
                Vector_genbank <- c()
                for (uid in uids) {
                    File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "gb")  # Get the genbank file with that uid
                    Vector_genbank <- c(Vector_genbank, File_genbank) # Append the genbank file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_genbank, file, append=TRUE) # Writes the vector containing all the genbank file information into one genbank file
                shiny::incProgress(1/progLength)
            })
            
        }
    )
    

# * NCBIBarcodeButtons -----------------------------------------------------

    observeEvent(input$barcodeOptionCO1,{ # Detects when the specific barcode (in this case CO1) button has been pressed
        if(input$barcodeList[[1]] != "") { # If the input barcodeList is not empty (ie. the inputtextarea is not empty) then use the paste function to the add the barcode/s to the beginning
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("(CO1; COI; COX1),", input$barcodeList)) # Updates the text area input adds the barcode/s to the beginning of whatever is already in it
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "(CO1; COI; COX1)") # Here since the textarea is empty we just set its value to the barcode/s
        }
    })
    
    observeEvent(input$barcodeOption16S,{ 
        if(input$barcodeList[[1]] != "") { 
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("16S,", input$barcodeList)) 
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "16S")
        }
    })
    
    observeEvent(input$barcodeOptionITS2,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("ITS2,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "ITS2")
        }
    })
    
    observeEvent(input$barcodeOption18S,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("18S,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "18S")
        }
    })
    
    observeEvent(input$barcodeOptionITS1,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("ITS1,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "ITS1")
        }
    })
    
    observeEvent(input$barcodeOptiontrnl,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("trnl,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "trnl")
        }
    })
    
    observeEvent(input$barcodeOption12S,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("12S,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "12S")
        }
    })
    

# * NCBIOutputTables ------------------------------------------------------------

    
    #outputs:
    output$seqLenInputs <- renderUI(seqLenList())
    
    output$NCBIcoverageResults <- DT::renderDataTable(
        matrixGet(), rownames = NCBIorganismList(), colnames = barcodeList()
    )
  

# * NCBInputFile ------------------------------------------------------------

  
    inputFileNCBI <- observeEvent(input$uploadNCBIButton,{
        isolate({
            req(input$uNCBIfile, file.exists(input$uNCBIfile$datapath))
            uploadinfo <- read.csv(input$uNCBIfile$datapath, header = TRUE)
            if(input$NCBIorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]), input$NCBIorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
            }
            if(input$barcodeList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(head(uploadinfo$Barcodes[uploadinfo$Barcodes != ""]), input$barcodeList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = uploadinfo$Barcodes[uploadinfo$Barcodes != ""])
            }
        })
    })
    

# * NCBIDownloadTable -------------------------------------------------------
    
    # Download NCBI table
    output$download <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste(input$NCBIorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- barcodeList() # Gets the column names for the matrix
            NCBImatrix <- matrixGet() # Gets the matrix for the NCBI results
            colnames(NCBImatrix) <- columns # Adds the column names to the matrix

            rownames(NCBImatrix) <- NCBIorganismList() # Adds the row names to the matrix
            write.csv(NCBImatrix, file) # Writes the dataframe to the CSV file
        }
        
        
    )

# * NCBIDownloadSearchTerms -------------------------------------------------

    #Download Search Terms:
    output$downloadStatements <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste(input$NCBIorganismList, ".csv", sep = "")
      },
      content = function(file) {
        columns <- barcodeList() # Gets the column names for the matrix
        NCBImatrix <- matrixGetSearchTerms() # Gets the matrix for the NCBI results
        colnames(NCBImatrix) <- columns # Adds the column names to the matrix
        
        rownames(NCBImatrix) <- NCBIorganismList() # Adds the row names to the matrix
        write.csv(NCBImatrix, file) # Writes the dataframe to the CSV file
      }
    )
    
})
