#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#;lkasdjf;aldf

library(shiny)
library(rentrez)
library(taxize)
library(tidyverse)
library(dplyr)
library(RSQLite)
library(rlist)
library(future)
library(promises)


plan(multicore)
shinyServer(function(input, output) {
    
    #CRUX:
    cruxOrgSearch <- eventReactive(input$searchButton, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList 
        input$CRUXorganismList #Returns as a string
    })
    
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
    
    cruxCoverage <- reactive({
        organismList <- cruxOrganismList()
        organismListLength <- length(organismList)
        
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        
        dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
        
        taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
        
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList){
            for(table in dbList){
                # 
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=organism))
                if(nrow(location) == 0){
                    searchTerm <- tax_name(query= organism, get= "genus", db= "ncbi")[1,3]
                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm))
                    if(nrow(location) == 0){
                        searchTerm <- tax_name(query= organism, get= "family", db= "ncbi")[1,3]
                        location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm))
                        if(nrow(location)==0){
                            searchTerm <- tax_name(query= organism, get= "order", db= "ncbi")[1,3]
                            location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm))
                            if(nrow(location) ==0){
                                searchTerm <- tax_name(query= organism, get= "class", db= "ncbi")[1,3]
                                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm))
                                if(nrow(location)==0){
                                    searchTerm <- tax_name(query= organism, get= "phylum", db= "ncbi")[1,3]
                                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm))
                                    results <- c(results, nrow(location))
                                } else { results <- c(results, "class")}
                            } else {results <- c(results, "order")}
                        } else {results <- c(results, "family")}
                    }else {results <- c(results, "genus") }
                } else {results <- c(results, toString(nrow(location)))}
            }
            
        }
        dbDisconnect(taxaDB)
        # unlink("taxa-db.sqlite")
        
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE) #store vector results in data matrix
        data #return data matrix
    })
    
    inputFileCrux <- observeEvent(input$uploadCRUXButton,{
        isolate({
            req(input$uCRUXfile, file.exists(input$uCRUXfile$datapath))
            uploadinfo <- read.csv(input$uCRUXfile$datapath, header = TRUE)
            if(input$CRUXorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = c(head(uploadinfo$OrganismNames, -1), input$CRUXorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = uploadinfo$OrganismNames)
            }
        })
    })
    
    # Download options
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
    
    #NCBI: 
    
    NCBISearch <- eventReactive(input$NCBIsearchButton, { #When searchButton clicked, update NCBIOrgSearch to return the value input into NCBIorganismList 
        list(input$NCBIorganismList, input$barcodeList) #Returns as a string
    })
    
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
    
    barcodeList <- reactive({
        barcodeList <- strsplit(NCBISearch()[[2]], ",") #separate based on comma
        barcodeList[[1]]
    })
    
    seqLenList <- reactive({ #list of sequence length specifications
        if(input$seqLengthOption){ #only present if the option is selected
            textList <- list()
            for(marker in barcodeList()){ #allow the user to specify a different length for every barcode
                textList <- list(textList, numericInput(marker, paste("Minimum sequence length for", marker), 500, min= 0)) #add a numeric input
            }
            textList #return the list of numeric inputs
        }
    })
    
    genBankCoverage <- reactive({
        
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        
        codeList <- barcodeList()
        codeListLength <- length(barcodeList()) 
        validate( #verify that the  user has typed things into both inputs
            need(organismListLength > 0, 'Please name at least one organism'),
            need(codeListLength > 0, 'Please choose at least one barcode')
        )
        searchTerm <- ""
        searchResult <- 0

        countResults <- list() #initialize empty vector
        uids <- list()
        searchTerms <- list() #list of search terms
        
        searchOptionOrgn <- input$NCBISearchOptionOrgn
        searchOptionGene <- input$NCBISearchOptionGene
        seqLengthOption <- input$seqLengthOption
        
        future_promise({
        for(organism in organismList){
            for(code in codeList){
                if(searchOptionOrgn){
                    searchTerm <- paste(organism, "[ORGN] AND ", sep="") #our query to GenBank
                }
                else {
                    searchTerm <- paste(organism, " AND ", sep="") #our non-Metadata query to GenBank
                }
                if(searchOptionGene) {
                    searchTerm <- paste(searchTerm, code, "[GENE]", sep="") #our query to GenBank
                }
                else {
                    searchTerm <- paste(searchTerm, code, sep="") #our query to GenBank
                }
                if(seqLengthOption){
                    searchTerm <- paste(searchTerm, " AND ", input[[code]],":99999999[SLEN]", sep="") #if the user specified sequence length
                }

                searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 5) #only get back the number of search results
                uids <- list.append(uids, searchResult$ids)
                searchTerms <- list.append(searchTerms, searchTerm) # 
                countResults <- list.append(countResults, searchResult$count) #append the count to the vector of results
            }
        }
        results <- list(count=countResults, ids=uids,searchTermslist = searchTerms ) #
        results
        })
    })
    
    
    matrixGet <- reactive({ # creates and returns the matrix to be displayed with the count
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        genBankCoverage() %...>% { # Get the results from the NCBI query
        count <- c()
        for (i in .[[1]]) {
            count <- c(count, i)
        }
        data <- matrix(count, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
        data
        }
    })
    
    matrixGetSearchTerms <- reactive({ # creates and returns the matrix to be displayed with the count
      organismList <- NCBIorganismList() #get species and barcode inputs
      organismListLength <- length(organismList)
      codeListLength <- length(barcodeList())
      genBankCoverage() %...>% { # Get the results from the NCBI query
      SearchStatements <- c()
      for (i in .[[3]]) { #3 is the 3rd list in genBankCovearage aka the searchterms list
        SearchStatements <- c(SearchStatements, i)
      }
      data <- matrix(SearchStatements, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
      data
      }
    })
    
    
    uidsGet <- reactive({ # Returns the uids stored in the results from the NCBi query
        genBankCoverage() %...>% { # Get the results from the NCBI query
        uids <- c()
        for (i in .[[2]]) {
            uids <- c(uids, i)
        }
        uids
        }
    })
    
    # Download NCBI table
    output$fileDownloadF <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("TEST", ".fasta", sep = "")
        },
        content = function(file) {
            uidsGet() %...>% {
            progLength <- length(.)
            shiny::withProgress(message="Downloading", value=0, {
                Vector_Fasta <- c()
                for (uid in .) {
                    File_fasta <- entrez_fetch(db = "nucleotide", id = uid, rettype = "fasta") # Get the fasta file with that uid
                    Vector_Fasta <- c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_Fasta, file) # Writes the vector containing all the fasta file information into one fasta file
                shiny::incProgress(1/progLength)
            })
            }
        }
    )
    
    # Download NCBI table
    output$fileDownloadG <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("GenbankTEST", ".gb", sep = "")
        },
        content = function(file) {
            uidsGet() %...>% {
            progLength <- length(.)
            shiny::withProgress(message="Downloading", value=0,{
                Vector_genbank <- c()
                for (uid in .) {
                    File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "genbank")  # Get the genbank file with that uid
                    Vector_genbank <- c(Vector_genbank, File_genbank) # Append the genbank file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_genbank, file, append=TRUE) # Writes the vector containing all the genbank file information into one genbank file
                shiny::incProgress(1/progLength)
            })
            }
        }
    )
    
    observeEvent(input$barcodeOptionCO1,{ # Detects when the specific barcode (in this case CO1) button has been pressed
        if(input$barcodeList[[1]] != "") { # If the input barcodeList is not empty (ie. the inputtextarea is not empty) then use the paste function to the add the barcode/s to the beginning
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("CO1, COI, COX1,", input$barcodeList)) # Updates the text area input adds the barcode/s to the beginning of whatever is already in it
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "CO1, COI, COX1") # Here since the textarea is empty we just set its value to the barcode/s
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
    
    #outputs:
    output$seqLenInputs <- renderUI(seqLenList())
    
    output$NCBIcoverageResults <- DT::renderDataTable(
        matrixGet(), rownames = NCBIorganismList(), colnames = barcodeList()
    )
    
    output$CRUXcoverageResults <- DT::renderDataTable(
        cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
        
    )
    
    inputFileNCBI <- observeEvent(input$uploadNCBIButton,{
        isolate({
            req(input$uNCBIfile, file.exists(input$uNCBIfile$datapath))
            uploadinfo <- read.csv(input$uNCBIfile$datapath, header = TRUE)
            if(input$NCBIorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(head(uploadinfo$OrganismNames, -1), input$NCBIorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = uploadinfo$OrganismNames)
            }
            if(input$barcodeList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(head(uploadinfo$Barcodes, -1), input$barcodeList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = uploadinfo$Barcodes)
            }
        })
    })
    
    # Download NCBI table
    output$download <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste(input$NCBIorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- barcodeList() # Gets the column names for the matrix
            matrixGet() %...>% { # Gets the matrix for the NCBI results
            colnames(.) <- columns # Adds the column names to the matrix

            rownames(.) <- NCBIorganismList() # Adds the row names to the matrix
            write.csv(., file) # Writes the dataframe to the CSV file
            }
        }
        
        
    )
    
    #Download Search Terms:
    output$downloadStatements <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste(input$NCBIorganismList, ".csv", sep = "")
      },
      content = function(file) {
        columns <- barcodeList() # Gets the column names for the matrix
        matrixGetSearchTerms() %...>% { # Gets the matrix for the NCBI results
        colnames(.) <- columns # Adds the column names to the matrix
        
        rownames(.) <- NCBIorganismList() # Adds the row names to the matrix
        write.csv(., file) # Writes the dataframe to the CSV file
        }
      }
    )
    
})
