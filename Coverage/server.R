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
#install.packages("bold")    # R package to pull sequences from BOLD
require("bold") 


shinyServer(function(input, output, session) {
    
  hideTab("BOLDpage", "Results")
  hideTab("BOLDpage", "Organism Names")
  
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
    

# * CRUXCoverage ------------------------------------------------------------

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
            searchTerm <- tax_name(query= organism, get = c("genus", "family", "order", "class","phylum", "domain"), db= "ncbi")
            for(table in dbList){
                # 
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=organism))
                if(nrow(location) == 0){
                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,3]))
                    if(nrow(location) == 0){
                        location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,4]))
                        if(nrow(location)==0){
                            location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,5]))
                            if(nrow(location) ==0){
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
            
        }
        dbDisconnect(taxaDB)
        # unlink("taxa-db.sqlite")
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE) #store vector results in data matrix
        data #return data matrix
    })
    

# * CRUXInputCSV -----------------------------------------------------------
    
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
      cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      
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

        countResults <- list() #initialize empty vector
        uids <- list()
        searchTerms <- list() #list of search terms
        for(organism in organismList){
            for(code in barcodeList()){
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

                searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 5) #only get back the number of search results
                uids <- list.append(uids, searchResult$ids)
                searchTerms <- list.append(searchTerms, searchTerm) # 
                countResults <- list.append(countResults, searchResult$count) #append the count to the vector of results
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
                    File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "genbank")  # Get the genbank file with that uid
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
    

# BOLD --------------------------------------------------------------------


# * BOLDSearchButton ------------------------------------------------------

    BOLDOrgSearch <- eventReactive(input$BOLDsearchButton, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList
      input$BOLDorganismList #Returns as a string
    })
    
    observeEvent(input$BOLDsearchButton, {
      updateTabsetPanel(session, "BOLDpage", selected = "Results")
      showTab("BOLDpage", "Results")
    })
    
    observeEvent(input$BOLDStartButton,
   {
     updateTabsetPanel(session, "BOLDpage", selected = "Organism Names")
     showTab("BOLDpage", "Organism Names")
     # It requires a file to be uploaded first
     req(input$uBOLDfile, file.exists(input$uBOLDfile$datapath))
     isolate({
       uploadinfo <- read.csv(input$uBOLDfile$datapath, header = TRUE)
       if(input$BOLDorganismList[[1]] != "") {
         updateTextAreaInput(getDefaultReactiveDomain(), "BOLDorganismList", value = c(head(uploadinfo$OrganismNames, -1), input$CRUXorganismList))
       }
       else {
         updateTextAreaInput(getDefaultReactiveDomain(), "BOLDorganismList", value = uploadinfo$OrganismNames)
       }
     })
   })


# * BOLDStrToList ---------------------------------------------------------

    boldOrganismList <- reactive({ #Converts string from cruxOrgSearch into a list of Strings
        print("HEY")
        organismList <- strsplit(BOLDOrgSearch(), ",")[[1]] #separate based on commas
        if(input$BOLDtaxizeOption){ #if the taxize option is selected
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
            print(taxize_organism_list)
            taxize_organism_list  
        } else{
            organismList #return the list as is
        }
    })
    

    
    
    # * BOLDCoverage ------------------------------------------------------------
    
    boldCoverage <- eventReactive(input$BOLDsearchButton, {
        print("in boldCoverage")
        print(input$removeNCBI)
        organismList <- boldOrganismList()
        organismListLength <- length(organismList)
       # print("HEY CONTINIOUING COVERAGE")
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        list <- c('processid', 'genbank_accession','lat', 'lon')
        # dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
        
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        temp <- c()

        for(organism in organismList){
           # test_bold <- records_bold <- bold_seqspec(taxon = organism)
           # print(test_bold)

            records_bold <- bold_seqspec(taxon = organism)[, c('species_name',   #this vector is the dataframe's column"
                                                                    'processid',             # BOLD identifier
                                                                    'genbank_accession', 
                                                                    'lat', 
                                                                    'lon')]

            #print(records_bold)
            #print(records_bold$genbank_accession)
            #print(input$removeNCBI)
            #print(length(records_bold$species_name))

            for(i in 1:length(records_bold$species_name)){
                temp <- c(records_bold$processid[i], records_bold$genbank_accession[i], records_bold$lat[i], records_bold$lon[i])
                results <- c(results, temp)
            }
        }
        
        data <- matrix(results, nrow = length(records_bold$species_name)-1, ncol = length(list), byrow = TRUE) #store vector results in data matrix
        data #return data matrix
    })
    
      output$BOLDcoverageResults <- 
            DT::renderDataTable(
            boldCoverage(), rownames = boldOrganismList(), colnames = c('processid', 'genbank_accession','lat', 'lon'))
         
        
    # why is remove_ncbi getting called after boldCoverage() when removeNCBI == FALSE?
    
    # * BOLDFASTADownload ------------------------------------------------------------
    
    output$downloadBoldFasta <- downloadHandler(
      filename = function() {
        paste("Test", ".fasta", sep="")
      },
      content = function(file) {
          Vector_Fasta <- c()
          organismList <- boldOrganismList()
          for(organism in organismList){
            res <- bold_seqspec(taxon = organism)
            print(res)
            for(i in 1:length(res$processid)){
              species_name <- if(res$subspecies_name[i] == "") res$species_name[i] else res$subspecies_name[i]
              org_vector = c(res$processid[i], species_name, res$markercode[i], res$genbank_accession[i])
              #remove all elements with no data from vector
              org_vector = org_vector[org_vector != '']
              #put data into string format
              org_data = paste(org_vector, collapse = '|')
              #write data to file
              Vector_Fasta <- c(Vector_Fasta, org_data)
              Vector_Fasta <- c(Vector_Fasta, res$nucleotides[i])
      
            }

          }
          #print(Vector_Fasta)

          write(Vector_Fasta, file)
      }
      
      
      
    )
    
    # * BOLD remove ncbi genomes ------------------------------------------------------------
   
     # option to remove NCBI genomes from BOLD output
    # somehow calling boldCoverage associates that output, need to remove that output as well

    remove_ncbi <- eventReactive(input$BOLDsearchButton,{
      records_bold = boldCoverage()
      print(input$removeNCBI)
      print("in remove_ncbi")
      list <- c('processid','lat', 'lon')
      results <- c()
      for(i in 1:nrow(records_bold)){
        if (records_bold[i,2] == ""){
          #print(records_bold[i,2])
          temp <- c(records_bold[i,1], records_bold[i,3], records_bold[i,4])
          results <- c(results, temp)
          #print(results)
        }
      }
      # how to get rid 
      #print("results are done")
      #print(results)
      data <- matrix(results, nrow = length(results)/3, ncol = length(list), byrow = TRUE) #store vector results in data matrix
      #print(data)
      data #return data matrix
  })
  
  output$removeNCBIResults <- 
      DT::renderDataTable(
      remove_ncbi(), rownames = boldOrganismList(), colnames = c('processid', 'lat', 'lon'))
    
})


