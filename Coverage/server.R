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

require("treemap")
library(plotly)
library(treemapify)
library(ggplot2)

shinyServer(function(input, output, session) {
    
  hideTab("BOLDpage", "Results")
  hideTab("BOLDpage", "Organism Names")
  hideTab("BOLDpage", "Plot Unique Species Per Country")
  hideTab("BOLDpage", "Plot Total Sequences Per Country")
  hideTab("BOLDpage", "Filter By Country")
  hideTab("BOLDpage", "Country Data")
  
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
     req(input$uBOLDfile, file.exists(input$uBOLDfile$datapath))
     isolate({
       uploadinfo <- read.csv(input$uBOLDfile$datapath, quote = "", header = TRUE)
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
        taxize_organism_list  
      } else{
        organismList #return the list as is
      }
    })
    
    # * BOLDCoverage ------------------------------------------------------------

    
    boldCoverage <- reactive ({
      print("WTF BROOOO")
      organismList <- boldOrganismList()
      organismListLength <- length(organismList)
      countries <- c()
      validate(
        need(organismListLength > 0, 'Please name at least one organism')
      )

      #puts variable in global scope
      unfound_species <<- c()
      
      results <- data.frame(matrix(ncol=0, nrow=0))
      for(organism in organismList){
        searchResult <- tryCatch({
          records_bold <- bold_seqspec(taxon = organism)
        }, error = function(err) {
          print("ERROR")
          # POP UP TELLING USER THAT BOLD IS DOWN
        })
        if (!is.na(records_bold)){
          for (i in 1:nrow(records_bold)) {
            if (is.na(records_bold$country[i]) || records_bold$country[i] == "") {
              records_bold$country[i] = "No Country Listed"
            }
          }
          if (!is.na(records_bold$species_name)) {
            countries <- c(countries, records_bold$country)
            results <- rbind(results, records_bold)
          } else {
            print("unfound:")
            print(organism)
            print(records_bold)
            unfound_species <<- c(unfound_species, organism)
          }
        }
      }
      shinyjs::show(id = "BOLDClearFilter")
      shinyjs::show(id = "BOLDfilterCountries")
      shinyjs::show(id = "BOLDSkipFilter")
      shinyjs::show(id = "BOLDNullSpecies")
      shinyjs::show(id = "BOLDNullSpeciesWarning")
      results <- list(results=results, countries=countries)
      results #return data matrix
    })
    
    BOLDOrgCountries <- eventReactive(input$BOLDfilterCountries, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList
      input$selectCountry #Returns a list of countries
    })
    
    # * BOLD MATRIX----------------
    
    BoldMatrix <- reactive({# creates and returns the matrix to be displayed with the count
      list <- boldCoverage()
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
    })
    
    # * Plot: Unique Species per Country ----------
    # country summary function
    # no. of species for all countries
    
    BoldPlotBarGraph <- function(){
      if (!is.null(BOLDOrgCountries())){
        vals <- c()
        countries_values <- list()
        require(data.table)
        records_bold <- BoldMatrix()
        countries <- c(unique(records_bold$country)) 
        for (i in 1:length(countries)){
          if (countries[i] == ""){
            countries[i] = "no country listed"
          }
          countries_values[[countries[i]]] = 0
        }
        presentMatrix <- presentMatrix()
        for (i in 1:ncol(presentMatrix)) {
          countries <- colnames(presentMatrix)
          for (j in 1:nrow(presentMatrix)) {
            if (presentMatrix[j,i] > 0) {
              countries_values[[countries[i]]] = countries_values[[countries[i]]] + 1
            }
          }
        }}
      
      # set vals
      ## don't need for loop, unlist() will extract values
      vals <- unlist(countries_values)
      print(countries_values)
      
      ## BOLD adds species/subspecies to search results
      ## so # of species will often be more than # from boldOrganismList()
      max_uniq_species <- max(vals)
      print(max_uniq_species)
  
      # the "countries" list from above seems to exclude countries with 0 species
      # this list won't
      filter_countries <- names(countries_values)

      xf <- data.frame(country = filter_countries, values = vals)
      ggplot(data=xf, aes(x = country, y = values)) +
        geom_bar(stat="identity", fill="purple") +
        labs(y = "# unique species", x = "countries") +
        scale_y_continuous(limits = c(0, max_uniq_species)) +
        theme(text = element_text(size = 17), axis.ticks.length = unit(0.25, "cm")) +
        coord_flip()
      #barplot(vals, names.arg = countries, xlab = "countries", ylab = "# unique species", col = "purple")
    }
    
    
    output$species <- renderPlot ({
      BoldPlotBarGraph()
    }, height = 700, width = 1000)
    
    output$downloadBarGraph = downloadHandler(
      filename = 'test.png',
      content = function(file) {
        device <- function(..., width, height) {
          grDevices::png(..., width = width, height = height,
                         res = 300, units = "in")
        }
        BoldPlotBarGraph()
        ggsave(file, device = device)
      })
    
    
    # * Plot: Total sequences per Country ----------
    # for the treemap
    
    BoldPlotTreemap <- function(){
      if (!is.null(input$selectCountry)){
        records_bold <- BoldMatrix()
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
          geom_treemap_text(fontface = "bold", colour = "white", place = "centre", grow = TRUE, reflow = TRUE) +
          theme(axis.text = element_text(size = theme.size))
        # how to change the colors + get a legend ?

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
        BoldPlotTreemap()
        ggsave(file, device = device)
      })
    
    
    output$BOLDcoverageResults <- 
      DT::renderDataTable(
        reduce_barcode_summary(barcode_summary()))

    output$BOLDPresentTable <- 
      DT::renderDataTable(
        presentMatrix())
    
    output$BOLDAbsentTable <- 
      DT::renderDataTable(
        absentMatrix())
    
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
      selectizeInput(inputId="selectCountry", label="Filter by Countries", choices=boldCoverage()$countries, selected = NULL, multiple = TRUE,options = NULL, width = 500)
    })

    # * BOLDFASTADownload ------------------------------------------------------------
    
    output$downloadBoldFasta <- downloadHandler(
      filename = function() {
        paste("BOLD", ".fasta", sep="")
      },
      content = function(file) {
        fasta_vector = c()
        records_bold <- BoldMatrix()
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

          #isequal <- !is.na(records_bold$nucleotides[i]) == (records_bold$nucleotides[i] != '')
          #print(isequal)
          #if(!isequal) {
          #  print(records_bold$nucleotides[i])    
          #}
          if (records_bold$nucleotides[i] != '' && !is.na(records_bold$nucleotides[i])){
            fasta_vector <- c(fasta_vector, org_data)
            fasta_vector <- c(fasta_vector, records_bold$nucleotides[i])
          }
        }

        write(fasta_vector, file)
      }
    )
    
    # * BOLDSummaryDownload ------------------------------------------------------------
    
    output$downloadBoldSummary <- downloadHandler(
      filename = function() {
        paste("BOLD_Summary_Report", ".csv", sep="")
      },
      content = function(file) {
        write.csv(summary_report(2), file)
      }
    )
    
    # * BOLDCountryFilter -------------------------------------------
    
    country_summary <- function(){
      summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
      records_bold <- boldCoverage()[["results"]]
      if (length(records_bold$species_name) == 0){
        return(summary_df)
      }
      for(i in 1:length(records_bold$species_name)){
        if (!is.na(records_bold$species_name[i]) && (records_bold$species_name[i] != "") && !(records_bold$species_name[i] %in% rownames(summary_df)))
          #add a row to summary_df
          summary_df[records_bold$species_name[i],] <- integer(ncol(summary_df))
        #add data to summary_df to get summary data
        if (!is.na(records_bold$country[i]) && records_bold$country[i] != '' && (records_bold$species_name[i] != "")){
          #if country is not yet in the dataframe, initiate new col
          if (!(records_bold$country[i] %in% colnames(summary_df))){
            #create a new column of 0s
            summary_df[records_bold$country[i]] <- integer(nrow(summary_df))
          }
          #add 1 to existing count
          summary_df[records_bold$species_name[i], records_bold$country[i]] <- summary_df[records_bold$species_name[i], records_bold$country[i]] + 1
        }
      }
      # print("COUNTRY SUMMARY")
      # print(summary_df)
      summary_df
      
      
    }
    
    presentMatrix <- function(){
      present_df <- country_summary()
      
      print(present_df)
      print("I GOT HERE YEEEEE")
      #remove all columns that are not in filter
      present_df <- present_df[ , which(names(present_df) %in% BOLDOrgCountries())]
      
      #remove all rows that have all 0s as values and return
      #present_df <- present_df[apply(present_df[, -1], 1, function(x) !all(x==0)),]
      present_df <- present_df[rowSums(present_df[])>0,]
      
      # print("PRESENT MATRIX")
      # print(present_df)
      present_df
      
    }
    
    absentMatrix <- function(){
      all_names <- rownames(country_summary())
      #get rownames of presentMatrix  
      present_names <- rownames(presentMatrix())
      #get complement of names
      absent_names <- all_names[is.na(pmatch(all_names, present_names))]
      
      country_names <- boldCoverage()$countries[]
      country_names <- country_names[!duplicated(country_names)]
      country_names <- na.omit(country_names[country_names != ""])
      summary_df <- country_summary()
      absent_df <- data.frame(matrix(ncol = 3, nrow = 0))
      colnames(absent_df) <- c("1st", "2nd", "3rd")
      if(length(absent_names) == 0){
        return(absent_df)
      }
      for(i in 1:length(absent_names)){
        sig_countries <- c("NA" = 0,"NA" = 0,"NA" = 0)
        for (j in 1:length(country_names)){
          
          val <- summary_df[absent_names[i], country_names[j]]
          #if value is bigger than the most significant
          if(val > sig_countries[[1]]){
            #move col 2 to col 3
            names(sig_countries)[3] <- names(sig_countries)[2]
            sig_countries[[3]] <- sig_countries[[2]]
            #move col 1 to col 2
            names(sig_countries)[2] <- names(sig_countries)[1]
            sig_countries[[2]] <- sig_countries[[1]]
            #replace col 1 with new val
            names(sig_countries)[1] <- country_names[j]
            sig_countries[[1]] <- val
            
          } else if (val > sig_countries[[2]]){
            #move col 2 to col 3
            names(sig_countries)[3] <- names(sig_countries)[2]
            sig_countries[[3]] <- sig_countries[[2]]
            #replace col 2 with new val
            names(sig_countries)[2] <- country_names[j]
            sig_countries[[2]] <- val
          } else if (val > sig_countries[[3]]){
            #replace col3 with new val
            names(sig_countries)[3] <- country_names[j]
            sig_countries[[3]] <- val
          }
        }
        absent_df[absent_names[i],] <- names(sig_countries)
      }
      #print("ABSENT MATRIX")
      #print(absent_df)
      absent_df
      
    }  
    
    
  
  # * SummaryReport ------------------------------------------------------------
  
  barcode_summary <- function() {
    summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
    records_bold <- BoldMatrix()
    if (length(records_bold$species_name) == 0){
      return(summary_df)  
    }
    for(i in 1:length(records_bold$species_name)){
      #if species name not in dataframe
      if (!is.na(records_bold$species_name[i]) && (records_bold$species_name[i] != "") && !(records_bold$species_name[i] %in% rownames(summary_df)))
        #add a row to summary_df
        summary_df[records_bold$species_name[i],] <- integer(ncol(summary_df))
      #add data to summary_df to get summary data
      if (!is.na(records_bold$markercode[i]) && records_bold$markercode[i] != '' && (records_bold$species_name[i] != "")){
        #if markercode is not yet in the dataframe, initiate new col
        if (!(records_bold$markercode[i] %in% colnames(summary_df))){
          #create a new column of 0s
          summary_df[records_bold$markercode[i]] <- integer(nrow(summary_df))
        }
        #add 1 to existing count
        summary_df[records_bold$species_name[i], records_bold$markercode[i]] <- summary_df[records_bold$species_name[i], records_bold$markercode[i]] + 1
      }
    }

    # print("BARCODE SUMMARY")
    # print(summary_df)
    summary_df
  }
  
  reduce_barcode_summary <- function(b_summary) {
    #number of non-zeros in each column
    summary <- b_summary
    count <- apply(summary, 2, function(c)sum(c!=0))
    sums <- apply(summary, 2, sum)
    
    #find most representative 
    
    #result <- c()
    #for (i in 1:length(sums)){
    #    result <- sums[[i]] * count[[i]]
   # }
    #print(c)

    
    #calculate and sort by relavance
    calculated <- names(rev(sort(sums * count)))
    
    #if (ncol(summary) > 3){
    #  summary <- subset(summary, select = c(names(calculated[1]), names(calculated[2]), names(calculated[3])))
    #}
    summary <- subset(summary, select = (calculated))
    # print("Sorted Summary")
    # print(summary)
    summary
  }
  
  summary_report <- function(databaseFlag) {
    if (databaseFlag == 1) {
      #NCBI
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
    } else if (databaseFlag == 0){
      #CRUX
      matrixGetCRUX() %...>% {
        # Gets the matrix for the Crux results
        CRUXmatrix <- . 
        organismListGet() %...>% {
          # Gets the column names for the matrix
          columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") 
          # Adds the column names to the matrix
          colnames(CRUXmatrix) <- columns 
          # Adds the row names to the matrix
          rownames(CRUXmatrix) <- .
          dataframe <- CRUXmatrix
          #calls convert_CRUX()s
          dataframe <- convert_CRUX(dataframe)
          summary_report_dataframe(dataframe)
        }
      }
    } else {
      #BOLD
      summary_report_dataframe(barcode_summary())
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


