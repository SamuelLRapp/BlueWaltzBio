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

shinyServer(function(input, output) {
    
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
        
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE)
        data
    })
    
    NCBIorganismList <- reactive({
        organismList <- strsplit(input$NCBIorganismList, ",")[[1]]
        if(input$NCBItaxizeOption){
            taxize_organism_list <- c()
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b")
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
                row_count <- nrow(NCBI_names)
                
                if(row_count > 0)
                {
                    for(j in 1:row_count)
                    {
                        taxa_name <- NCBI_names[[j,3]]
                        taxize_organism_list <- c(taxize_organism_list, taxa_name)
                    }
                }
                else
                {
                    taxize_organism_list <- c(taxize_organism_list, organism)
                }
            }
            taxize_organism_list  
        } else{
            organismList
        }
    })
    
    barcodeList <- reactive({
        barcodeList <- strsplit(input$barcodeList, ",")
        barcodeList[[1]]
    })
    
    seqLenList <- reactive({
        if(input$seqLengthOption){
            textList <- list()
            for(marker in barcodeList()){
                textList <- list(textList, numericInput(marker, paste("Minimum sequence length for", marker), 500, min= 0))
            }
            textList
        }
    })
    
    genBankCoverage <- reactive({
        
        organismList <- NCBIorganismList()
        organismListLength <- length(organismList)
        
        codeListLength <- length(barcodeList())
        validate(
            need(organismListLength > 0, 'Please name at least one organism'),
            need(codeListLength > 0, 'Please choose at least one barcode')
        )
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList){
            for(code in barcodeList()){
                searchTerm <- paste(organism, "[ORGN] AND ", code, "[GENE]", sep="")
                if(input$seqLengthOption){
                    searchTerm <- paste(searchTerm, " AND ", input[[code]],":99999999[SLEN]", sep="")
                }
                searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 0)$count
                results <- c(results, searchResult)
            }
        }
        data <- matrix(results, nrow = organismListLength, ncol = codeListLength, byrow = TRUE)
        data
    })
    

    output$seqLenInputs <- renderUI(seqLenList())
    
    output$NCBIcoverageResults <- DT::renderDataTable(
        genBankCoverage(), rownames = NCBIorganismList(), colnames = barcodeList()
    )
    
    output$CRUXcoverageResults <- DT::renderDataTable(
        cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
        
    )
    
    # Download NCBI table
    output$download <- downloadHandler(
        filename = function() {
            paste(input$NCBIorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- barcodeList()
            NCBImatrix <- genBankCoverage()
            colnames(NCBImatrix) <- columns
            rownames(NCBImatrix) <- NCBIorganismList()
            write.csv(NCBImatrix, file)
        }
    )
    
    # Download options
    output$downloadCrux <- downloadHandler(
        filename = function() {
            paste(input$CRUXorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
            CRUXmatrix <- cruxCoverage()
            colnames(CRUXmatrix) <- columns
            rownames(CRUXmatrix) <- cruxOrganismList()
            write.csv(CRUXmatrix, file)
        }
    )
})