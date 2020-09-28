#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rentrez)
library(taxize)
library(tidyverse)
library(dplyr)

shinyServer(function(input, output) {
    
    file_to_DF <- function(filepath) {                                  #LEAVE THESE HERE FOR LATER
        taxonomytable<-read.delim(filepath, header=FALSE)
        taxonomy_only_table <-select(taxonomytable, V2) %>%
            separate(V2, c("domain", "phylum", "class", "order", "family", "genus", "genus species"), sep = ";",remove=FALSE)
    }
    
    df_18S <- file_to_DF("18S_taxonomy.txt")
    df_16S <- file_to_DF("16S_taxonomy.txt")
    df_PITS <- file_to_DF("PITS_taxonomy.txt")
    df_CO1 <- file_to_DF("CO1_taxonomy.txt")
    df_FITS <- file_to_DF("FITS_taxonomy.txt")
    
    
    dbList <- list(df_18S, df_16S, df_PITS, df_CO1, df_FITS)
    
    
    cruxCoverage <- reactive({
        organismList <- cruxOrganismList()
        organismListLength <- length(organismList)
        
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList){
            for(table in dbList){
                location <- which(table == organism, arr.ind = TRUE)
                if(nrow(location) == 0){
                    searchTerm <- tax_name(query= organism, get= "genus", db= "ncbi")[1,3]
                    location <- which(table == searchTerm, arr.ind = TRUE)
                    if(nrow(location) == 0){
                        searchTerm <- tax_name(query= organism, get= "family", db= "ncbi")[1,3]
                        location <- which(table == searchTerm, arr.ind = TRUE)
                        if(nrow(location)==0){
                            searchTerm <- tax_name(query= organism, get= "order", db= "ncbi")[1,3]
                            location <- which(table == searchTerm, arr.ind = TRUE)
                            if(nrow(location) ==0){
                                searchTerm <- tax_name(query= organism, get= "class", db= "ncbi")[1,3]
                                location <- which(table == searchTerm, arr.ind = TRUE)
                                if(nrow(location)==0){
                                    searchTerm <- tax_name(query= organism, get= "phylum", db= "ncbi")[1,3]
                                    location <- which(table == searchTerm, arr.ind = TRUE)
                                    results <- c(results, nrow(location))
                                } else { results <- c(results, "class")}
                            } else {results <- c(results, "order")}
                        } else {results <- c(results, "family")}
                    }else {results <- c(results, "genus") }
                } else {results <- c(results, toString(nrow(location)))}
            }
            
        }
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE)
        data
    })
    
    cruxOrgSearch <- eventReactive(input$searchButton, {
        input$CRUXorganismList
    })
    
    cruxOrganismList <- reactive({
        organismList <- strsplit(cruxOrgSearch(), ",")[[1]]
        if(input$CRUXtaxizeOption){
            taxize_organism_list <- c()
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b")
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI      THIS NEEDS TO GET VERIFIED
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
    
    seqLenList <- reactive({
        if(input$seqLengthOption){
            textList <- list()
            for(marker in barcodeList()){
                textList <- list(textList, numericInput(marker, paste("Minimum sequence length for", marker), 500, min= 0))
            }
            textList
        }
    })
    
    output$seqLenInputs <- renderUI(seqLenList())


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
    
    output$NCBIcoverageResults <- DT::renderDataTable(
        genBankCoverage(), rownames = NCBIorganismList(), colnames = barcodeList()
    )
    
    output$CRUXcoverageResults <- DT::renderDataTable(
        cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S", "16S", "PITS", "CO1", "FITS")
        
    )
    
    
})