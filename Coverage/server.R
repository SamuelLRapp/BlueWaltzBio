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
    
    file_to_DF <- function(filepath) {
        #phyloseq::import_qiime_sample_data(input$in_metadata$datapath) %>%
        #    as.matrix() %>%
        #    as.data.frame()

        taxonomytable<-read.delim(filepath, header=FALSE)
        taxonomy_only_table <-select(taxonomytable, V2) %>%
            separate(V2, c("domain", "phylum", "class", "order", "family", "genus", "genus species"), sep = ";",remove=FALSE)
        #change taxon data to only have genus_spp, what rGlobi accepts as an argument
    }
    
    df_18S <- file_to_DF("18S_taxonomy.txt")
    
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
            location <- which(taxonomy_only_table == organism, arr.ind = TRUE)
            results <- c(results, nrow(location))
        }
        data <- matrix(results, nrow = organismListLength, ncol = 1, byrow = TRUE)
        data
    })
    
    cruxOrganismList <- reactive({
        organismList <- strsplit(input$CRUXorganismList, ",")[[1]]
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
        cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S")
    )
    
    output$debug <- renderText(
        organismList()
        
    )
    
    
})