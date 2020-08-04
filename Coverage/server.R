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

shinyServer(function(input, output) {
    
    coverage <- reactive({
        organismListLength <- length(taxize_org_list())
        codeListLength <- length(barcodeList())
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in taxize_org_list()){
            for(code in barcodeList()){
                searchTerm <- paste(organism, "[ORGN] AND ", code, "[GENE]", sep="")
                searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 0)$count
                results <- c(results, searchResult)
            }
        }
        data <- matrix(results, nrow = organismListLength, ncol = codeListLength, byrow = TRUE)
        data
    })
    
    taxize_org_list <- reactive({
        taxize_organism_list <- c()
        
        for(organism in organismList())
        {
            NCBI_name <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
            row_count <- nrow(NCBI_name)

            if(row_count > 0)
            {
                for(i in 1:row_count)
                {
                    taxa_name <- NCBI_name[[i,3]]
                    taxize_organism_list <- c(taxize_organism_list, taxa_name)
                }
            }
            else
            {
                taxize_organism_list <- c(taxize_organism_list, organism)
            }
        }
        taxize_organism_list
    })
    
    organismList <- reactive({
        organismList <- strsplit(input$organismList, ",")
        organismList[[1]]
    })
    
    barcodeList <- reactive({
        barcodeList <- strsplit(input$barcodeList, ",")
        barcodeList[[1]]
    })
    
    output$coverageResults <- DT::renderDataTable(
        coverage(), rownames = taxize_org_list(), colnames = barcodeList()
    )
    
    output$debug <- renderText(
        # class(taxize_org_list)
        # class(barcodeList)
         #typeof(barcodeList())
         # typeof(taxize_org_list())
        
         typeof((gnr_resolve(sci = "homo", data_source_ids = 4)[[1,3]])) 
        # NCBI_name <- gnr_resolve(sci = "homo", data_source_ids = 4) #4 = NCBI
         
         # taxize_org_list()[[2]]
        # taxize_org_list()[[3]]
        
    )
    
    
})