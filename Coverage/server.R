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
        
        organismList <- organismList()
        organismListLength <- length(organismList())
        
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
                searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 0)$count
                results <- c(results, searchResult)
            }
        }
        data <- matrix(results, nrow = organismListLength, ncol = codeListLength, byrow = TRUE)
        data
    })
    
    organismList <- reactive({
        organismList <- strsplit(input$organismList, ",")[[1]]
        if(input$taxizeOption){
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
    
    output$coverageResults <- DT::renderDataTable(
        coverage(), rownames = organismList(), colnames = barcodeList()
    )
    
    output$debug <- renderText(
        # class(taxize_org_list)
        # class(barcodeList)
         #typeof(barcodeList())
         # typeof(taxize_org_list())
        #"  "
         #typeof((gnr_resolve(sci = "homo", data_source_ids = 4)[[1,3]])) 
        #gnr_resolve(sci = "homo", data_source_ids = 4)[[1,3]]
        # NCBI_name <- gnr_resolve(sci = "homo", data_source_ids = 4) #4 = NCBI
        # typeof(strsplit(input$organismList, ",")[[1]])
         # taxize_org_list()[[2]]
        # taxize_org_list()[[3]]
        #length(strsplit(input$organismList, ",")[[1]])
        organismList()
        
    )
    
    
})