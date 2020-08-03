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

shinyServer(function(input, output) {
    
    coverage <- reactive({
        organismListLength <- length(organismList())
        codeListLength <- length(barcodeList())
        validate(
            need(organismListLength > 0, 'Please name at least one organism'),
            need(codeListLength > 0, 'Please choose at least one barcode')
        )
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList()){
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
        organismList <- strsplit(input$organismList, ",")
        organismList[[1]]
    })
    
    barcodeList <- reactive({
        barcodeList <- strsplit(input$barcodeList, ",")
        barcodeList[[1]]
    })
    
    output$coverageResults <- DT::renderDataTable(
        coverage(), rownames = organismList(), colnames = barcodeList()
    )
    
})