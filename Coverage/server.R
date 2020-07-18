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
        organismList <- strsplit(input$organismList, ",")
        barcodeList <- strsplit(input$barcodeList, ",")
        organismListLength <- length(organismList[[1]])
        codeListLength <- length(barcodeList[[1]])
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList[[1]]){
            for(code in barcodeList[[1]]){
                searchTerm <- paste(organism, "[ORGN] AND ", code, "[GENE]", sep="")
                searchResult <- entrez_search(db = "nucleotide", term = searchTerm)
                results <- c(results, length(searchResult$ids))
            }
        }
        data <- matrix(results, nrow = organismListLength, ncol = codeListLength, byrow = TRUE)
        data
    })
    
    output$coverageResults <- renderTable(
        coverage(), striped = TRUE, hover = TRUE, bordered = TRUE, rownames = TRUE
    )
    
})