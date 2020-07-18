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
    
    
    
    output$coverageResults <- renderUI({
        organismList <- strsplit(input$organismList, ",")
        barcodeList <- strsplit(input$barcodeList, ",")
        listLength <- length(organismList[[1]])
        searchTerm <- ""
        searchResult <- 0
        for(organism in organismList[[1]]){
            for(code in barcodeList[[1]]){
                searchTerm <- paste(organism, "[ORGN] AND ", code, "[GENE]", sep="")
                searchResult <- entrez_search(db = "nucleotide", term = searchTerm)
            }
        }
        numResults <- length(searchResult$ids)
        toString(numResults)
    })
    
})