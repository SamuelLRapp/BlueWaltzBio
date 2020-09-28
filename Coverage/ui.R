#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(fluidPage(
    
    # Application title
    titlePanel("Search for database coverage of your species and barcodes of interest"),
    
    # Usage instructions
    fluidRow(
      mainPanel(
        p("Enter the names of your species of interest and genetic bar codes of interest
           separated by commas")
      ),
    ),
 
    fluidRow(
      # Sidebar with a text area for organisms and bar code
      sidebarPanel(
          textAreaInput(inputId = "organismList", label = "Species Names"),
          checkboxInput(inputId = "taxizeOption", label = "Include taxonomic resolution", value = TRUE),
          textAreaInput(inputId = "barcodeList", label = "Barcodes of Interest"),
          checkboxInput(inputId = "seqLengthOption", label = "Set minimum sequence lengths(by marker)"),
          uiOutput("seqLenInputs"),
          actionButton(inputId = "searchButton", label = "Search")
      )
    ),
    
    fluidRow(
      # Show a plot of the generated distribution
      mainPanel (
          DT::dataTableOutput("coverageResults"),
          textOutput("debug")
      )
    )
))