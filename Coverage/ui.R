#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinycssloaders)

shinyUI(fluidPage(
    navbarPage("Coverage",
      tabPanel("NCBI",          #NCBI Tab    
        # Application title
        titlePanel("Find NCBI records of your species and barcodes of interest"),
    
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
              textAreaInput(inputId = "NCBIorganismList", label = "Species Names"),
              checkboxInput(inputId = "NCBItaxizeOption", label = "Include taxonomic resolution", value = TRUE),
              textAreaInput(inputId = "barcodeList", label = "Barcodes of Interest"),
              checkboxInput(inputId = "seqLengthOption", label = "Set minimum sequence lengths(by marker)"),
              uiOutput("seqLenInputs"),
          )
        ),
    
        fluidRow(
          # Show a plot of the generated distribution
              DT::dataTableOutput("NCBIcoverageResults")
          
          
        )
      ),
      
      #CRUX tab
      tabPanel("CRUX",
               # Application title
               titlePanel("Find CRUX database coverage of your species of interest"),
               
               # Usage instructions
               fluidRow(
                 mainPanel(
                   p("Enter the names of your species of interest, then hit the \"search\" button.\n NOTE: This may take upwards of 10 minutes on initial load. Subsequent searches will be faster.")
                 ),
               ),
               
               fluidRow(
                 # Sidebar with a text area for organisms and bar code
                 sidebarPanel(
                   textAreaInput(inputId = "CRUXorganismList", label = "Species Names"),
                   checkboxInput(inputId = "CRUXtaxizeOption", label = "Include taxonomic resolution", value = TRUE),
                   actionButton("searchButton", "Search")
                 )
               ),
               
               fluidRow(
                 # Show a plot of the generated distribution
                 DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1")
               )
      )
    )
))