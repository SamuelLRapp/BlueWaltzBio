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
    navBarPage("Coverage",
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
              textAreaInput(inputId = "organismList", label = "Species Names"),
              checkboxInput(inputId = "taxizeOption", label = "Include taxonomic resolution", value = TRUE),
              textAreaInput(inputId = "barcodeList", label = "Barcodes of Interest"),
              checkboxInput(inputId = "seqLengthOption", label = "Set minimum sequence lengths(by marker)"),
              uiOutput("seqLenInputs"),
          )
        ),
    
        fluidRow(
          # Show a plot of the generated distribution
          tabsetPanel (type = "tabs", 
              #tabPanel(textOutput("debug")),
              tabPanel(DT::dataTableOutput("coverageResults")),
              #tabPanel(),
          
          )
        )
      ),
      
      #CRUX tab
      tabPanel("CRUX",
               # Application title
               titlePanel("Find CRUX database coverage of your species of interest"),
               
               # Usage instructions
               fluidRow(
                 mainPanel(
                   p("Enter the names of your species of interest")
                 ),
               ),
               
               fluidRow(
                 # Sidebar with a text area for organisms and bar code
                 sidebarPanel(
                   textAreaInput(inputId = "organismList", label = "Species Names"),
                   checkboxInput(inputId = "taxizeOption", label = "Include taxonomic resolution", value = TRUE),
                 )
               ),
      )
    )
))