#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyUI(pageWithSidebar(
    
    # Application title
    headerPanel("Dynamically append arbitrary number of inputs"),
    
    # Sidebar with a slider input for number of bins
    sidebarPanel(
        textAreaInput(inputId = "organismList", label = "Species Names"),
        textAreaInput(inputId = "barcodeList", label = "Barcodes of Interest"),
        actionButton(inputId = "searchButton", label = "Search")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        p("This shows how to add an arbitrary number of inputs
      without resetting the values of existing inputs each time a new input is added.
      For example, add a new input, set the new input's value to Option 2, then add
      another input. Note that the value of the first input does not reset to Option 1.")
    )
))