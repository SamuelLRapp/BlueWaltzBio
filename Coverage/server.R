#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output) {
    
    output$allInputs <- renderUI({
        # Get value of button, which represents number of times pressed (i.e. number of inputs added)
        inputsToShow <- input$appendInput
        # Return if button not pressed yet
        if(is.null(inputsToShow) || inputsToShow < 1) return()
        # Initialize list of inputs
        inputTagList <- tagList()
        # Populate the list of inputs
        lapply(1:inputsToShow,function(i){
            # Define unique input id and label
            newInputId <- paste0("input", i)
            newInputLabel <- paste("Input", i)
            # Prevent dynamic inputs from resetting
            newInputValue <- "Option 1"
            if (newInputId %in% names(input)) {
                newInputValue <- input[[newInputId]]
            }
            # Define new input
            newInput <- selectInput(newInputId, newInputLabel, c("Option 1", "Option 2", "Option 3"), selected=newInputValue)
            # Append new input to list of existing inputs
            inputTagList <<- tagAppendChild(inputTagList, newInput)
        })
        # Return updated list of inputs
        inputTagList
    })
    
})