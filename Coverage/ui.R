# ------------------------------------------------------------------------------
# Name: ui.R
# Last Date Updated : September 22, 2024
#
# This code was build by Sriram Ramesh and Jorge Tapias Gomez
# With help from Dickson Chung and Zia Truong
#
# In collaboration with Samuel Rapp, Benjamine Levine, and Daniel Tapias Gomez
# Also, a big thanks to all those that helped us along the project.
# ------------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(shiny)
  library(shinyjs)
  library(shinycssloaders)
  library(shinyWidgets)
  library(tidyverse)
  library(vembedr)
  library(shinydashboard)
  library(shinyalert) # popup library
  library(modules)
  library(shinyjs)
  library(shinyBS) # tooltip library
})
 
source("ui_files/ui_Rfiles/elements_ui.R", echo = FALSE, print.eval = FALSE)[1]

shinyUI(fluidPage(
  navbarPage("Reference Sequence Browser",
             id = "mainPage",
             # Home tab
             tabPanel("Home", 
                      sidebarLayout(
                        sidebarPanel(
                          width=2,
                          div(
                            textInput(inputId="NCBIKey", label="Input your NCBI API key here", width = '250px'),
                            p("Note: the app will still function if you don't have an API Key, but you may experience slower search/download times"),
                            actionButton(inputId = "SetKey", label = "Set Key"),
                          )
                        ),
                        mainPanel(
                          h2(style="text-align:center", "Welcome to the Reference Sequence Browser"),
                          # DON'T DELETE THE COMMENT BELOW, WE USE IT FOR INTERNAL TESTING
                          # actionButton(inputId = "dwntest", label = "Run Download Tests"),
                          h4(style="text-align:center", "Read this page first if you want to make best use of our app!"),
                          p(style="padding-bottom:20px;text-align:center", "This app was developed by the ", a("BlueWaltzBio", href="https://www.bluewaltzbio.com/", target="_blank", rel="noopener noreferrer"), " team. "),
                          includeMarkdown("ui_files/ui_text/rsb_welcome_page.md"),
                        ))),
             # CRUX Tab
             tabPanel("CRUX",
                      source("ui_files/ui_Rfiles/crux_ui.R", echo = FALSE, print.eval = FALSE)[1],
             ),
             # NCBI Tab
             tabPanel("NCBI",
                      source("ui_files/ui_Rfiles/ncbi_ui.R", echo = FALSE, print.eval = FALSE)[1],
             ),
             # BOLD Tab
             tabPanel("BOLD",
                      source("ui_files/ui_Rfiles/bold_ui.R", echo = FALSE, print.eval = FALSE)[1],                      
             ),
             # Contact Tab
             tabPanel("Contact Us", 
                      includeMarkdown("ui_files/ui_text/contact_us.md"),
                      twitter.icon <- tags$a(href='https://twitter.com/?lang=en',
                                             icon("twitter"),
                                             'Twitter', target="_blank")
             ))
  ))
