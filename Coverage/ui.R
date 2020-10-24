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
library(shinyWidgets)

shinyUI(fluidPage(
  
  navbarPage("Coverage",
    #CRUX tab
    tabPanel("CRUX",
           # Application title
           titlePanel("Find CRUX database coverage of your species of interest"),
           
           # Usage instructions
           fluidRow(
             mainPanel(
               h4("Introduction and the role of reference databases: "),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ tool identifies how well sets of organisms are represented in the public CALeDNA reference databases. This tool may be used by either scientists interested in using the public CALeDNA reference database or parties interested in working with CALeDNA scientists for conducting a study. The site displays the taxonomic resolution and reference sequence abundance available within the CALeDNA databases for different organisms, which informs the user how well the databases may fulfill the needs of their study."),
               p(HTML('&emsp;'), "The databases were generated using the CRUX Pipeline, part of the Anacapa Toolkit (Curd et al., 2019 in MEE). The full databases and further documentation can be found here: https://ucedna.com/reference-databases-for-metabarcoding."),
               p(HTML('&emsp;'), "Reference databases allow for taxonomic assignment of metagenomic sequences. DNA sequences are taxonomically identified by matching them to previously identified reference sequences. Large swaths of organisms and taxonomic groups have yet to be DNA barcoded, and thus cannot be detected using metagenomic sequencing. However, organizations like BOLD and the Smithsonian are currently working to fill these holes in our reference libraries."),

               h4("What does the CRUX Coverage Matrix do?"),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ returns a value that represents how many reference  sequences exist for the user’s organism search term(s) in each public database. When direct matches are not found in a database, the tool will instead search for lower taxonomic ranks until a match is found. When a metabarcoding study is being performed, it is critical to confirm the existence of and obtain the reference sequences of organisms of interest, as well as the taxonomic resolution of said sequences, and what metabarcode loci the reference sequences belong to. (See additional information for more details) CRUX databases are designed to be shared, and this tool allows users to assess whether the public CRUX databases meet their study’s taxonomic  requirements. "),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ searches by taxonomic ranks: domain, phylum, class, order, family, genus, genus-spp.The rows of the table produced are the organism search terms, and the columns are CRUX databases: 16S,  12S, 18S, PITS, CO1, FITS, trnL, Vertebrate."),
               p("The cells will show one of the following: "),
               p("1) The number of sequences in a database, if direct matches are found",  HTML("<br/>"), "2) If no direct matches are found, the next most specific taxonomic rank found", HTML("<br/>"), "3) “0” if nothing is found at any taxonomic rank."),#The list

               h4("Additional Information:"),
               dropdown(p(HTML('&emsp;'), "CRUX databases are metabarcode specific, which means each database is oriented around one specific genetic loci that is shared across the organisms in a given CRUX reference database. For example, the 16S ribosomal RNA metabarcoding loci specifically works well in identifying bacteria and archaea, while the trnL Chloroplast UAA loci is specifically useful for identifying plants."), label="Why are there multiple databases? How are they different?"),
               dropdown(p(HTML('&emsp;'), "Taxonomic resolution is the taxonomic rank to which a DNA sequence can successfully be matched to an organism. The highest taxonomic resolution possible is genus-species identification, and the lowest resolution is the largest taxonomic grouping domain. The taxonomic resolution required for a study heavily depends on its goals. A metabarcoding biodiversity survey would likely desire the highest taxonomic resolution possible, whereas a study focused on a specific group of organisms may be okay with lower resolution results. Some studies require identification down to the genus or species level, whereas others may find lower taxonomic resolution acceptable.  The ‘CRUX Coverage Matrix’ determines the taxonomic resolution the CALeDNA public reference databases contain for the input set of organisms."), label="What is taxonomic resolution?"), 
               p() #empty space 
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
    ),
  
  
  
    
      tabPanel("NCBI",          #NCBI Tab    
        # Application title
        titlePanel("Find NCBI records of your species and barcodes of interest"),
    
        # Usage instructions
        fluidRow(
          mainPanel(
            p("Enter the names of your species of interest and genetic bar codes of interest
               separated by commas.\n NOTE: The appication may take upwards of 15 minutes on initial load. Subsequent searches will be faster.")
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
              DT::dataTableOutput("NCBIcoverageResults") %>% withSpinner(color="#0dc5c1")
        )
      )
      
      
    )
))