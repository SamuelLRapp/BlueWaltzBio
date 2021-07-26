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
library(tidyverse)
library(vembedr)
library(shinydashboard)
library(shinyalert) # popup library
#library(shinybusy)

shinyUI(fluidPage(
  
  navbarPage("Coverage",
    
    #CRUX tab
    tabPanel("Home", 
             titlePanel("Welcome to the Reference Sequence Browser"),
             p(HTML('&emsp;'), "The Reference Sequence Browser rShiny application returns how many publicly accessible genetic barcodes exist in the NCBI nucleotide database or the CRUX databases.
 Users only need to assemble a list of organisms (and gene names for the NCBI search) for the tool to search the NCBI and CRUX databases.
" ),
             p(HTML('&emsp;'), "This rShiny app was built to bridge the gap between ecologists and computer scientists by providing efficient and intuitive access to NCBI and CRUX databases without the user having to write a single line of code. If you would like to learn more details about either the NBCI or CRUX search browser specific design and function click on the relevant tab above. Assemble a list of organisms and get started! 
" ),
             p(HTML('&emsp;'), "The diagram represents how the two search portals function in the back end."),
             img(src='backend.png',  align = "center", height = 450, width = 900),
          
                          
             ),
    tabPanel("CRUX",
             tabsetPanel(
             
               tabPanel("Search",
                        # Application title
                        titlePanel("Find CRUX database coverage of your species of interest"),
                          img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                        
                        # Usage instructions
                        fluidRow(
                          mainPanel(
                            h4("Additional Information:"),
                            dropdown(p(HTML('&emsp;'), "CRUX databases are metabarcode specific, which means each database is oriented around one specific genetic loci that is shared across the organisms in a given CRUX reference database. For example, the 16S ribosomal RNA metabarcoding loci specifically works well in identifying bacteria and archaea, while the trnL Chloroplast UAA loci is specifically useful for identifying plants."), label="Why are there multiple databases? How are they different?"),
                            dropdown(p(HTML('&emsp;'), "Taxonomic resolution is the taxonomic rank to which a DNA sequence can successfully be matched to an organism. The highest taxonomic resolution possible is genus-species identification, and the lowest resolution is the largest taxonomic grouping domain. The taxonomic resolution required for a study heavily depends on its goals. A metabarcoding biodiversity survey would likely desire the highest taxonomic resolution possible, whereas a study focused on a specific group of organisms may be okay with lower resolution results. Some studies require identification down to the genus or species level, whereas others may find lower taxonomic resolution acceptable.  The ‘CRUX Coverage Matrix’ determines the taxonomic resolution the CALeDNA public reference databases contain for the input set of organisms."), label="What is taxonomic resolution?"), 
                            p(), #empty space 
                            
                          ),
                        ),
                        
                        fluidRow(
                          # Sidebar with a text area for organisms and bar code
                          sidebarPanel(
                            useShinyalert() #this line is needed for the popup,
                            fileInput("uCRUXfile", "Choose CSV file to upload", accept = c(".csv")),
                            actionButton(inputId = "uploadCRUXButton", label = "Upload file to textboxes"),
                            textAreaInput(inputId = "CRUXorganismList", label = "Species Names"),
                            checkboxInput(inputId = "CRUXtaxizeOption", label = "Check spelling and synonyms for organism names", value = TRUE),
                            actionButton("searchButton", "Search")
                          ), 
                          mainPanel(
                            # Show a plot of the generated distribution
                            DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1"),
                            # Download button
                            downloadButton('downloadCrux',"Download table"),
                          )
                        ),
                        ),
               tabPanel("Information",
                        # Application title
                        titlePanel("Find CRUX database coverage of your species of interest"),
                        embed_youtube("DNS7i2m4sB0", width = 560, height = 315, frameborder= 0, allowfullscreen = TRUE),
                        
                        # Usage instructions
                        fluidRow(
                          mainPanel(
                            h4("Introduction and the role of reference databases: "),
                            p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ tool identifies how well sets of organisms are represented in the public CALeDNA reference databases. This tool may be used by either scientists interested in using the public CALeDNA reference database or parties interested in working with CALeDNA scientists for conducting a study. The site displays the taxonomic resolution and reference sequence abundance available within the CALeDNA databases for different organisms, which informs the user how well the databases may fulfill the needs of their study."),
                            p(HTML('&emsp;'), "The databases were generated using the CRUX Pipeline, part of the Anacapa Toolkit (Curd et al., 2019 in MEE). The full databases and further documentation can be found here: https://ucedna.com/reference-databases-for-metabarcoding."),
                            p(HTML('&emsp;'), "Reference databases allow for the taxonomic assignment of metagenomic sequences. DNA sequences are taxonomically identified by matching them to previously identified reference sequences. Large swaths of organisms and taxonomic groups have yet to be DNA barcoded, and thus cannot be detected using metagenomic sequencing. However, organizations like BOLD and the Smithsonian are currently working to fill these holes in our reference libraries."),
                            
                            h4("What does the CRUX Coverage Matrix do?"),
                            p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ returns a value that represents how many reference sequences exist for the user’s organism search term(s) in each public database. When direct matches are not found in a database, the tool will instead search for lower taxonomic ranks until a match is found. When a metabarcoding study is being performed, it is critical to confirm the existence of and obtain the reference sequences of organisms of interest, as well as the taxonomic resolution of said sequences, and what metabarcoding loci the reference sequences belong to. (See additional information for more details) CRUX databases are designed to be shared, and this tool allows users to assess whether the public CRUX databases meet their study’s taxonomic requirements. "),
                            p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ searches by taxonomic ranks: domain, phylum, class, order, family, genus, genus-spp.The rows of the table produced are the organism search terms, and the columns are CRUX databases: 16S,  12S, 18S, PITS, CO1, FITS, trnL, Vertebrate."),
                            p("The cells will show one of the following: "),
                            p("1) The number of sequences in a database, if direct matches are found",  HTML("<br/>"), "2) If no direct matches are found, the next most specific taxonomic rank found", HTML("<br/>"), "3) “0” if nothing is found at any taxonomic rank."),#The list
                            p(), #empty space 
                            
                          ),
                        ),
                  )
               ),
               

         

           
           # # Download button
           # downloadButton('downloadCrux',"Download table"),
           # 
           # fluidRow(
           #   # Show a plot of the generated distribution
           #   DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1")
           # )
    ),
  

      tabPanel("NCBI",          #NCBI Tab    
        # Application title

        tabsetPanel(
          tabPanel("Search", 
            titlePanel("Find NCBI records of your species and barcodes of interest"),
            fluidRow(
              mainPanel(
                h4("Descriptions of each Search Field"),
                dropdown(label="Species List (Text box)", p("A comma separated list of the names for your organism(s) of interest. All taxonomic ranks apply.")),
                dropdown(label="Check spelling and synonyms for organism names (Check box)", p("If this box is checked, the programing package ‘Taxize’ will: "), p("1) Spell check each of your species names before searching the NCBI database",  HTML("<br/>"), "2) Check if you have the most up to date organism names, and replaces your search term if not", HTML("<br/>"), "3) Add synonyms for the organism(s) listed to assist in finding more entries. Example: Homo sapien with the 'Check spelling and synonyms for organism names' box checked will search both ‘Homo sapien’ and ‘Homo sapien varitus’"), p("Note: for a full list of the data sources that Taxize references for proper nomenclature, see the Taxize github here: https://github.com/ropensci/taxize")),
                dropdown(label="Barcodes of Interest (Text box)", p("A comma separated list of the genes you want to search. Common genes used as organism barcodes include: CO1, 16S, 18S, rbcL, matK, ITS, FITS, trnL, Vert12S."), p("Note: naming conventions in NCBI may vary, thus one gene may be found under multiple names. Cytochrome Oxidase subunit 1, for example, may be found under the names COI, CO1, COXI, and COX1.")),
                dropdown(label="Minimum sequence lengths", p("When searching for barcodes, A NCBI database record may only be useful for identifying an organism if it is above a certain base pair length. This varies from gene to gene and thus the tool allows each gene’s minimum base pair length to be specified individually."), p("By checking this box users can set a minimum base pair length filter. Entries that are below the specified base pair length, won’t appear in the coverage matrix output.")),
                p("")

              ),
            ),
            fluidRow(
              # Sidebar with a text area for organisms and bar code
              sidebarPanel(
                fileInput("uNCBIfile", "Choose CSV file to upload", accept = c(".csv")),
                actionButton(inputId = "uploadNCBIButton", label = "Upload file to textboxes"),
                textAreaInput(inputId = "NCBIorganismList", label = "Species Names"),
                checkboxInput(inputId = "NCBItaxizeOption", label = "Check spelling and synonyms for organism names", value = TRUE),
                checkboxInput(inputId = "NCBISearchOptionOrgn", label = "Search by Metadata", value = TRUE),
                fluidRow(column(width = 3, actionButton(inputId = "barcodeOptionCO1", label = "CO1")),
                         column(width = 3, actionButton("barcodeOption16S", "16S")),
                         column(width = 3, actionButton(inputId = "barcodeOption12S", label = "12S")),
                         column(width = 3, actionButton(inputId = "barcodeOption18S", label = "18S"))
                         
                ),
                fluidRow(
                  column(width = 4, actionButton(inputId = "barcodeOptionITS2", label = "ITS2")),
                  column(width = 4, actionButton(inputId = "barcodeOptiontrnl", label = "trnl")),
                  column(width = 4, actionButton(inputId = "barcodeOptionITS1", label = "ITS1"))
                ),
                textAreaInput(inputId = "barcodeList", label = "Barcodes of Interest"),
                checkboxInput(inputId = "NCBISearchOptionGene", label = "Search by Metadata", value = TRUE),
                checkboxInput(inputId = "seqLengthOption", label = "Set minimum sequence lengths(by marker)"),
                uiOutput("seqLenInputs"),
                actionButton("NCBIsearchButton", "Search"),
              ),

              mainPanel(  
                DT::dataTableOutput("NCBIcoverageResults") %>% withSpinner(color="#0dc5c1"),
                # Download button
                downloadButton('downloadStatements',"Download Search Terms table"),
                downloadButton('download',"Download Counts table"),
                downloadButton("fileDownloadF","Download FASTA Files"),
                downloadButton("fileDownloadG","Download Genbank Files"),
                #add_busy_spinner(spin = "fading-circle")
              )
            )

          ),
          tabPanel("Information", 
            titlePanel("Find NCBI records of your species and barcodes of interest"),
            embed_youtube("DNS7i2m4sB0", width = 560, height = 315, frameborder= 0, allowfullscreen = TRUE),
            fluidRow(
              mainPanel(
                h4("What does the tool do?"),
                p("The ‘NCBI Nucleotide Coverage Matrix’ was designed to screen the Nucleotide database for genetic barcode coverage prior to environmental DNA metabarcoding studies. Before conducting a metabarcoding study, scientists need to be aware of which organisms have reference sequences at known genetic barcoding loci. The tool finds out if the Nucleotide database contains sequences labeled with a specific gene and organism name. Numerous searches can be done in parallel instead of manually searching for each species-gene combination on the NCBI Nucleotide website."),
                p("The ‘NCBI Nucleotide Coverage Matrix’ tool takes in a list of species and genes of interest and then queries the Nucleotide database to find how many records match the search. The tool then produces a table where the organism names are rows, gene names are columns, and each intersection of a row and column shows how many records are in the NCBI Nucleotide database. All of the search options are detailed in the ‘Search fields' section below. The power and flexibility of this tool allows scientists to check the NCBI Nucleotide database for genetic coverage in ways that aren’t possible without knowledge of the NCBI Entrez coding package."),
                
                h4("Limitations:"),
                p("This tool may not find all possible entries that the user desires. Some limitations of this text based search include, but are not limited to:"),
                p("1) Alternative names of the listed gene in NCBI Nucleotide database",  HTML("<br/>"), "2) Incorrect or missing metadata", HTML("<br/>"), "3) Full genomes entries with unlabeled individual genes"),#The list
                p("Searching by Blast or in Silico PCR  are more empirical ways of identifying quality reference sequences, but are not implemented here. In addition, This tool only searches against the NCBI Nucleotide database, and thus doesn’t include other genetic databases such as BOLD, Silva, Fishbase, etc."),
              )
            )
          )
        ),
      ),
    
   tabPanel("Contact Us", 
            
            titlePanel("Contact us"),
            p(HTML('&emsp;'), "This app was developed by the BlueWaltzBio team." ),
            p(HTML('&emsp;'), "Learn more about us on our website BlueWaltzBio.com or direct message us on twitter @BlueWaltzBio" ),
            p(HTML('&emsp;'), "If you want to provide feedback please use the google link: https://forms.gle/ysT6g8sk1zxWQ1wZA"),
            p(HTML('&emsp;'), "The app was iteratively built with direct feedback from eDNA scientists who spoke with members of BlueWaltzBio. In total our team has interviewed over 70 lab techs, professors, government regulators, and investors in the field of Environmental DNA. 
            " ),
            twitter.icon <- tags$a(href='https://twitter.com/?lang=en',
                                   icon("twitter"),
                                   'Twitter', target="_blank")
            
            )
      
      
    )
))
