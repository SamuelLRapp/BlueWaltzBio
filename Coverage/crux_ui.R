tabsetPanel(
  id = "CRUXpage",
  tabPanel("Start Your CRUX Search",
           # Application title
           fluidRow(
             column(8, align="center", offset = 2,
                    titlePanel("Welcome to the CRUX Metabarcoding Pipeline")
             ),
             column(8, align="justify", offset = 2,
                    p(HTML('&emsp;'), "The CRUX pipeline of RSB takes in a list of organism(s) and searches through the seven publically available CALeDNA 
                                                 CRUX Metabarcode databases to find how many records match the search. The RSB searches through a copy of these databases that are 
                                                 updated periodically. The last update was in October 2019. When direct matches are not found in a database, the tool will then search 
                                                 for higher  taxonomic ranks (genus, family, order, class, phylum, domain), via the R package “Taxize”, until a match is found. For example, if 
                                                 the Giant Seastar (", tags$i("Pisaster giganteus"), ") isn’t found in the ", tags$i("COI"), " database the app will search for the presence of the genus", tags$i("Pisaster,"), " and 
                                                 then family ", tags$i("Asteriidae"), " and so forth."),
                    p(style="padding-bottom:60px", HTML('&emsp;'), "Users are given the choice to use the package “Taxize” to append synonyms and correct spelling mistakes 
                                                 of organism names. The tool then showcases a Coverage Matrix (CM), showing the reference sequence abundance or taxonomic resolution
                                                 for each marker/gene per organism, and a statistical summary of the CM."),
             ),
             
             fluidRow(
               column(6, align="center", offset = 3,
                      fileInput("uCRUXfile", "Choose CSV file to upload", accept = c(".csv"), width=800),
                      actionButton("CruxStart", "Manually enter & adjust Organism name inputs"),
               )
             ),
             
             HTML("<br><br>"),
             
             fluidRow(
               column(6, align="center", offset = 3,
                      actionButton("CRUXUserGuide", 
                                   "CRUX User Guide", 
                                   onclick="window.open('https://docs.google.com/document/d/1A1_4d21JKkk98WujeqVp51epxDzPrTSr9--_CnM5M5E/edit?usp=sharing', '_blank')",
                                   icon=icon("book"),
                                   class="btn-success",
                                   style="margin-right:5px;"
                      ),
                      actionButton("CRUXCsvTemplate", 
                                   "CRUX CSV Template", 
                                   onclick="window.open('https://www.bluewaltzbio.com/research/test-our-tool/ncbi-csv-template', '_blank')",
                                   icon=icon("file-csv"),
                                   class="btn-success",
                                   style="margin-left:5px;",
                      ),
               ),
             ),
             
             
             
           )),
  tabPanel("Organism Names",
           # Application title
           # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
           fluidRow(
             column(6, align="center", offset = 3,  
                    titlePanel("Organism Names"),
                    textAreaInput(inputId = "CRUXorganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                    checkboxInput(inputId = "CRUXtaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package “Taxize”", tags$a(id="CRUXtaxizeHelp",icon("question-circle"))) , value = TRUE, width = 500),
                    checkboxInput(inputId = "CRUXhomonymOption", label = list("Check for homonyms and append to the list of organisms if any are found"), value = TRUE, width = 500),
                    
                    bsPopover(id="CRUXtaxizeHelp", title="Help", content = paste0(
                      '<p>If this box is checked, the programming package “Taxize” will:</p>',
                      '<ol>',
                      '<li><p>Spellcheck each of your organisms’ names before searching the NCBI database.</p></li>',
                      '<li><p>Check if you have the most up-to-date organism names, and replaces your search term if not.</p></li>',
                      '<li><p>Add synonyms for the organism(s) listed to assist in finding more entries. Example: <i>Homo sapiens</i> with the <b>Check spelling and synonyms for organism names</b> box checked will search both &#39;<i>Homo sapiens</i>&#39; and &#39;<i>Homo sapiens subsp. varitus</i>&#39;.</p></li>', # &#39; is HTML for ' (single apostrophe)
                      '</ol>',
                      '<p>Note: for a full list of the data sources that “Taxize” references for proper nomenclature, see the “Taxize” GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'                                          ),
                      placement = "right",
                      options = list(container = "body"),
                      trigger="hover"),
                    actionButton("searchButton", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                    tags$div(id = 'CRUXtaxizeOption', style ='font-size:120%'),
             )),
  ),
  tabPanel("Summary Results",
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    conditionalPanel( condition = "output.CRUXSummaryResults",
                                      titlePanel("Summary of Search Results"),
                                      p("For each barcode, we display the total number of database entries found, the percentage of the total number of database entries that each marker/gene accounts for, and the number of organisms with at least one or no sequences found"),
                    ),
                    tags$div(id="loaderWrapperCRUX"),
                    DT::dataTableOutput("CRUXSummaryResults")  %>% withSpinner(color="#00000000"),
                    conditionalPanel( condition = "output.CRUXSummaryResults",
                                      downloadButton("CRUXfileDownloadSD","Download summary data"),
                                      actionButton("detailsButton", "See More Detailed Results"))
             )),
           
  ),
  tabPanel("Coverage Matrix",
           div(style="padding-top:40px",),
           sidebarLayout(
             sidebarPanel(
               p("The ‘CRUX Coverage Matrix’ returns a value that represents how many reference sequences exist for the user’s organism search 
                                       term(s) in each public database.The rows of the table produced are the organism search terms, and the columns are CRUX databases: ",
                 tags$i("16S,"), "Vertebrate", tags$i("12S,"), tags$i("18S,"), "Plant", tags$i("ITS1,"), tags$i("CO1,"), "Fungal", tags$i("ITS2,"), "and", tags$i("trnL.")),
               p("The cells will show one of the following"),
               tags$ol(
                 tags$li("The number of sequences in a database, if direct matches are found"),
                 tags$li("If no direct matches are found, the next most specific taxonomic rank found"),
                 tags$li("“0” if nothing is found at any taxonomic rank.")
               )
             ),
             mainPanel(
               column(12, align="center", style='padding-top:15px',
                      titlePanel("Instances of an Organism Found in Each CRUX Metabarcoding Database"),
                      DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1"),
                      conditionalPanel( condition = "output.CRUXcoverageResults",
                                        downloadButton('downloadCrux',"Download Coverage Matrix"))
               ) 
             )
             
           )
           
  ),
  tabPanel("Information",
           # Application title
           titlePanel("Find CRUX database coverage of your organisms of interest"),
           # embed_youtube("DNS7i2m4sB0", width = 560, height = 315, frameborder= 0, allowfullscreen = TRUE),
           # Usage instructions
           fluidRow(
             mainPanel(
               h4("Introduction and the role of reference databases: "),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ tool identifies how well sets of organisms are represented in the public CALeDNA reference databases. This tool may be used by either scientists interested in using the public CALeDNA reference database or parties interested in working with CALeDNA scientists for conducting a study. The site displays the taxonomic resolution and reference sequence abundance available within the CALeDNA databases for different organisms, which informs the user how well the databases may fulfill the needs of their study."),
               p(HTML('&emsp;'), "The databases were generated using the CRUX Pipeline, part of the Anacapa Toolkit (Curd et al., 2019 in MEE). The full databases and further documentation can be found here: https://ucedna.com/reference-databases-for-metabarcoding."),
               p(HTML('&emsp;'), "Reference databases allow for the taxonomic assignment of metagenomic sequences. DNA sequences are taxonomically identified by matching them to previously identified reference sequences. Large swaths of organisms and taxonomic groups have yet to be DNA barcoded, and thus cannot be detected using metagenomic sequencing. However, organizations like BOLD and the Smithsonian are currently working to fill these holes in our reference libraries."),
               p(HTML ('&emsp;'), "You can find the public CRUX databases at this link: ", a("https://ucedna.com/reference-databases-for-metabarcoding", href="https://ucedna.com/reference-databases-for-metabarcoding")),
               h4("What does the CRUX Coverage Matrix do?"),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ returns a value that represents how many reference sequences exist for the user’s organism search term(s) in each public database. When direct matches are not found in a database, the tool will instead search for lower taxonomic ranks until a match is found. When a metabarcoding study is being performed, it is critical to confirm the existence of and obtain the reference sequences of organisms of interest, as well as the taxonomic resolution of said sequences, and what metabarcoding loci the reference sequences belong to. (See additional information for more details) CRUX databases are designed to be shared, and this tool allows users to assess whether the public CRUX databases meet their study’s taxonomic requirements. "),
               p(HTML('&emsp;'), "The ‘CRUX Coverage Matrix’ searches by taxonomic ranks: domain, phylum, class, order, family, genus, genus-spp.The rows of the table produced are the organism search terms, and the columns are CRUX databases: 16S,  12S, 18S, PITS, CO1, FITS, trnL, Vertebrate."),
               p("The cells will show one of the following: "),
               p("1) The number of sequences in a database, if direct matches are found",  HTML("<br/>"), "2) If no direct matches are found, the next most specific taxonomic rank found", HTML("<br/>"), "3) “0” if nothing is found at any taxonomic rank."),#The list
               p(), #empty space 
               
               #copied from the twitter icon implementation
               #in the contact us code.
               CruxUserGuide.icon <- tags$a(href='https://docs.google.com/document/d/1A1_4d21JKkk98WujeqVp51epxDzPrTSr9--_CnM5M5E/edit',
                                            icon("book"),
                                            'CRUX User Guide', target="_blank"),
               p(), #for aesthetics
             ),
           ),
  )
)