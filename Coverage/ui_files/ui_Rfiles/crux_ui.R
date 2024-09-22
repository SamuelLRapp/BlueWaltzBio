tabsetPanel( 
  id = "CRUXpage",
  # CRUX Welcome Page
  tabPanel("Start Your CRUX Search",
           fluidRow(
             column(8, align="center", offset = 2,
                    titlePanel("Welcome to the CRUX Metabarcoding Pipeline")
             ),
             column(8, align="justified", offset = 2,
                    includeMarkdown("ui_files/ui_text/crux_welcome_page.md"),
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
                      ))))),
  
  
  # CRUX Organism Search Tab
  tabPanel("Organism Names",
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
                      '<p>Note: for a full list of the data sources that “Taxize” references for proper nomenclature, see the “Taxize” GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'),
                      placement = "right",
                      options = list(container = "body"),
                      trigger="hover"),
                    
                    actionButton("searchButton", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                    tags$div(id = 'CRUXtaxizeOption', style ='font-size:120%')
             ))),
  
  
  # CRUX Summary Data
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
             ))),
  
  
  # Crux Coverage Matrix
  tabPanel("Coverage Matrix",
           div(style="padding-top:40px",),
           sidebarLayout(
             sidebarPanel(
               includeMarkdown("ui_files/ui_text/crux_coverage_matrix.md"),
             ),
             mainPanel(
               column(12, align="center", style='padding-top:15px',
                      titlePanel("Instances of an Organism Found in Each CRUX Metabarcoding Database"),
                      DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1"),
                      conditionalPanel( condition = "output.CRUXcoverageResults",
                                        downloadButton('downloadCrux',"Download Coverage Matrix"))
               )))),
  
  
  # CRUX Information 
  tabPanel("Information",
           titlePanel("Find CRUX database coverage of your organisms of interest"),
           fluidRow(
             mainPanel(
               includeMarkdown("ui_files/ui_text/crux_information_tab.md"),
               CruxUserGuide.icon <- tags$a(href='https://docs.google.com/document/d/1A1_4d21JKkk98WujeqVp51epxDzPrTSr9--_CnM5M5E/edit',
                                            icon("book"),
                                            'CRUX User Guide', target="_blank"),
             ))))