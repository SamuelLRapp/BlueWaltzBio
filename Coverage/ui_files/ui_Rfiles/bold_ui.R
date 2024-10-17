tabsetPanel(
  id = "BOLDpage",
  
  
  # BOLD Welcome Tab
  tabPanel("Start Your BOLD Search",
           fluidRow(
             column(8, align="center", offset = 2,
                    titlePanel("Welcome to the BOLD Metabarcoding Pipeline")
             ),
             column(8, align="justify", offset = 2,
                    p("The BOLD, Barcode of Life, pipeline of the Reference Sequence Browser (RSB) is designed to screen the BOLD 
                                              database for genetic barcode coverage using a list of scientific names provided by the user. Once inputted, 
                                              the application will conduct a search on the most up to date version of the BOLD database using the BOLD Systems API Package for R."),
                    p("The tool allows users to manipulate the results gathered by prompting the user to filter results by country 
                                              of origin and by giving the option to remove entries also present in NCBI. All subsequent tables and visualizations 
                                              are based on this filtering, which can be changed by returning to the appropriate tabs at any time."),
                    p(style="padding-bottom:40px", "Like the other database pipelines, the app produces a CM where the organism names are rows, gene names are columns, 
                                              and each intersection of a row and column shows how many records are in the BOLD database. The app also creates 
                                              a statistical summary of the CM and users may download the associated FASTA files. Afterwards, a variety of tables 
                                              and graphs are presented to help users analyze the data and fine tune their filters."),
                    includeMarkdown("ui_files/ui_text/bold_welcome_page.md"),
                    
             )),
           
           fluidRow(
             column(6, align="center", offset = 3,
                    fileInput("uBOLDfile", "Choose CSV file to upload", accept = c(".csv"), width=800),
                    actionButton("BOLDStartButton", "Manually enter & adjust Organism name inputs"),
                    HTML("<br><br>"),
                    actionButton("BOLDUserGuide", 
                                 "BOLD User Guide", 
                                 onclick="window.open('https://docs.google.com/document/d/1A1_4d21JKkk98WujeqVp51epxDzPrTSr9--_CnM5M5E/edit?usp=sharing', '_blank')",
                                 icon=icon("book"),
                                 class="btn-success",
                                 style="margin-right:5px;"
                    ),
                    actionButton("BOLDCsvTemplate", 
                                 "BOLD CSV Template", 
                                 onclick="window.open('https://www.bluewaltzbio.com/research/test-our-tool/ncbi-csv-template', '_blank')",
                                 icon=icon("file-csv"),
                                 class="btn-success",
                                 style="margin-left:5px;",
                    )))),
  
  
  # BOLD Organism Search Tab
  tabPanel("Organism Names",
           fluidRow(
             column(6, align="center", offset = 3,
                    div(
                      titlePanel("Organism Names"),
                      style = "padding-top: 20px; padding-bottom: 10px;"
                    ),
                    textAreaInput(inputId = "BOLDorganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                    checkboxInput(inputId = "BOLDtaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package “Taxize”", tags$a(id="BOLDtaxizeHelp",icon("question-circle")) ), value = TRUE, width = 500),
                    bsPopover(id="BOLDtaxizeHelp", title="Help", content = paste0(
                      '<p>If this box is checked, the programming package “Taxize” will:</p>',
                      '<ol>',
                      '<li><p>Spellcheck each of your organisms’ names before searching the NCBI database.</p></li>',
                      '<li><p>Check if you have the most up-to-date organism names, and replaces your search term if not.</p></li>',
                      '<li><p>Add synonyms for the organism(s) listed to assist in finding more entries. Example: <i>Homo sapiens</i> with the <b>Check spelling and synonyms for organism names</b> box checked will search both &#39;<i>Homo sapiens</i>&#39; and &#39;<i>Homo sapiens subsp. varitus</i>&#39;.</p></li>', # &#39; is HTML for ' (single apostrophe)
                      '</ol>',
                      '<p>Note: for a full list of the data sources that “Taxize” references for proper nomenclature, see the “Taxize” GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'
                    ),
                    placement = "right",
                    options = list(container = "body"),
                    trigger="hover"),
                    actionButton("BOLDsearchButton", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
             ))),
  
  
  # BOLD Species Not found Tab
  tabPanel("Species Not Found in BOLD Database",
           column(12, align="center", offset=0,style='padding-top:15px',
                  div(id = "bold_species_not_found_panel",
                      titlePanel("Species not found in BOLD database"),
                      p("List of species that were not found when searching the BOLD database, 
                                  if the species are not displayed in this table then they have results in the BOLD database. 
                                  Additionally, the results in this table are not affected by the Country or NCBI filter")
                  ),
                  tags$div(id="loaderWrapperBOLD"),
                  DT::dataTableOutput("BOLDNullSpecies") %>% withSpinner(color="#0dc5c1") #transparent spinner
           )),
  
  
  # BOLD Filters Tab
  tabPanel("Filters",
           fluidRow(
             # Padding at the top so it doesn't clash with tabs
             div(
               style = "padding-bottom: 20px;"
             ),
             # First column set with the dropdown country filter
             column(6, align="center", offset = 3,
                    uiOutput("selectCountry") %>% withSpinner(color="#0dc5c1"),
             )),
           fluidRow(
             div(
               style = "padding-bottom: 30px;"
             ),
             conditionalPanel(condition = "output.selectCountry", 
                              column(6, align="center", offset = 3,  id = "removeNCBICol",
                                     div(
                                       style = "background-color: lightgray; width: 100%; padding: 0px; border-radius: 10px; border: 2px solid black;",
                                       # add ncbi option remove genome
                                       titlePanel("NCBI Entries Filter"),
                                       p("If the box is checked all entries also found in NCBI will be removed."), 
                                       p("If left unchecked then the NCBI entry removal filter won't applied "),
                                       tags$style(".my-class input[type=checkbox] { transform: scale(1.5); } .my-class label { font-size: 15px; display: flex; align-items: center; }"),                                                
                                       div(class = "my-class",
                                           checkboxInput("removeNCBI", label = "Please Check the box to the left to remove all entries also present in NCBI", value = FALSE, width = 450),
                                       ),
                                     ))),  
             column(6, align="center", offset = 3, style='padding-top:15px',
                    conditionalPanel(condition = "output.selectCountry",
                                     div(#class = "buttonClass",
                                       actionButton('BOLDfilterCountries',"Apply Filters"),
                                       actionButton('BOLDSkipFilter',"Skip Filters")))
             ))),
  
  
  # BOLD Summary Data Tab
  tabPanel("Summary Data",
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    titlePanel("Summary of Search Results"),
                    p("For each barcode, we display the total number of database entries found, the percentage of the total number of database entries that each marker/gene accounts for, and the number of organisms with at least one or no sequences found"),
                    DT::dataTableOutput("BOLDSummaryData") %>% withSpinner(color="#0dc5c1"),
                    conditionalPanel(condition = "output.BOLDSummaryData",
                                     downloadButton('downloadBoldSummary', "Download Summary Data"),
                    )))),
  
  
  # BOLD Coverage Matrix Tab
  tabPanel("Coverage Matrix",
           column(12, align="center", style='padding-top:15px',
                  
                  titlePanel("Sequences Found for each Species Classified by Barcode"),
                  p("Total number of entries found for each species classified into the different barcodes found."),
                  p("The different barcodes found in BOLD are ordered left to right from most to least results"),
                  
                  column(width = 12,
                         DT::dataTableOutput("BOLDcoverageResults"),style = "height:500px; overflow-y: scroll;"%>% withSpinner(color="#0dc5c1"),
                  ),
                  conditionalPanel(condition = "output.BOLDcoverageResults",
                                   downloadButton('downloadBoldFasta',"Download Fasta Files"),
                                   downloadButton('downloadBoldMatrix', "Download Coverage Matrix")
                  ))),
  
  
  # BOLD Country Data Table Tab
  tabPanel("Country Data",
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    titlePanel("Total Number of Barcodes Found by Country for Each Unique Species"),
                    column(width = 12,
                           DT::dataTableOutput("BOLDPresentTable"),style = "height:500px; overflow-y: scroll;"%>% withSpinner(color="#0dc5c1"),
                    ),
                    downloadButton('downloadBoldPresent', "Download entries per country table"),
                    titlePanel("Suggested Countries to Add to Your Filter"),
                    
                    p("For those species that have no barcodes found in the country(s) filtered we provide 
                                 the top 3 unselected countries with the most sequence results."),
                    
                    DT::dataTableOutput("BOLDAbsentTable") %>% withSpinner(color="#0dc5c1"),
                    downloadButton('downloadBoldAbsent', "Download suggested country filters table")
             ))),
  
  
  # BOLD Plot Total Species TreeMap Graph
  tabPanel("Plot Total Sequences Per Country",
           fluidRow(
             column(10, align="center", style='padding-top:15px',
                    mainPanel(plotOutput('treemap')),
             ),
             column(2, align="left", style='padding-top:15px',
                    downloadButton('downloadTreeGraph',"Download Graph"),
             )
           )),
  
  
  # BOLD Plot Unique Species Bar Graph
  tabPanel("Plot Unique Species Per Country",
           fluidRow(
             column(10, align="center", style='padding-top:15px',
                    mainPanel(plotOutput('species')),
             ),
             column(2, align="left", style='padding-top:15px',
                    downloadButton('downloadBarGraph',"Download Graph"))
           )),
  
  
  # BOLD Manual Data Processing
  tabPanel("Manual Data Processing Required",
           column(12, align="center", offset=0,style='padding-top:15px', 
                  conditionalPanel(condition = "output.selectCountry", 
                                   titlePanel("Bold UIDs for Entries With Unlabeled Barcodes"),
                                   p("For those entries in BOLD that have unlabeled barcodes, we provide their respective BOLD UIDs 
                                                so that scientists may explore these results further."),
                                   DT::dataTableOutput("BOLDNATable") %>% withSpinner(color="#0dc5c1"),
                                   downloadButton('downloadBoldNaBarcodes', "Download NA Barcodes table"))
           )))