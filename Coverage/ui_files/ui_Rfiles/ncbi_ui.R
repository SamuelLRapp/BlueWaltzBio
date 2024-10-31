tabsetPanel(
  id = "NCBIpage",
  
  # NCBI Start Page Tab
  tabPanel("Start Your NCBI Search",
           column(8, align="center", offset = 2,
                  titlePanel("Welcome to the NCBI Nucleotide Pipeline")
           ),
           column(8, align="justify", offset = 2,
                  includeMarkdown("ui_files/ui_text/ncbi_welcome_page.md"),
           ),
           fluidRow(
             column(6, align="center", offset = 3, style='padding-top:100px;',
                    fileInput("uNCBIfile", "Choose CSV file to upload", accept = c(".csv"), width=800),
                    actionButton(inputId = "StartNCBIButton", label = "Manually enter & adjust Organism name inputs"),
             )),
           
           fluidRow(
             column(6, align="center", offset = 3,
                    HTML("<br><br>"),
                    actionButton("NCBIUserGuide", 
                                 "NCBI User Guide", 
                                 onclick="window.open('https://docs.google.com/document/d/1-VbO7nzPHY27xDZ714Kzu8xemG2Hsg-Gu5iE8-Da5C8/edit?usp=sharing', '_blank')",
                                 icon=icon("book"),
                                 class="btn-success",
                                 style="margin-right:5px;"
                    ),
                    actionButton("NCBICsvTemplate", 
                                 "NCBI CSV Template", 
                                 onclick="window.open('https://www.bluewaltzbio.com/research/test-our-tool/ncbi-csv-template', '_blank')",
                                 icon=icon("file-csv"),
                                 class="btn-success",
                                 style="margin-left:5px;"
                    )))),
  
  
  # NCBI Organism Search Tab 
  tabPanel("Organism Names",
           fluidRow(
             column(6, align="center", offset = 3,
                    titlePanel("Organism Names"),
                    textAreaInput(inputId = "NCBIorganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                    checkboxInput(inputId = "NCBItaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package “Taxize”", tags$a(id="NCBItaxizeHelp",icon("question-circle")) ), value = TRUE, width = 500),
                    bsPopover(id="NCBItaxizeHelp", title="Help", content = paste0(
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
                    checkboxInput(inputId = "NCBISearchOptionOrgn", label = "Search by the [ORGN] Metadata field", value = TRUE, width = 500),
                    actionButton(inputId = "BarcodesNext", label = "Manually enter & adjust barcodes of interest inputs"),
             ))),
  
  
  # NCBI Barcode Search Tab
  tabPanel("Barcodes of Interest",
           fluidRow(
             column(6, align="center", offset = 3,
                    titlePanel("Barcodes of Interest"),
                    textAreaInput(inputId = "barcodeList", label = "A comma separated list of the genes you want to search. Common genes used as organism barcodes include: CO1, 16S, 18S", width = 500, height = 200),
                    actionButton(inputId = "barcodeOptionCO1", label = list("CO1", tags$a(id="CO1ButtonHelp",icon("question-circle")))),
                    bsPopover(id="CO1ButtonHelp", title="Help", content = paste0(
                      "<p>Naming conventions in NCBI may vary, thus one gene may be found under multiple names.</p>",
                      "<p>Cytochrome oxidase subunit 1, for example, may be found under the names <i>COI</i>, <i>CO1</i>, <i>COXI</i>, and <i>COX1</i>. This button will search for all 3 names and group the results together to make your search more comprehensive.</p>"), trigger="hover" ),
                    actionButton("barcodeOption16S", label = "16S"),
                    actionButton(inputId = "barcodeOption12S", label = "12S"),
                    actionButton(inputId = "barcodeOption18S", label = "18S"),
                    actionButton(inputId = "barcodeOptionITS2", label = "ITS2"),
                    actionButton(inputId = "barcodeOptiontrnl", label = "trnL"),
                    actionButton(inputId = "barcodeOptionITS1", label = "ITS1"),
                    checkboxInput(inputId = "NCBISearchOptionGene", label = "Search by the [GENE] Metadata field", value = TRUE, width = 500),
                    checkboxInput(inputId = "seqLengthOption", label = list("Set minimum sequence lengths(by marker)", tags$a(id="seqLenHelp",icon("question-circle"))) ),
                    bsPopover(id="seqLenHelp", title="Help", content = paste0(
                      "<p>When searching for barcodes, a NCBI database record may only be useful for identifying an organism if it is of an appropriate base pair length. This varies from gene to gene and thus the tool allows each gene’s minimum and maximum base pair lengths to be specified individually.</p>",
                      "<p>By checking this box users can filter their results to only get sequences within a certain range of base pair lengths. New inputs will appear for every barcode specified in the text box above.</p>"
                    ),
                    placement = "right",
                    options = list(container = "body"),
                    trigger="hover" ),
                    uiOutput("seqLenInputs"),
                    actionButton("NCBIRetMaxButton", "One last step!")
             ))),
  
  
  # NCBI Download Number Tab
  tabPanel("One last step!",
           fluidRow(
             column(6, align="center", offset = 3,
                    titlePanel("Will you be downloading sequences?"),
                    p("If you are interested in downloading FASTA or Genbank files from the results, you must pre-specificy the number of sequences to download per organism-barcode combination."),
                    p("If you will not be downloading any sequences, then you can simply move on to the next step"),
                    numericInput("downloadNum", "Number of Sequences to Download per Cell:", 5, min = 1, max = 500),
                    actionButton("NCBIsearchButton", "Search"),
             ))),
  
  
  # NCBI Summary Data
  tabPanel("Summary Results",
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    conditionalPanel( condition = "output.NCBISummaryResults",
                                      titlePanel("Summary of Search Results"),
                                      p("For each barcode, we display the total number of database entries found, the percentage of the total number of database entries that each marker/gene accounts for, and the number of organisms with at least one or no sequences found"),
                    ),
                    tags$div(id="loaderWrapperNCBI"),
                    DT::dataTableOutput("NCBISummaryResults")  %>% withSpinner(color="#00000000"),
                    conditionalPanel( condition = "output.NCBISummaryResults",
                                      downloadButton("NCBIfileDownloadSD","Download Summary Data"),
                                      actionButton("NCBIdetailsButton", "See More Detailed Results"),
                                      actionButton("NCBIStartOverSummary", "Start the Search again"))
             ))),
  
  
  # NCBI Coverage Matrix
  tabPanel("Coverage Matrix",
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    titlePanel("Number of Sequences Found Per Organism-Barcode Pairing"),
                    DT::dataTableOutput("NCBIcoverageResults") %>% withSpinner(color="#0dc5c1"),
                    conditionalPanel( condition = "output.NCBIcoverageResults",
                                      downloadButton('download',"Download Coverage Matrix"),
                                      downloadButton("fileDownloadF","Download FASTA Files"),
                                      downloadButton("fileDownloadG","Download Genbank Files"),
                                      actionButton("NCBIStartOver", "Start the Search again")
                    ))),
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    titlePanel("An Example of the Search Queries Sent to NCBI"),
                    p("Below we show an example of some of the queries we are sending to NCBI Nucleotide. If you wish to check the validity of our results, you can go to the NCBI website and paste these queries into their online search tool and see if you get the same results. If you want to see all of the queries used in this search, you can download them using the button located below."),
                    DT::dataTableOutput("NCBIsearchQueries") %>% withSpinner(color="#0dc5c1"),
                    downloadButton('downloadStatements',"Download the full search terms table"),
                    p())
           )
  ),
  tabPanel("Manual Data Processing Required",
           column(12, align="center", offset=0,style='padding-top:15px',
                  titlePanel("NCBI Search Terms Requiring Manual Processing"),
                  p("For those entries in NCBI where the database returned some incorrect data that we could not process correctly.
                    This is usually due to a error thrown on the NCBI server badly formatted data. In these cases, you may want to manually search in NCBI Nucleotide"),
                  DT::dataTableOutput("NCBINATable") %>% withSpinner(color="#0dc5c1"),
                  downloadButton('downloadNCBINaTable', "Download NA Barcodes table")
           )),
  # NCBI Information Tab
  tabPanel("Information", 
           titlePanel("Find NCBI records of your organisms and barcodes of interest"),
           fluidRow(
             mainPanel(
               includeMarkdown("ui_files/ui_text/ncbi_information_tab.md"),
               NCBIUserGuide.icon <- tags$a(href='https://docs.google.com/document/d/1-VbO7nzPHY27xDZ714Kzu8xemG2Hsg-Gu5iE8-Da5C8/edit?usp=sharing',
                                            icon("book"),
                                            'NCBI User Guide', target="_blank"),
               
               
             ))))