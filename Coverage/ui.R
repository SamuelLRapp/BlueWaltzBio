#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

jsCode <- paste0('shinyjs.clickBtn = function(params){
                    console.log("btn clicked");
                    var defaultParams = {
                       btnId : ""
                    };
                    params = shinyjs.getParams(params, defaultParams);
                    btn = $("#"+params.btnId);
                    console.log(btn);
                    btn[0].click();
                  }')

tst <- paste0('shinyjs.ncbiDwnFastaTest = function(params){
                var defaultParams = {
                   dwnBtnId : "fileDownloadF",
                   waitTimeForFileDwn: 40000,
                   testOrganisms: "gallus gallus",
                   testBarcodes: "(CO1; COI; COX1)"
                };
                params = shinyjs.getParams(params, defaultParams);
                
                //set search params
                Shiny.setInputValue("NCBIorganismList", params.testOrganisms);
                Shiny.setInputValue("barcodeList", params.testBarcodes);
                shinyjs.clickBtn("NCBIsearchButton"); //run search
  
                //loop to check if search complete
                var dwnPanelVis = false;
                console.log(dwnPanelVis);
                var intr = setInterval(function() {
                    
                  dwnPanelVis = $("#"+params.dwnBtnId).is(":visible");
                  //run this code after search completed
                  if (dwnPanelVis) {
                    clearInterval(intr);
                    console.log("loop finished");
                    shinyjs.clickBtn(params.dwnBtnId);
                    setTimeout(function(){
                      //let server know to check if file has downloaded
                        Shiny.onInputChange("ui-test-complete", true);
                    }, params.waitTimeForFileDwn);
                  }
                }, 1000)
              }')
loaderJs <- '
shinyjs.setLoaderAppearance = function(tab){
  $("#shiny-notification-panel").addClass("custom-loader");
  $("#shiny-notification-panel").detach().prependTo("#loaderWrapper"+tab);
  }
'

components <- modules::use("components.R")

shinyUI(fluidPage(
  useShinyjs(),
  extendShinyjs(text = jsCode, functions = c("clickBtn")),
  extendShinyjs(text = tst, functions = c("ncbiDwnFastaTest")),
  extendShinyjs(text = loaderJs, functions = c("setLoaderAppearance")),
  tags$link(rel="stylesheet", type="text/css", href="styles.css"),
  navbarPage("Reference Sequence Browser",
             id = "mainPage",
             
             # Home tab
             #img(src='backend.png',  align = "center", height = 350, width = 700), Image in HOME page leaving it here just in case
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
                               p(style="text-align:left", HTML('&emsp;'), "The Reference Sequence Browser (RSB) is a rShiny application that enables the eDNA community to screen and extract organism-specific data from genetic barcode databases such as the ", 
                                 a("NCBI Nucleotide", href="https://www.ncbi.nlm.nih.gov/nucleotide/", target="_blank", rel="noopener noreferrer"), ", ", 
                                 a("NCBI Genome", href="https://www.ncbi.nlm.nih.gov/genome/", target="_blank", rel="noopener noreferrer"), ", ", 
                                 a("BOLD (Barcode for Life Database)", href="https://boldsystems.org", target="_blank", rel="noopener noreferrer"), ", and publicly available ", a("CRUX databases", href="https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13214", target="_blank", rel="noopener noreferrer"), 
                                 ". The app is best used prior to conducting metabarcoding to assess reference sequence availability or to download non duplicative FASTA files from BOLD and NCBI to develop local sequence databases for q-pcr development.",
                                 " Each different database can be searched in its own tab in RSB and has its own quick user-guide."),
                               p(style="text-align:left", HTML('&emsp;'), "The tool is built to search for many organisms and genetic barcodes simultaneously and thus can be used in multiple ways to speed up Environmental DNA workflows. Users only need to assemble",
                                 " a list of organisms in scientific names for the tool to search for reference sequences at known barcoding loci (e.g COI, 16S, 18S, trnL). Only NCBI additionally requires barcode-gene names to complete its Nucleotide search. To make",
                                 " inputting long lists into the app easier, download ", a("this CSV template", href="https://www.bluewaltzbio.com/research/test-our-tool/ncbi-csv-template", target="_blank", rel="noopener noreferrer"), " and fill it out with your organism and barcode names. This template can be uploaded within any database tab."),
                               p(style="text-align:left", HTML('&emsp;'), "The app uses the R packages ‘rentrez’ and ‘bold’ to access the live, up to date NCBI and BOLD databases. RSB retrieves and displays the number of reference sequences in one of the",
                                 " aforementioned databases for the combination of an organism AND a barcoding loci. For example RSB searches for instances of Canis’s Lupus’s COI barcode-gene. Thus every database tab produces a Coverage Matrix (CM) with organism",
                                 " names as rows and barcoding loci as columns, with cells that display the number of reference sequences retrieved. Each tab also includes summary statistics and tab specific visualizations/tables to help make sense of searches of numerous species and barcodes." ),
                               p(style="text-align:left", HTML('&emsp;'), "In addition to previewing what sequences are available in the CM tables, users are able to download the sequences in FASTA file format (excluding CRUX); which can then easily be imported into various genomics softwares",
                                 " (e.g Geneious, etc). The RSB BOLD database search allows users to exclude entries also in NCBI Nucleotide from the BOLD CM results, visualizations, and FASTA downloads. This allows users to avoid downloading duplicate FASTA files between BOLD and NCBI Nucleotide."),
                               h4("Use cases:"),
                               p(style="text-align:left", HTML('&emsp;'), "RSB can be used to improve the workflow of species specific q-pcr development for eDNA applications (Klymus 1). If you are interested in developing a species specific q-primer, RSB can be used to rapidly create a non-duplicative local sequence database by downloading FASTA files from the NCBI and BOLD tabs."),
                               p(style="text-align:left", HTML('&emsp;'), "RSB can also be used to determine what organisms can and to what be detected by metabarcoding and which metabarcodes fit your study needs This can be determined by searching in the seven metabarcoding databases (16S,  12S, 18S, ITS1, CO1, ITS2, trnL) in the CRUX tab or in the BOLD tab."),
                               p(style="text-align:left", HTML('&emsp;'), "Additionally, If you are interested in finding full mitochondrial or chloroplast genomes in NCBI Nucleotide or entries in NCBI Genome go to the full genome tab. Guides to the aforementioned processes can be found lower down on this page."),
                               p(style="text-align:left", HTML('&emsp;'), "Lastly, we hope this tool may be used to point to taxonomic groups lacking publically available reference sequences and thus aid in creating more deliberate and specific sequencing efforts."),
                               p(style="text-align:left;padding-top:30px", HTML('&emsp;'), "This rShiny app was built in part to bridge the gap between eDNA scientists and large genomics databases by providing efficient and high throughput access to NCBI, BOLD, and CRUX databases without the user having to write a single line of code. Click on one of the tabs to get started."),
                        ),
                      ),
                      
                      
             ),
             tabPanel("CRUX",
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
                                                 for higher  taxonomic ranks (genus, family, order, class, phylum, domain), via the R package Taxize, until a match is found. I.E if 
                                                 the Giant Seastar (Pisaster giganteus) isn’t found in the COI database the app will search for the presence of the genus Pisaster, and 
                                                 then family Asteriidae and so forth."),
                                               p(style="padding-bottom:60px", HTML('&emsp;'), "Users are given the choice to utilize the package Taxize to append synonyms and correct spelling mistakes 
                                                 of organism names. The tool then showcases a Coverage Matrix (CM), showing the reference sequence abundance or taxonomic resolution
                                                 for each barcoding loci per organism, and a statistical summary of the CM."),
                                 ),
                                 
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          fileInput("uCRUXfile", "Choose CSV file to upload", accept = c(".csv"), width=800),
                                          #actionButton(inputId = "uploadCRUXButton", label = "Upload file to textboxes"),
                                          actionButton("CruxStart", "Manually enter & adjust Organism name inputs"),
                                          #tags$style(type='text/css', "#uCRUXfile { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
                                          #tags$style(type='text/css', "#Button { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
                                          #tags$style(type='text/css', "#uploadCRUXButton { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
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
                                          checkboxInput(inputId = "CRUXtaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package Taxize", tags$a(id="CRUXtaxizeHelp",icon("question-circle"))) , value = TRUE, width = 500),
                                          bsPopover(id="CRUXtaxizeHelp", title="Help", content = paste0(
                                            '<p>If this box is checked, the programming package <i>Taxize</i> will:</p>',
                                            '<ol>',
                                            '<li><p>Spellcheck each of your organisms’ names before searching the NCBI database.</p></li>',
                                            '<li><p>Check if you have the most up-to-date organism names, and replaces your search term if not.</p></li>',
                                            '<li><p>Add synonyms for the organism(s) listed to assist in finding more entries. Example: <i>Homo sapiens</i> with the <b>Check spelling and synonyms for organism names</b> box checked will search both &#39;<i>Homo sapiens</i>&#39; and &#39;<i>Homo sapiens subsp. varitus</i>&#39;.</p></li>', # &#39; is HTML for ' (single apostrophe)
                                            '</ol>',
                                            '<p>Note: for a full list of the data sources that <i>Taxize</i> references for proper nomenclature, see the <i>Taxize</i> GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'                                          ),
                                          placement = "right",
                                          options = list(container = "body"),
                                          trigger="hover"),
                                          actionButton("searchButton", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                                          tags$div(id = 'CRUXtaxizeOption', style ='font-size:120%'),
                                          #tags$style(type='text/css', "#CRUXorganismList { vertical-align- middle; height- 100px; width- 100%; font-size- 200px;}"),
                                          #tags$style(type='text/css', "#CRUXtaxizeOption { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}"),
                                          #tags$style(type='text/css', "#searchButton { vertical-align- middle; height- 50px; width- 100%; font-size- 30px;}")
                                   )),
                        ),
                        tabPanel("Summary Results",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          titlePanel("Summary of Search Results"),
                                          p("For each barcode we display the total number of entries found and the number of organisms with at least one or no sequence"),
                                          tags$div(id="loaderWrapperCRUX"),
                                          DT::dataTableOutput("CRUXSummaryResults") %>% withSpinner(color="#0dc5c1"),
                                          conditionalPanel( condition = "output.CRUXSummaryResults",
                                                            downloadButton("CRUXfileDownloadSD","Download summary data"),
                                                            actionButton("detailsButton", "See More Detailed Results"))
                                   )),
                                 
                        ),
                        tabPanel("Coverage Matrix",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution and the corresponding buttons
                                 div(style="padding-top:40px",),
                                 sidebarLayout(
                                   sidebarPanel(
                                     p("The ‘CRUX Coverage Matrix’ returns a value that represents how many reference sequences exist for the user’s organism search 
                                       term(s) in each public database.The rows of the table produced are the organism search terms, and the columns are CRUX databases: 
                                       16S, 12S, 18S, PITS, CO1, FITS, trnL, Vertebrate."),
                                     p("The cells will show one of the following"),
                                     tags$ol(
                                       tags$li("The number of sequences in a database, if direct matches are found"),
                                       tags$li("If no direct matches are found, the next most specific taxonomic rank found"),
                                       tags$li("“0” if nothing is found at any taxonomic rank.")
                                     )
                                   ),
                                   mainPanel(
                                     column(12, align="center", style='padding-top:15px',
                                            titlePanel("Instances of a organism found in each CRUX metabarcoding database"),
                                            DT::dataTableOutput("CRUXcoverageResults") %>% withSpinner(color="#0dc5c1"),
                                            conditionalPanel( condition = "output.CRUXcoverageResults",
                                                              downloadButton('downloadCrux',"Download table"))
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
                      ),
             ),
             
             
             tabPanel("NCBI",          #NCBI Tab    
                      # Application title
                      
                      tabsetPanel(
                        id = "NCBIpage",
                        tabPanel("Start Your NCBI Search",
                                 # Application title
                                 column(8, align="center", offset = 2,
                                        titlePanel("Welcome to the NCBI Nucleotide Pipeline")
                                 ),
                                 column(8, align="justify", offset = 2,
                                   p("The NCBI Nucleotide pipeline of RSB takes in a list of organism(s) and barcode-gene(s) of interest and then directly queries the Nucleotide database
                                     using the Rentrez package to find how many records match the search. To further tailor the search, users are able to:"),
                                   tags$ol(
                                     tags$li("Set minimum and maximum sequence lengths parameters"),
                                     tags$li("Utilize the package Taxize to append synonyms and correct spelling mistakes of organism names"),
                                     tags$li("Choose to search for results either within all fields or specifically in the organism and/or gene metadata fields of NCBI data.")
                                   ),
                                   p("The tool then showcases a Coverage Matrix (CM), described on the RSB home page, a statistical summary of the CM, and the search statements used to query
                                     NCBI Nucleotide. Users are able to download the results showcased and FASTA and GenBank files up to a preset limit. Greater detail for the options and displays 
                                     are provided in the relevant portions of the pipeline.")
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
                                   ),
                                 )),
                                 
                        ),
                        tabPanel("Organism Names",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          titlePanel("Organism Names"),
                                          textAreaInput(inputId = "NCBIorganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                                          checkboxInput(inputId = "NCBItaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package Taxize", tags$a(id="NCBItaxizeHelp",icon("question-circle")) ), value = TRUE, width = 500),
                                          bsPopover(id="NCBItaxizeHelp", title="Help", content = paste0(
                                            '<p>If this box is checked, the programming package <i>Taxize</i> will:</p>',
                                            '<ol>',
                                            '<li><p>Spellcheck each of your organisms’ names before searching the NCBI database.</p></li>',
                                            '<li><p>Check if you have the most up-to-date organism names, and replaces your search term if not.</p></li>',
                                            '<li><p>Add synonyms for the organism(s) listed to assist in finding more entries. Example: <i>Homo sapiens</i> with the <b>Check spelling and synonyms for organism names</b> box checked will search both &#39;<i>Homo sapiens</i>&#39; and &#39;<i>Homo sapiens subsp. varitus</i>&#39;.</p></li>', # &#39; is HTML for ' (single apostrophe)
                                            '</ol>',
                                            '<p>Note: for a full list of the data sources that <i>Taxize</i> references for proper nomenclature, see the <i>Taxize</i> GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'
                                          ),
                                          placement = "right",
                                          options = list(container = "body"),
                                          trigger="hover"),
                                          checkboxInput(inputId = "NCBISearchOptionOrgn", label = "Search by the [ORGN] Metadata field", value = TRUE, width = 500),
                                          actionButton(inputId = "BarcodesNext", label = "Manually enter & adjust barcodes of interest inputs"),
                                   )),
                        ),
                        tabPanel("Barcodes of Interest",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
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
                                          actionButton(inputId = "barcodeOptiontrnl", label = "trnl"),
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
                                   )),
                        ),
                        tabPanel("One last step!",
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          titlePanel("Will you be downloading sequences?"),
                                          p("If you are interested in downloading FASTA or Genbank files from the results, you must pre-specificy the number of sequences to download per organism-barcode combination."),
                                          p("If you will not be downloading any sequences, then you can simply move on to the next step"),
                                          numericInput("downloadNum", "Number of Sequences to Download per Cell:", 5, min = 1, max = 500),
                                          actionButton("NCBIsearchButton", "Search"),
                                   )
                                 )
                        ),
                        tabPanel("Summary Results",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          titlePanel("Summary of Search Results"),
                                          p("For each barcode we display the total number of entries found and the number of organisms with at least one or no sequence"),
                                          tags$div(id="loaderWrapperNCBI"),
                                          DT::dataTableOutput("NCBISummaryResults") %>% withSpinner(color="#0dc5c1"),
                                          conditionalPanel( condition = "output.NCBISummaryResults",
                                                            downloadButton("NCBIfileDownloadSD","Download summary data"),
                                                            actionButton("NCBIdetailsButton", "See More Detailed Results"),
                                                            actionButton("NCBIStartOverSummary", "Start the Search again"))
                                   )),
                        ),
                        tabPanel("Coverage Matrix",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution and the corresponding buttons
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          titlePanel("Number of Sequences Found Per Organism-Barcode Pairing"),
                                          DT::dataTableOutput("NCBIcoverageResults") %>% withSpinner(color="#0dc5c1"),
                                          conditionalPanel( condition = "output.NCBIcoverageResults",
                                                            downloadButton('download',"Download counts table"),
                                                            downloadButton("fileDownloadF","Download FASTA files"),
                                                            downloadButton("fileDownloadG","Download Genbank files"),
                                                            actionButton("NCBIStartOver", "Start the Search again")
                                                            )
                                   )),
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          titlePanel("An Example of the Search Queries Sent to NCBI"),
                                          p("Below we show an example of some of the queries we are sending to NCBI Nucleotide. If you wish to check the validity of our results, you can go to the NCBI website and paste these queries into their online search tool and see if you get the same results. If you want to see all of the queries used in this search, you can download them using the button located below."),
                                          DT::dataTableOutput("NCBIsearchQueries") %>% withSpinner(color="#0dc5c1"),
                                          downloadButton('downloadStatements',"Download search terms table"),
                                          p())
                                 )
                                 
                        ),
                        tabPanel("Information", 
                                 titlePanel("Find NCBI records of your organisms and barcodes of interest"),
                                 # embed_youtube("DNS7i2m4sB0", width = 560, height = 315, frameborder= 0, allowfullscreen = TRUE),
                                 fluidRow(
                                   mainPanel(
                                     h4("What does the tool do?"),
                                     p("The ‘NCBI Nucleotide Coverage Matrix’ was designed to screen the Nucleotide database for genetic barcode coverage prior to environmental DNA metabarcoding studies. Before conducting a metabarcoding study, scientists need to be aware of which organisms have reference sequences at known genetic barcoding loci. The tool finds out if the Nucleotide database contains sequences labeled with a specific gene and organism name. Numerous searches can be done in parallel instead of manually searching for each organism-gene combination on the NCBI Nucleotide website."),
                                     p("The ‘NCBI Nucleotide Coverage Matrix’ tool takes in a list of organisms and genes of interest and then queries the Nucleotide database to find how many records match the search. The tool then produces a table where the organism names are rows, gene names are columns, and each intersection of a row and column shows how many records are in the NCBI Nucleotide database. All of the search options are detailed in the ‘Search fields' section below. The power and flexibility of this tool allows scientists to check the NCBI Nucleotide database for genetic coverage in ways that aren’t possible without knowledge of the NCBI Entrez coding package."),
                                     h4("User Guide"),
                                     tags$a(href="https://docs.google.com/document/d/1-VbO7nzPHY27xDZ714Kzu8xemG2Hsg-Gu5iE8-Da5C8/edit?usp=sharing",
                                            icon("question-circle"), #icon from fa v4, please change to circle-question if you update fa version to latest
                                            "Click this link to read the user guide"),
                                     
                                     h4("Limitations:"),
                                     p("This tool may not find all possible entries that the user desires. Some limitations of this text based search include, but are not limited to:"),
                                     p("1) Alternative names of the listed gene in NCBI Nucleotide database",  HTML("<br/>"), "2) Incorrect or missing metadata", HTML("<br/>"), "3) Full genomes entries with unlabeled individual genes"),#The list
                                     p("Searching by Blast or in Silico PCR  are more empirical ways of identifying quality reference sequences, but are not implemented here. In addition, This tool only searches against the NCBI Nucleotide database, and thus doesn’t include other genetic databases such as BOLD, Silva, Fishbase, etc."),
                                   )
                                 )
                        )
                      ),
             ),
             
             
             tabPanel("Full Genome Search",
                      tabsetPanel(
                        id = "FullGenomePage",
                        tabPanel("Start Your Full Genome Search",
                                 # Application title
                                 titlePanel("Find genome of interest"),
                                 h4("Descriptions of each Search Field"),
                                 dropdown(label="Organisms List (Text box)", p("A comma separated list of the names for your organism(s) of interest. All taxonomic ranks apply.")),
                                 dropdown(label="Check spelling and synonyms for organism names (Check box)", p("If this box is checked, the programing package ‘Taxize’ will: "), p("1) Spell check each of your organisms' names before searching the NCBI database",  HTML("<br/>"), "2) Check if you have the most up to date organism names, and replaces your search term if not", HTML("<br/>"), "3) Add synonyms for the organism(s) listed to assist in finding more entries. Example: Homo sapien with the 'Check spelling and synonyms for organism names' box checked will search both ‘Homo sapien’ and ‘Homo sapien varitus’"), p("Note: for a full list of the data sources that Taxize references for proper nomenclature, see the Taxize github here: https://github.com/ropensci/taxize")),
                                 p(""),
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          selectInput("gsearch", "Choose which genome to search for:", 
                                                      choices = c("Full mitochondrial genomes in NCBI Nucleotide", "Full chloroplast genomes in NCBI Nucleotide", "Number of entries per taxa in NCBI Genome"), width=500),
                                          fileInput("uploadGenomeFile", "Choose CSV file to upload", accept = c(".csv"), width = 800),
                                          #actionButton(inputId = "uploadCRUXButton", label = "Upload file to textboxes"),
                                          actionButton("FullGenomeStart", "Manually enter & adjust Organism name inputs"),
                                   )),
                                 
                        ),
                        tabPanel("Organism Names",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          titlePanel("Organism Names"),
                                          textAreaInput(inputId = "genomeOrganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                                          checkboxInput(inputId = "refSeq", label = "Search for reference sequences", value = TRUE),
                                          checkboxInput(inputId = "fullGenomeTaxizeOption", label = "Append organism name synonyms and spelling corrections via the R Package Taxize", value = TRUE),
                                          actionButton("genomeSearchButton", "Search"),
                                   )),
                        ),
                        tabPanel("Results",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution and the corresponding buttons
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          titlePanel("Number of Sequences Found per Organism"),
                                          tags$div(id="loaderWrapperFullGenome"),
                                          DT::dataTableOutput("genomeResults") %>% withSpinner(color="#0dc5c1"),
                                          conditionalPanel( condition = "output.genomeResults",                            
                                                            downloadButton('fullGenomeDownloadT',"Download table"),
                                                            downloadButton('fullGenomeDownloadF', "Download Fasta files"),
                                                            downloadButton('fullGenomeDownloadG', "Download Genbank files"))
                                          #actionButton("FullGenomeSummaryDataButton", "Check Summary Data")) # TO BE USED OEN DAY?
                                   )),
                                 
                        ),
                        tabPanel("Information"),
                        h4("User Guide"),
                        full_genome_guide.icon <- tags$a(href='https://docs.google.com/document/d/1Z9qLSy1ZiHaoT2i6rC_CLM_mKbo4uN2zLxaNbkz_w7U/edit?usp=sharing',
                                                         icon("question-circle"),
                                                         "Full Genome User Guide", target="_blank")
                      )),
             
             tabPanel("BOLD",
                      tabsetPanel(
                        id = "BOLDpage",
                        tabPanel("Start Your BOLD Search",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 
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
                                              and graphs to help users analyze the data and fine tune their filters.")
                                           
                                   )
                                 ),
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
                                          ),
                                   ),
                                 ),
                        ),
                        tabPanel("Organism Names",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 fluidRow(
                                   column(6, align="center", offset = 3,
                                          div(
                                            titlePanel("Organism Names"),
                                            style = "padding-top: 20px; padding-bottom: 10px;"
                                          ),
                                          textAreaInput(inputId = "BOLDorganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                                          checkboxInput(inputId = "BOLDtaxizeOption", label = list("Append organism name synonyms and spelling corrections via the R Package Taxize", tags$a(id="BOLDtaxizeHelp",icon("question-circle")) ), value = TRUE, width = 500),
                                          bsPopover(id="BOLDtaxizeHelp", title="Help", content = paste0(
                                            '<p>If this box is checked, the programming package <i>Taxize</i> will:</p>',
                                            '<ol>',
                                            '<li><p>Spellcheck each of your organisms’ names before searching the NCBI database.</p></li>',
                                            '<li><p>Check if you have the most up-to-date organism names, and replaces your search term if not.</p></li>',
                                            '<li><p>Add synonyms for the organism(s) listed to assist in finding more entries. Example: <i>Homo sapiens</i> with the <b>Check spelling and synonyms for organism names</b> box checked will search both &#39;<i>Homo sapiens</i>&#39; and &#39;<i>Homo sapiens subsp. varitus</i>&#39;.</p></li>', # &#39; is HTML for ' (single apostrophe)
                                            '</ol>',
                                            '<p>Note: for a full list of the data sources that <i>Taxize</i> references for proper nomenclature, see the <i>Taxize</i> GitHub repo <a href="https://github.com/ropensci/taxize" target="_blank">here</a>.</p>'
                                          ),
                                          placement = "right",
                                          options = list(container = "body"),
                                          trigger="hover"),
                                          actionButton("BOLDsearchButton", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                                   )),
                        ),
                        tabPanel("Species Not Found in BOLD Database",
                          column(12, align="center", offset=0,style='padding-top:15px',
                            titlePanel("Species not found in BOLD database"),
                            p("List of species that were not found when searching the BOLD database, if the species are not displayed in this table then they have results in the BOLD database. Additionally, the results in this table are not affected by the Country or NCBI filter"),
                            DT::dataTableOutput("BOLDNullSpecies") %>% withSpinner(color="#0dc5c1")
                        )),
                        tabPanel("Filters",
                                 fluidRow(
                                   # Padding at the top so it doesn't clash with tabs
                                   div(
                                     style = "padding-bottom: 20px;"
                                   ),
                                   # First column set with the dropdown country filtero
                                   column(6, align="center", offset = 3,
                                          uiOutput("selectCountry") %>% withSpinner(color="#0dc5c1"),
                                   )),
                                 
                                 # OLd left aligned  button leaving it here cause it may be useful for future reference
                                 # column(2, align="left", style='padding-top:110px',
                                 #        conditionalPanel(condition = "output.selectCountry",
                                 #                         actionButton('BOLDClearFilter',"Remove Countries From Filter")
                                 #        )),
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
                                                           # tags$style(".buttonClass button { transform: scale(1.2); margin-right: 60px;}"),
                                                           div(#class = "buttonClass",
                                                             actionButton('BOLDfilterCountries',"Apply Filters"),
                                                             actionButton('BOLDSkipFilter',"Skip Filters"),
                                                           ),
                                          ),
                                   ),
                                 ),
                        ),
                        tabPanel("Summary Data",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution and the corresponding buttons
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          #conditionalPanel(condition = "input.geo != list()",
                                          #DT::dataTableOutput("specificGeoResults") %>% withSpinner(color="#0dc5c1")),
                                          #mainPanel(plotOutput("geo_pie")),
                                          titlePanel("Summary of Search Results"),
                                          p("For each barcode we display the total number of entries found and the number of organisms with at least one or no sequence"),
                                          DT::dataTableOutput("BOLDSummaryData") %>% withSpinner(color="#0dc5c1"),
                                          conditionalPanel(condition = "output.BOLDSummaryData",
                                                           #actionButton("geoSearch", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                                                           downloadButton('downloadBoldSummary', "Download Summary Data Table"),
                                          )),
                                   
                                 ),
                        ),
                        tabPanel("Coverage Matrix",
                                 column(12, align="center", style='padding-top:15px',
                                        titlePanel("Sequences Found for each Species Classified by Barcode"),
                                        p("Total number of entries found for each species classified into the different barcodes found."),
                                        p("The different barcodes found in BOLD are ordered left to right from most to least results"),
                                        DT::dataTableOutput("BOLDcoverageResults") %>% withSpinner(color="#0dc5c1"),
                                        conditionalPanel(condition = "output.BOLDcoverageResults",
                                                         #actionButton("geoSearch", "Search", width = 100, style='vertical-align- middle; font-size:120%'),
                                                         downloadButton('downloadBoldFasta',"Download Fasta Files"),
                                                         downloadButton('downloadBoldMatrix', "Download Counts Table")
                                        )
                                  ),
                        ),
                        tabPanel("Country Data",
                                 # Application title
                                 # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
                                 # Show a plot of the generated distribution and the corresponding buttons
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          #conditionalPanel(condition = "input.geo != list()",
                                          #DT::dataTableOutput("specificGeoResults") %>% withSpinner(color="#0dc5c1")),
                                          #mainPanel(plotOutput("geo_pie")),
                                          titlePanel("Total number of barcodes found by country for each unique species"),
                                          DT::dataTableOutput("BOLDPresentTable") %>% withSpinner(color="#0dc5c1"),
                                          downloadButton('downloadBoldPresent', "Download entries per country table"),
                                          
                                          titlePanel("Suggested countries to add to your filter"),
                                          p("For those species that have no barcodes found in the country(s) filtered we provide 
                                 the top 3 unselected countries with the most sequence results."),
                                          DT::dataTableOutput("BOLDAbsentTable") %>% withSpinner(color="#0dc5c1"),
                                          downloadButton('downloadBoldAbsent', "Download suggested country filters table")
                                   ),
                                   
                                 ),
                        ),
                        tabPanel("Plot Total Sequences Per Country",
                                 fluidRow(
                                   column(10, align="center", style='padding-top:15px',
                                          mainPanel(plotOutput('treemap')),
                                          # downloadButton('downloadTreeGraph',"Download Graph"),
                                          
                                   ),
                                   column(2, align="center", style='padding-top:15px',
                                          downloadButton('downloadTreeGraph',"Download Graph"),
                                   ),
                                 ),
                        ),
                        tabPanel("Plot Unique Species Per Country",
                                 fluidRow(
                                   column(12, align="center", style='padding-top:15px',
                                          mainPanel(plotOutput('species')),
                                          downloadButton('downloadBarGraph',"Download Graph"))
                                   
                                 ),
                                 #   column(2, align="center", style='padding-top:15px', 
                                 #     downloadButton('downloadBarGraph',"Download Graph"))
                                 # ),
                        ),
                        tabPanel("Manual Data Processing Required",
                                 column(12, align="center", offset=0,style='padding-top:15px', 
                                        conditionalPanel(condition = "output.selectCountry", 
                                                         titlePanel("Bold UIDs for Entries With Unlabeled Barcodes"),
                                                         p("For those entries in BOLD that have unlabeled barcodes, we provide their respective BOLD UIDs 
                                                so that scientists may explore these results further."),
                                                         DT::dataTableOutput("BOLDNATable") %>% withSpinner(color="#0dc5c1"),
                                                         downloadButton('downloadBoldNaBarcodes', "Download NA Barcodes table")
                                        ),
                                 )),
                      )),
             
             
             tabPanel("Contact Us", 
                      
                      titlePanel("Contact us"),
                      p(HTML('&emsp;'), "This app was developed by the BlueWaltzBio team." ),
                      p(HTML('&emsp;'), "Learn more about us on our website BlueWaltzBio.com or direct message us on twitter @BlueWaltzBio" ),
                      p(HTML('&emsp;'), "If you want to provide feedback please use the google link: ", a("https://forms.gle/ysT6g8sk1zxWQ1wZA", href="https://forms.gle/ysT6g8sk1zxWQ1wZA")),
                      p(HTML('&emsp;'), "The app was iteratively built with direct feedback from eDNA scientists who spoke with members of BlueWaltzBio. In total our team has interviewed over 70 lab techs, professors, government regulators, and investors in the field of Environmental DNA. 
            " ),
                      twitter.icon <- tags$a(href='https://twitter.com/?lang=en',
                                             icon("twitter"),
                                             'Twitter', target="_blank")
                      
             )
             
  )
))
