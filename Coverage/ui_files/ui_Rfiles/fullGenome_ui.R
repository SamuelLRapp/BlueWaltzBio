tabsetPanel(
  id = "FullGenomePage",
  
  # Full Genome Start Tab
  tabPanel("Start Your Full Genome Search",
           # Application title
           titlePanel("Find genome of interest"),
           h4("Descriptions of each Search Field"),
           dropdown(label="Organisms List (Text box)", p("A comma separated list of the names for your organism(s) of interest. All taxonomic ranks apply.")),
           dropdown(label="Check spelling and synonyms for organism names (Check box)", p("If this box is checked, the programing package “Taxize” will: "), p("1) Spell check each of your organisms' names before searching the NCBI database",  HTML("<br/>"), "2) Check if you have the most up to date organism names, and replaces your search term if not", HTML("<br/>"), "3) Add synonyms for the organism(s) listed to assist in finding more entries. Example: Homo sapien with the 'Check spelling and synonyms for organism names' box checked will search both ‘Homo sapien’ and ‘Homo sapien varitus’"), p("Note: for a full list of the data sources that “Taxize” references for proper nomenclature, see the “Taxize” github here: https://github.com/ropensci/taxize")),
           p(""),
           fluidRow(
             column(6, align="center", offset = 3,
                    selectInput("gsearch", "Choose which genome to search for:", 
                                choices = c("Full mitochondrial genomes in NCBI Nucleotide", "Full chloroplast genomes in NCBI Nucleotide", "Number of entries per taxa in NCBI Genome"), width=500),
                    fileInput("uploadGenomeFile", "Choose CSV file to upload", accept = c(".csv"), width = 800),
                    #actionButton(inputId = "uploadCRUXButton", label = "Upload file to textboxes"),
                    actionButton("FullGenomeStart", "Manually enter & adjust Organism name inputs"),
             ))),
  
  
  # Full Genome Organisms Search Tab
  tabPanel("Organism Names",
           # Application title
           # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
           fluidRow(
             column(6, align="center", offset = 3,
                    titlePanel("Organism Names"),
                    textAreaInput(inputId = "genomeOrganismList", label = "A comma separated list of the names for your organism(s) of interest. All taxonomic ranks (family, genus, species-genus, etc) are searchable", width = 500, height = 200),
                    checkboxInput(inputId = "refSeq", label = "Search for reference sequences", value = TRUE),
                    checkboxInput(inputId = "fullGenomeTaxizeOption", label = "Append organism name synonyms and spelling corrections via the R Package “Taxize”", value = TRUE),
                    actionButton("genomeSearchButton", "Search"),
             ))),
  
  
  # Full Genome Results Tab
  tabPanel("Results",
           # Application title
           # img(src = "https://media.giphy.com/media/rGlAZysKBcjRCkAX7S/giphy.gif", align = "left",height='250px',width='500px'),
           # Show a plot of the generated distribution and the corresponding buttons
           fluidRow(
             column(12, align="center", style='padding-top:15px',
                    conditionalPanel( condition = "output.genomeResults",
                                      titlePanel("Number of Sequences Found per Organism"),
                    ),
                    tags$div(id="loaderWrapperFullGenome"),
                    DT::dataTableOutput("genomeResults"),
                    conditionalPanel( condition = "output.genomeResults",                            
                                      downloadButton('fullGenomeDownloadT',"Download Coverage Matrix"),
                                      downloadButton('fullGenomeDownloadF', "Download Fasta Files"),
                                      downloadButton('fullGenomeDownloadG', "Download Genbank Files"))
             ))),
  
  
  # Full Genome Information Tab
  tabPanel("Information",
           h4("User Guide"),
           full_genome_guide.icon <- tags$a(href='https://docs.google.com/document/d/1Z9qLSy1ZiHaoT2i6rC_CLM_mKbo4uN2zLxaNbkz_w7U/edit?usp=sharing',
                                            icon("question-circle"),
                                            "Full Genome User Guide", target="_blank")
           ))