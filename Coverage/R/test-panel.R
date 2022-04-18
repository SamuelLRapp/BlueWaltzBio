#ignore this please

testPanelUI <- function(id){
  ns <- NS(id)
  mainPanel(
    div(style="padding: 1rem;background:hsl(40, 90%, 60%)",
      h4(
        "***Buttons for testing caching***",
      ),
      
      div(class="flex-btn-grid test-panel",
          actionButton(ns("tab-switcher"), "js tester"),
          actionButton(ns("get-data-test"), "just get data (no download)", style = "border: 1px dashed #555"),
          actionButton(ns("cache-log"), "log cache file paths", style = "border: 1px dashed #555"),
          actionButton(ns("mt-file"), "empty file cache", class = "btn-danger"),
          actionButton(ns("mt-entire"), "empty entire cache", class = "btn-danger"),
          downloadButton(ns("ncbi-dwn"),""),
          actionButton(inputId=ns("ncbi-dwn-trigger"), label=list(icon("download"), "download FASTA files"), class = "btn-primary")
      )
    )
  )
}

testPanelServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      
    }
  )    
}