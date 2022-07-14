library(shiny)
library(shinyjs)
cacheTestUI <- function(id) {

  #ns is a function that returns a namespaced/scoped id to prevent conflicts with elements sharing the same id name
  ns <- NS(id)
  mainPanel(
    h4(
      "***Buttons for testing caching***",
    ),
   div(class="flex-btn-grid test-panel", style="padding: 1rem;background:hsl(40, 90%, 60%)",
      actionButton(ns("get-data-test"), "just get data (no download)", style = "border: 1px dashed #555"),
      actionButton(ns("cache-log"), "log cache file paths", style = "border: 1px dashed #555"),
      actionButton(ns("mt-file"), "empty file cache", class = "btn-danger"),
      actionButton(ns("mt-entire"), "empty entire cache", class = "btn-danger"),
      downloadButton(ns("ncbi-dwn"),"", class="hidden-dwn"),
      actionButton(inputId=ns("ncbi-dwn-trigger"), label=list(icon("download"), "download FASTA files"), class = "btn-primary")
    )
  )

}

#passing functions because too lazy to copy and paste everything
cacheTestServer <- function(id, uidsGet, getFileContent) {
  moduleServer(
    id,
    function(input, output, session) {
      getCachedCells <- function() {
        cells_dwn <- listCacheDir(subdir="files", recur=T, showdir = F)
        cell_list <- gregexpr("[0-9]+(?=[.])", cells_dwn, perl=T) #regex gets the number before the file extension
        cell_list = unlist(regmatches(cells_dwn, cell_list))
      }
      #RShiny automatically namespaces all ids in moduleServer
      observeEvent(input[["get-data-test"]], {
        shinyjs::disable("get-data-test") #disable button
        cache <- createCache("NCBI")
        if ( cache$exists("uids-matrix") ) {
          print("cache detected")
          uids_matrix <- cache$get("uids-matrix")
          total_cells <- nrow(uids_matrix)*ncol(uids_matrix)

          print("cached cell ids:")
          print(getCachedCells())
          uncached <- setdiff(1:total_cells, getCachedCells())
          print("uncached cell ids:")
          print(uncached)
          getFileContent(uids_matrix, callbck=function(){
            print("data retrieved! a cache was detected")
            print("just_get_data was clicked so no fasta file download will start")
            shinyjs::enable("get-data-test")
          }, uncached_cells = uncached)
        } else {
          print("no cache detected")
          
            uidsGet() %...>% {
              uidsMatrix <- .
              setCache("uids-matrix", uidsMatrix, "NCBI")

              getFileContent(uidsMatrix, callbck = function(){
                print("data retrieved! no cache was detected")
                print("just_get_data was clicked so no fasta file download will start")
                shinyjs::enable("get-data-test") #re-enable button
              })
            }
          
        }
      })
      
      observeEvent(input[["cache-log"]], {
        cachelog <- listCacheDir(subdir="", recur=T, showdir = F)
        if (length(cachelog) > 0){
          print(cachelog)
        } else {
          print("cache empty")
        }
      })
      
      observeEvent(input[["mt-file"]], {
        emptyCache("NCBI/files")
        print("file cache emptied")
      })
      
      observeEvent(input[["mt-entire"]], {
        emptyCache("NCBI")
        print("entire cache emptied")
      })
      
      output[["ncbi-dwn"]] <- downloadHandler(
        filename <- function() {
          paste("NCBI_Fasta", "zip", sep=".")
        },
        content <- function(downloadedFile) {
          cache <- createCache("NCBI")
          cached_matrix <- cache$get("uids-matrix")
          barcodes <- colnames(cached_matrix)
          future_promise({
            # create temp file directory to hold files
            tempDir <- tempdir()
            # enter temp file dir
            setwd(tempDir) #program auto returns to home dir on exit
            
            #create separate fasta file for each barcode
            # sapply returns vector of function results
            filenames <- c()
            for (code in barcodes){
              filename <- paste("NCBI", code, "sequence", sep = "_")
              file_path <- paste0(filename, ".fasta")
              file.create(file_path)
              filenames <- c(filenames, file_path)
            }
            
            #write cache info to files
            lapply(1:length(barcodes), function(codeNum){
              cache_name <- paste("NCBI","files",codeNum,sep="/")
              file_cache <- createCache(cache_name)
              content_vec <- sapply(file_cache$keys(), function(key){
                getCache(key, cache_name)
              })
              #write sequences to appropriate file
              write(content_vec, file=filenames[[codeNum]], append=T)
            })
            emptyCache("NCBI")
            zip(zipfile = downloadedFile, files = filenames) #output zip just contains the fasta files
          })
        },
        contentType = "application/zip"
      )
      
      observeEvent(input[["ncbi-dwn-trigger"]], {
        shinyjs::disable("ncbi-dwn-trigger")
        #get the cache for NCBI stuff
        cache <- createCache("NCBI")
        if ( cache$exists("uids-matrix") ) {
          print("cache detected")
          uids_matrix <- cache$get("uids-matrix")
          total_cells <- nrow(uids_matrix)*ncol(uids_matrix) #integer vector with the ids of every cached cell
          
          print("cached cells:")
          print(getCachedCells())
          uncached <- setdiff(1:total_cells, getCachedCells())
          print("uncached cells:")
          print(uncached)
          content_callback <- function(){
            print("starting fasta file download...")
            js$clickBtn("ncbi-dwn")
            shinyjs::enable("ncbi-dwn-trigger")
          }
          getFileContent(uids_matrix, content_callback, uncached_cells = uncached)
        } else {
          print("no cache detected")
          
          uidsGet() %...>% {
            uidsMatrix <- .
            setCache("uids-matrix", uidsMatrix, "NCBI")
            content_callback <- function(){
              print("starting fasta file download...")
              #need to namespace this id since RShiny won't
              js$clickBtn(paste(id,"ncbi-dwn",sep="-"))
              shinyjs::enable("ncbi-dwn-trigger")
            }
            
            getFileContent(uidsMatrix, content_callback) 
          }
        }
      })
      
      # for auto-filling inputs during testing
      autofillInputs <- function(){
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "NCBIorganismList",
          value = "gallus gallus, cygnus, bos taurus"
        )
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "barcodeList",
          value = "trnl, (CO1; COI; COX1)"
        )
      }
      autofillInputs()
      
    }
  )
}