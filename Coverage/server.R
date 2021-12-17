#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#;lkasdjf;aldf


# Imports ------------------------------------------------------------------

library(shiny)
library(rentrez)
library(taxize)
library(tidyverse)
library(dplyr)
library(RSQLite)
library(rlist)
library(future)
library(promises)
library(ipc)
library(mpoly)

plan(multisession)
shinyServer(function(input, output, session) {

# Full Genome -------------------------------------------------------------------  

# * FullGenomeSearchButton --------------------------------------------------------
  
  fullGenomeSearchButton <- eventReactive(input$genomeSearchButton, { 
    #When searchButton clicked, update fGenOrgSearch to return the value input into genomeOrganismList 
    input$genomeOrganismList #Returns as a string
  })
  
  # * FullGenomeInputCSV -----------------------------------------------------------
  
  inputFileCrux <- observeEvent(input$uploadGenomeButton,{
    isolate({
      req(input$uploadGenomeFile, file.exists(input$uploadGenomeFile$datapath))
      uploadinfo <- read.csv(input$uploadGenomeFile$datapath, header = TRUE)
      if(input$genomeOrganismList[[1]] != "") {
        updateTextAreaInput(getDefaultReactiveDomain(), "genomeOrganismList", value = c(head(uploadinfo$OrganismNames, -1), input$genomeOrganismList))
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(), "genomeOrganismList", value = uploadinfo$OrganismNames)
      }
    })
  })
  

# * FGenOrgSearch -----------------------------------------------------------------
  
   fGenOrgSearch <- reactive({
    orgString <- fullGenomeSearchButton()
    fullGenomeTaxizeOption <- input$fullGenomeTaxizeOption
    future_promise({
    genomeOrgList <- strsplit(orgString, ",")[[1]]
    genomeOrgList <- unique(genomeOrgList[genomeOrgList != ""])
    if(fullGenomeTaxizeOption){ #if the taxize option is selected
      taxizeGenOrgList <- c() #initialize an empty vector

      for(i in 1:length(genomeOrgList))
      {
        err <- 1
        organism <- trimws(genomeOrgList[[i]], "b") #trim both leading and trailing whitespace
        while(err == 1) {
          NCBI_names <- tryCatch({
            Sys.sleep(0.34)
            NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #help user with various naming issues (spelling, synonyms, etc.)
            err <- 0
            NCBI_names
          }, error = function(err) {
            err <<- 1
          })
        }
        row_count <- nrow(NCBI_names) # get number of rows in dataframe

        if(row_count > 0) #If a legitimate name was found
        {
          for(j in 1:row_count)
          {
            taxa_name <- NCBI_names[[j,3]] #Store each matched name in taxa_name
            taxizeGenOrgList <- c(taxizeGenOrgList, taxa_name) #update the vector with all the taxa_names.
          }
        }
        else
        {
          taxizeGenOrgList <- c(taxizeGenOrgList, organism) #just append organism to the list, and return taxizeGenOrgList
        }
      }
      taxizeGenOrgList
    } else{
      genomeOrgList #return the list as is
    }
    })
  })
  
# * Mitochondrial Search -------------------------------------------------------
   
  Organisms_with_Mitochondrial_genomes <- reactive({
    fGenOrgSearch() %...>% {
      genomeList <- .
      num_rows <- length(genomeList)
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
      uids <- c()
  
      parameters <- "set vector up"

      if(isTRUE(input$refSeq))
      {
        parameters <- " AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN] AND srcdb_refseq[PROP]"
        names(Results) <- c('Num_RefSeq_Mitochondrial_Genomes_in_NCBI_Nucleotide','SearchStatements')
      }
      else
      {
        parameters <- " AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN]"
        names(Results) <- c('Num_Mitochondrial_Genomes_in_NCBI_Nucleotide','SearchStatements')
      }
    
      future_promise({
      for(i in 1:num_rows)
      {
        Mitochondrial_genome_SearchTerm <- paste0('', genomeList[i],'[ORGN]',parameters,'')
        searchResult <- tryCatch({
          Sys.sleep(0.34)
          genome_result <- entrez_search(db = "nucleotide", term = Mitochondrial_genome_SearchTerm, retmax = 5)
          Results[i,1] <- genome_result$count
          Results[i,2] <- Mitochondrial_genome_SearchTerm
          for(id in genome_result$ids){
            uids <- c(uids, id)
          }
        }, error = function(err) {
          Results[i,1] <<- "Error"
          Results[i,2] <<- "Error"
        })
      }
      list(Results, uids)
      })
    }
  })

# * Chloroplast Search ---------------------------------------------------------
  
  Organisms_with_Chloroplast_genomes <- reactive({
    
    fGenOrgSearch() %...>% {
      num_rows <- length(.)
      genomeList <- .
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
      uids <- c()
    
      parameters <- "set vector up"
    
      if(isTRUE(input$refSeq))
      {
        parameters <- " AND Chloroplast[TITL] AND 120000:170000[SLEN] AND srcdb_refseq[PROP]"
        names(Results) <- c('Num_RefSeq_Chloroplast_Genomes_in_NCBI_Nucleotide','SearchStatements')
      }else
      {
        parameters <- " AND Chloroplast[TITL] AND 120000:170000[SLEN]"
        names(Results) <- c('Num_Chloroplast_Genomes_in_NCBI_Nucleotide','Chloroplast_SearchStatements')
      }
    
      future_promise({
      for(i in 1:num_rows)
      {
        Chloroplast_genome_SearchTerm <- paste0('',genomeList[i],'[ORGN]',parameters,'')
        searchResult <- tryCatch({
          Sys.sleep(0.34)
          genome_result <- entrez_search(db = "nucleotide", term = Chloroplast_genome_SearchTerm, retmax = 5)
          Results[i,1] <- genome_result$count 
          Results[i,2] <- Chloroplast_genome_SearchTerm
          for(id in genome_result$ids){
            uids <- c(uids, id)
          }
        }, error = function(err) {
          Results[i,1] <<- "Error"
          Results[i,2] <<- "Error"
        })
      }
      list(Results, uids)
      })
    }
  })
  
# * Is_the_taxa_in_the_NCBI_genome_DB ------------------------------------------
  
  Is_the_taxa_in_the_NCBI_genome_DB <- reactive ({
    
    fGenOrgSearch() %...>% {
      num_rows <- length(.)
      genomeList <- .
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
      uids <- c()
      
      names(Results) <- c('present_in_NCBI_Genome','GenomeDB_SearchStatements')
      future_promise({
      for(i in 1:num_rows)
      {
        genome_SearchTerm <- paste0('', genomeList[i],'[ORGN]','')
        searchResult <- tryCatch({
          Sys.sleep(0.34)
          genome_result<- entrez_search(db = "genome", term = genome_SearchTerm, retmax = 5)
          Results[i, 1] <- genome_result$count #add zero
          Results[i, 2] <- genome_SearchTerm 
          for(id in genome_result$ids){
            uids <- c(uids, id)
          }
        }, error = function(err) {
          Results[i,1] <<- "Error"
          Results[i,2] <<- "Error"
        })
      }
      list(Results, uids)
      })
    }
  })
  
#  * selectFunction  --------------------------------------------------------------

 selectfunction <- reactive({
  if (input$gsearch == "Full mitochondrial genomes in NCBI Nucleotide")
  {
    Organisms_with_Mitochondrial_genomes() %...>% {
      genomes <- .
      genomes
    }
  }
  else if (input$gsearch == "Full chloroplast genomes in NCBI Nucleotide") 
  {
    Organisms_with_Chloroplast_genomes() %...>% {
      genomes <- .
      genomes
    }
  }
  else if (input$gsearch == "Number of entries per taxa in NCBI Genome") 
  {
    Is_the_taxa_in_the_NCBI_genome_DB() %...>% {
      genomes <- .
      genomes
    }
  }
 })

 # * Output Table render ----------------------------------------------------------
  output$genomeResults <- DT::renderDataTable({
    # selectfunction()[[1]], rownames = fGenOrgSearch(), colnames = names(selectfunction()[[1]]) 
    promise_all(data_df = selectfunction(), rows = fGenOrgSearch()) %...>% with({
      DT::datatable(data_df[[1]], rownames = rows, colnames = names(data_df[[1]]))
    })
  })
  
 # * Download Fastas ---------------------------------------------------------------
  
  # Download Full Genome table
  output$fullGenomeDownloadF <- downloadHandler(
    filename = function() { # Create the file and set its name
      paste("Full_Genome_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        uids <- .[[2]]
        progLength <- length(uids)
        progress <- AsyncProgress$new(session, min=0, max=progLength, message= "Downloading", value= 0)
        future_promise({
          Vector_Fasta <- c()
          for (uid in uids) {
            err <- 1
            while(err == 1){
              File <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
                Sys.sleep(0.34)
                File_fasta <- entrez_fetch(db = "nucleotide", id = uid, rettype = "fasta") # Get the fasta file with that uid
                err <- 0
              }, error = function(err) {
                err <<- 1
              })
            }
            Vector_Fasta <- c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
            progress$inc(amount=1)
          }
          write(Vector_Fasta, file) # Writes the vector containing all the fasta file information into one fasta file
          progress$set(value=progLength)
          progress$close()
        })
      }
    }
  )
  
  # * Download Genbank files --------------------------------------------------------
  
  output$fullGenomeDownloadG <- downloadHandler(
    filename = function() { # Create the file and set its name
      paste("Full_Genome_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        uids <- .[[2]]
        progLength <- length(uids)
        progress <- AsyncProgress$new(session, min=0, max=progLength, message= "Downloading", value= 0)
        future_promise({
          Vector_genbank <- c()
          for (uid in uids) {
            err <- 1
            while(err == 1){
              File <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
                Sys.sleep(0.34)
                File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "gb") # Get the genbank file with that uid
                err <- 0
              }, error = function(err) {
                err <<- 1
              })
            }
            Vector_genbank <- c(Vector_genbank, File_genbank) # Append the genbank file to a vector
            progress$inc(amount=1)
          }
          write(Vector_genbank, file) # Writes the vector containing all the genbank file information into one genbank file
          progress$set(value=progLength)
          progress$close()
        })
      }
    }
  )
  
  # * Download Full Genome Results Table ----------------------------------------
  
  output$fullGenomeDownloadT <- downloadHandler(
    filename = function() { # Create the file and set its name
      paste("Full_Genome_Table", ".csv", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        FullGenmatrix <- .[[1]] # Gets the matrix for the FullGenome search results
        fGenOrgSearch() %...>% {
          rownames(FullGenmatrix) <- . # Adds the row names to the matrix
          write.csv(FullGenmatrix, file) # Writes the dataframe to the CSV file
        }
      }
    }
  )

  
# CRUX ----------------------------------------------------------------------

# * CRUXSearchButton --------------------------------------------------------

    cruxOrgSearch <- eventReactive(input$searchButton, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList 
        input$CRUXorganismList #Returns as a string
    })

# * CRUXStrToList -----------------------------------------------------------
    
    cruxOrganismList <- reactive({ #Converts string from cruxOrgSearch into a list of Strings
      cruxOrgSearch <- cruxOrgSearch()
      CRUXtaxizeOption <- input$CRUXtaxizeOption
        future_promise({
        organismList <- strsplit(cruxOrgSearch[[1]], ",")[[1]] #separate based on commas
        organismList <- unique(organismList[organismList != ""])
        if(CRUXtaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector
            for(i in 1:length(organismList))
            {
                err <- 1
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                while(err == 1) {
                  NCBI_names <- tryCatch({
                    Sys.sleep(0.34) #sleeping for 1/3 of a second each time gives us 3 queries a second. If each user queries at this rate, we can service 4-8 at the same time.
                    NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #help user with various naming issues (spelling, synonyms, etc.)
                    err <- 0
                    NCBI_names
                  }, error = function(err) {
                    err <<- 1
                  })
                }

                row_count <- nrow(NCBI_names) # get number of rows in dataframe

                if(row_count > 0) #If a legitimate name was found
                {
                    for(j in 1:row_count)
                    {
                        taxa_name <- NCBI_names[[j,3]] #Store each matched name in taxa_name
                        taxize_organism_list <- c(taxize_organism_list, taxa_name) #update the vector with all the taxa_names.
                    }
                }
                else
                {
                    taxize_organism_list <- c(taxize_organism_list, organism) #just append organism to the list, and return taxize_organism_list
                }
            }
            taxize_organism_list
        } else{
            organismList #return the list as is
        }
        })
    })
    
# * CRUXSearch --------------------------------------------------------------------
    cruxSearch <- function(results, searchTerm, organism) {
      dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
      taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
      #results <- c()
        for(table in dbList){
          #
          location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=organism))
          if(nrow(location)==0){
            location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where genus= :x"), params=list(x=searchTerm[1,3]))
            if(nrow(location)==0){
              location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where familia= :x"), params=list(x=searchTerm[1,4]))
              if(nrow(location)==0){
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where ordo= :x"), params=list(x=searchTerm[1,5]))
                if(nrow(location)==0){
                  location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where classis= :x"), params=list(x=searchTerm[1,6]))
                  if(nrow(location)==0){
                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where phylum= :x"), params=list(x=searchTerm[1,7]))
                    if(nrow(location==0)){
                      location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x"), params=list(x=searchTerm[1,8]))
                      if(nrow(location==0)){results <- c(results, 0)} else {results <- c(results, "kingdom")}
                    } else{ results <- c(results, "phylum") }
                  } else { results <- c(results, "class")}
                } else {results <- c(results, "order")}
              } else {results <- c(results, "family")}
            }else {results <- c(results, "genus") }
          } else {results <- c(results, toString(nrow(location)))}
        }
        dbDisconnect(taxaDB)
        results
      }
    
# * CRUXCoverage ------------------------------------------------------------

    cruxCoverage <- reactive({
        
      cruxOrganismList() %...>% {
        organismList <- .
        organismListLength <- length(organismList)
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        
        searchTerm <- ""
        searchResult <- 0
        popuplist <- c()
        
        future_promise({
        errorPopupList <- c() # Error when trying to find if there are homonyms
        errorPopupListFound <- c() # Error when we know there are homonyms but we could not finish the search
        newOrgList <- c()
        err <- 0
        results <- c()
        search <- c()
        for(organism in organismList){
            errorHomonym <- 0
            search <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
              Sys.sleep(0.34)
              search <- get_uid_(sci_com = organism) # Check to see if there are homonyms
            }, error = function(err) {
              errorHomonym <<- 1
            })
            if(errorHomonym == 1){
              errorPopupList <- c(errorPopupList, organism)
            }
            else if(is.null(search[[1]])){
              results <- c(results, "0", "0", "0", "0", "0", "0", "0")
              newOrgList <- c(newOrgList, organism)
              next
            }
            if( errorHomonym != 1 && nrow(search[[1]]) > 1) {# There are homonyms
              popuplist <- c(popuplist, organism)
              # Process the 
              for (i in 1:nrow(search[[1]])) { # tax_name
                errorHomonym <- 0
                # if there are more than 5 homonyms then break we are not interested in more than 5
                if(i > 5) { 
                  break
                }
                # create new organism list since new organism are added
                newOrg <- paste(organism, search[[1]]$division[i], sep = " ")
                newOrgList <- c(newOrgList, newOrg)
                # Creating the same format as the other organisms so the Crux search can be performed correctly
                hierarchy <- tryCatch({ # Try catch for when we know there are homonyms but we dont know which homonyms yet, if there is an error fill up errorPopupListFound and activate the errorHomonym Flag
                  Sys.sleep(0.34)
                  hierarchy <- classification(search[[1]]$uid[i], db = "ncbi")[[1]] # Check to see if there are homonyms
                  hierarchy
                }, error = function(err) {
                  errorHomonym <<- 1
                })
                if(errorHomonym == 1) {
                  errorPopupListFound <- unique(c(errorPopupListFound, newOrg))
                  results <- c(results, "error", "error", "error", "error", "error", "error", "error")
                  next
                }
                match <- hierarchy$name[match(tolower(c("genus", "family", "order", "class","phylum", "domain")), tolower(hierarchy$rank))]
                query <- c("db", "query", "genus", "family", "order", "class","phylum", "domain")
                match <- c("ncbi", organism, match)
                searchTerm <- stats::setNames(
                  data.frame(t(match), stringsAsFactors = FALSE), 
                  query
                )
                # Perform the CruxSearch
                results <- cruxSearch(results, searchTerm, organism)
              }
            } else { # There are no homonyms
              newOrgList <- c(newOrgList, organism)
              searchTerm <- tryCatch({
                Sys.sleep(0.34)
                searchTerm <- tax_name(query= organism, get = c("genus", "family", "order", "class","phylum", "domain"), db= "ncbi", messages = FALSE)
                searchTerm
              }, error = function(err) {
                results <<- c(results, "error", "error", "error", "error", "error", "error", "error")
                err <<- 1
              })
              if(err == 1) {
                err <- 0
                next
              }
              results <- cruxSearch(results, searchTerm, organism)
            }
        }
        results <- list(organismList=newOrgList, data=results, popupinfo=popuplist, errorPopupList = errorPopupList, errorPopupListFound = errorPopupListFound) 
        results
        })
      }
    })
    
# * matrixGetCRUX ------------------------------------------------------------
    
    matrixGetCRUX <- reactive({ # creates and returns the matrix to be displayed with the count
      dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S")
      cruxCoverage() %...>% {
        cruxCoverage <- . # Get the results from the SQL queries
        results <- c()
        organismListLength <- length(cruxCoverage[[1]])
        for (i in cruxCoverage[[2]]) {
          results <- c(results, i)
        }
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE) #convert results vector to dataframe
        data
      }
    })
    
# * organismListGet ------------------------------------------------------------
    
    organismListGet <- reactive({ # Returns the uids stored in the results from the NCBi query
      organismList <- c()
      cruxCoverage() %...>% {
        cruxCoverage <- . # Get the results from the NCBI query
        for (i in cruxCoverage[[1]]) {
          organismList <- c(organismList, i)
        }
        # Send the alert to the user that we have found some homonyms
        cruxOrganismList() %...>% {
          if(length(organismList) > length(.)) {
            shinyalert("We have found Homonyms", cruxCoverage[[3]], type = "warning")
          }
          if(length(cruxCoverage[[4]]) > 0) {
            shinyalert("Homonyms for the following species could not be checked properly try again later", cruxCoverage[[4]], type = "error")
          }
          if(length(cruxCoverage[[5]]) > 0) {
            shinyalert("Homonyms for the following species were found but were not able to be processed correctly", cruxCoverage[[5]], type = "error")
          }
        }
        organismList
      }
    })
    

# * CRUXInputCSV -----------------------------------------------------------
    
    inputFileCrux <- observeEvent(input$uploadCRUXButton,{
        isolate({
            req(input$uCRUXfile, file.exists(input$uCRUXfile$datapath))
            uploadinfo <- read.csv(input$uCRUXfile$datapath, header = TRUE)
            if(input$CRUXorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = c(head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]), input$CRUXorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "CRUXorganismList", value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
            }
        })
    })
    

# * CRUXDownload ------------------------------------------------------------
    
    output$downloadCrux <- downloadHandler(
        filename = function() { # Create the file and set its name
          paste("CRUX_Table", ".csv", sep = "")
        },
        content = function(file) {
            columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") # Gets the column names for the matrix
            cruxOrganismList() %...>% {
              rows <- .
              matrixGetCRUX() %...>% { # Gets the matrix for the Crux results
                colnames(.) <- columns # Adds the column names to the matrix
                rownames(.) <- rows # Adds the row names to the matrix
                write.csv(., file) # Writes the matrix to the CSV file
              }
            }
        }
    )
    
# * CRUXSummaryReportDownload ------------------------------------------------------------    
    output$CRUXfileDownloadSD <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste("CRUX_Summary_Report", ".csv", sep = "")
      },
      content = function(file) {
        promise_all(data_df = summary_report(0)) %...>% with({
          write.csv(data_df, file) # Writes the dataframe to the CSV file
        })
      })

# * CRUXOutput --------------------------------------------------------------

    output$CRUXcoverageResults <- DT::renderDataTable({
      # matrixGetCRUX(), rownames = organismListGet(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      promise_all(data_df = matrixGetCRUX(), rows = organismListGet()) %...>% with({
        DT::datatable(data_df, rownames = rows, colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S"))
      })
    })
    

# NCBI --------------------------------------------------------------------


# * NCBISearchButton --------------------------------------------------------
    
    NCBISearch <- eventReactive(input$NCBIsearchButton, { #When searchButton clicked, update NCBIOrgSearch to return the value input into NCBIorganismList 
        list(input$NCBIorganismList, input$barcodeList) #Returns as a string
    })
    

# * NCBIStrToList -----------------------------------------------------------

    NCBIorganismList <- reactive({ #Converts string from NCBIorganismList into a list of Strings
      orgString <- NCBISearch()
      NCBItaxizeOption <- input$NCBItaxizeOption
        future_promise({
        organismList <- strsplit(orgString[[1]], ",")[[1]] #separate based on commas
        organismList <- unique(organismList[organismList != ""])
        if(NCBItaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector

            for(i in 1:length(organismList))
            {
                err <- 1
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                while(err == 1) {
                  NCBI_names <- tryCatch({
                    Sys.sleep(0.34) #sleeping for 1/3 of a second each time gives us 3 queries a second. If each user queries at this rate, we can service 4-8 at the same time.
                    NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
                    err <- 0
                    NCBI_names
                  }, error = function(err) {
                    err <<- 1
                  })
                }
                row_count <- nrow(NCBI_names)# get number of rows in dataframe

                if(row_count > 0)#If a legitimate name was found
                {
                    for(j in 1:row_count)
                    {
                        taxa_name <- NCBI_names[[j,3]] #Store each matched name in taxa_name
                        taxize_organism_list <- c(taxize_organism_list, taxa_name) #update the vector with all the taxa_names.
                    }
                }
                else
                {
                    taxize_organism_list <- c(taxize_organism_list, organism) #just append organism to the list, and return taxize_organism_list
                }
            }
            taxize_organism_list
        } else{
            organismList #return the list as is
        }
        })
    })
    

# * NCBIBarcodeList ---------------------------------------------------------

    barcodeList <- reactive({
        # Detect that there is a parenthesis then do not change and keep it together
        barcodeList <- strsplit(NCBISearch()[[2]], ",") #separate based on comma
        barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
        barcodeList[[1]] <- unique(barcodeList[[1]][barcodeList[[1]] != ""])
        barcodeList[[1]]
    })
    

# * NCBISequenceLength ------------------------------------------------------

    seqLenList <- reactive({ #list of sequence length specifications
        if(input$seqLengthOption){ #only present if the option is selected
          barcodeList <- strsplit(input$barcodeList, ",") #separate based on comma
          barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
          barcodeList[[1]] <- unique(barcodeList[[1]][barcodeList[[1]] != ""])
          
            textList <- list()
            for(marker in barcodeList[[1]]){ #allow the user to specify a different length for every barcode
                textList <- list(textList, numericRangeInput(inputId=marker, label=paste("Min/max sequence length for", marker), value=c(0,2000))) #add a numeric input
            }
            textList #return the list of numeric inputs
        }
    })
    

# * NCBICoverage ------------------------------------------------------------
    
    genBankCoverage <- reactive({
      
      NCBIorganismList() %...>% {
      organismList <- . #get species and barcode inputs
      organismListLength <- length(organismList)
      
      codeList <- barcodeList()
      codeListLength <- length(barcodeList()) 
      validate( #verify that the  user has typed things into both inputs
        need(organismListLength > 0, 'Please name at least one organism'),
        need(codeListLength > 0, 'Please choose at least one barcode')
      )
      searchTerm <- ""
      countResults <- list() #initialize empty vector
      uids <- list()
      searchResult <- 0
      
      #Temp vars for search options
      NCBISearchOptionGene <- input$NCBISearchOptionGene
      NCBISearchOptionOrgn <- input$NCBISearchOptionOrgn
      downloadNumber <- input$downloadNum
      seqLengthOption <- input$seqLengthOption
      seq_len_list <- list()
      for(code in codeList){
        seq_len_list[[code]] <- input[[code]]
      }
      
      future_promise({
      err <- 0              # These must be declared inside the promise, even though seemingly there is no reason
      countResults <- list() # why it should. However, if ever declared outside of the promise, everything breaks
      searchTerms <- list()   # so here they shall stay. I think it has something to do with the try-catch
      
      for(organism in organismList){
        for(code in codeList){
          # TODO: Add more sanitization to this
          # if there is a parenthesis
          code <- trimws(code)
          if(substring(code, 1,1) == "("){
            # code is in the format (loci1; loci2; loci3...)
            # We will make a combined query by substituting the ;s for other stuff
            
            # set up a replacement string
            replacement <- ""
            if(NCBISearchOptionGene){
              replacement <- "[GENE]"
            }
            # Add organism info 
            if(NCBISearchOptionOrgn){
              #our query to GenBank
              replacement <- paste(replacement, " AND ", organism, "[ORGN]", sep="") 
            }
            else {
              #our non-Metadata query to GenBank
              replacement <- paste(replacement, " AND ", organism, sep="") 
            }
            # Add sequence length info
            if(seqLengthOption){
              #if the user specified sequence length
              replacement <- paste(replacement, " AND ", seq_len_list[[code]][1],":", seq_len_list[[code]][2], "[SLEN]", sep="")
            }
            # Add the tail to the replacement string
            replacement <- paste(replacement, ") OR (", sep="")
            print(replacement)
            # Now we finally set searchTerm by replacing the ;s.
            searchTerm <- gsub(";", replacement, code)
            # But the last synonym won't have a ; after it! Sub in one last time:
            # trim last parenthesis
            searchTerm <- substring(searchTerm, 1, nchar(searchTerm)-1)
            # add in replacement string
            searchTerm <- paste(searchTerm, replacement, sep="")
            # cut off the " OR ("
            searchTerm <- substring(searchTerm, 1, nchar(searchTerm)-5)
            
          }else {
            print("Entering else block")
            if(NCBISearchOptionOrgn){
              searchTerm <- paste(organism, "[ORGN] AND ", sep="") #our query to GenBank
            }
            else {
              searchTerm <- paste(organism, " AND ", sep="") #our non-Metadata query to GenBank
            }
            print("past NCBISearchOptionOrgn")
            if(NCBISearchOptionGene) {
              searchTerm <- paste(searchTerm, code, "[GENE]", sep="") #our query to GenBank
            }
            else {
              searchTerm <- paste(searchTerm, code, sep="") #our query to GenBank
            }
            print("entering seqLenghtOption")
            if(seqLengthOption){
              searchTerm <- paste(searchTerm, " AND ", seq_len_list[[code]][1],":", seq_len_list[[code]][2], "[SLEN]", sep="") #if the user specified sequence length
            }
            print("Past seqLengthOption")
          }
          searchResult <- tryCatch({
            Sys.sleep(0.34)
            searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = downloadNumber) #only get back the number of search results
          }, error = function(err) {
            # results <- c(results, "error", "error", "error", "error", "error", "error", "error")
            countResults <<- list.append(countResults, "error")
            searchTerms <<- list.append(searchTerms, searchTerm)
            err <<- 1
          })
          if(err == 1) {
            err <- 0
            next
          } else {
            uids <- list.append(uids, searchResult$ids)
            searchTerms <- list.append(searchTerms, searchTerm) # 
            countResults <- list.append(countResults, searchResult$count) #append the count to the vector of results
          }
        }
      }
      results <- list(count=countResults, ids=uids,searchTermslist = searchTerms ) #
      results
      })
      }
    })
    

# * NCBIMatrix --------------------------------------------------------------
    
    matrixGet <- reactive({ # creates and returns the matrix to be displayed with the count
      NCBIorganismList() %...>% {
        organismList <- . #get species and barcode inputs
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        genBankCoverage() %...>% { # Get the results from the NCBI query
        count <- c()
        for (i in .[[1]]) {
            count <- c(count, i)
        }
        data <- matrix(count, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
        data
        }
      }
    })

# * NCBITableOutput ---------------------------------------------------------
    
    matrixGetSearchTerms <- reactive({ # creates and returns the matrix to be displayed with the count
      NCBIorganismList() %...>% {
        organismList <- . #get species and barcode inputs
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        genBankCoverage() %...>% { # Get the results from the NCBI query
        SearchStatements <- c()
        for (i in .[[3]]) { #3 is the 3rd list in genBankCovearage aka the searchterms list
          SearchStatements <- c(SearchStatements, i)
        }
        data <- matrix(SearchStatements, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
        data
        }
      }
    })
    

# *   NCBIGetIDs --------------------------------------------------------------

    uidsGet <- reactive({ # Returns the uids stored in the results from the NCBi query
        genBankCoverage() %...>% { # Get the results from the NCBI query
        uids <- c()
        for (i in .[[2]]) {
            uids <- c(uids, i)
        }
        uids
        }
    })
    

# * NCBIDownloadFASTA -------------------------------------------------------
  
    # Download Fasta Files
    output$fileDownloadF <- downloadHandler(
        filename = function() { # Create the file and set its name
          paste("NCBI_Fasta_File", ".fasta", sep = "")
        },
        content = function(file) {
          uidsGet() %...>% {
            progLength <- length(.)
            progress <- AsyncProgress$new(session, min=0, max=progLength, message="Downloading", value=0)
            future_promise({
                  Vector_Fasta <- c()
                  for (uid in .) {
                      err <- 1
                      while(err == 1){
                        File <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
                          Sys.sleep(0.34) #sleeping for 1/3 of a second each time gives us 3 queries a second. If each user queries at this rate, we can service 4-8 at the same time.
                          File_fasta <- entrez_fetch(db = "nucleotide", id = uid, rettype = "fasta") # Get the fasta file with that uid
                          err <- 0
                        }, error = function(err) {
                          err <<- 1
                        })
                      }
                      Vector_Fasta <- c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
                      progress$inc(amount=1)
                  }
                  write(Vector_Fasta, file) # Writes the vector containing all the fasta file information into one fasta file
                  progress$set(value=progLength)
                  progress$close()
              #})
            })
          }
        }
    )
    

# * NCBIDownloadGenbank -----------------------------------------------------

    # Download NCBI Genbank Files
    output$fileDownloadG <- downloadHandler(
        filename = function() { # Create the file and set its name
          paste("NCBI_Genbank_File", ".gb", sep = "")
        },
        content = function(file) {
            uidsGet() %...>% {
              progLength <- length(.)
              progress <- AsyncProgress$new(session, min=0, max=progLength, message="Downloading", value=0)
              future_promise({
                  Vector_genbank <- c()
                  for (uid in .) {
                      err <- 1
                      while(err == 1){
                        File <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
                          Sys.sleep(0.34) #sleeping for 1/3 of a second each time gives us 3 queries a second. If each user queries at this rate, we can service 4-8 at the same time.
                          File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "gb")  # Get the genbank file with that uid
                          err <- 0
                        }, error = function(err) {
                          err <<- 1
                        })
                      }
                      Vector_genbank <- c(Vector_genbank, File_genbank) # Append the genbank file to a vector
                      progress$inc(amount=1)
                  }
                  write(Vector_genbank, file, append=TRUE) # Writes the vector containing all the genbank file information into one genbank file
                  progress$set(value=progLength)
                  progress$close()
              })
            }
        }
    )
    
# * NCBISummaryReportDownload -----------------------------------------------------
    output$NCBIfileDownloadSD <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste("NCBI_Summary_Report", ".csv", sep = "")
      },
      content = function(file) {
          promise_all(data_df = summary_report(1)) %...>% with({
            write.csv(data_df, file) # Writes the dataframe to the CSV file
          })
        })

# * NCBIBarcodeButtons -----------------------------------------------------

    observeEvent(input$barcodeOptionCO1,{ # Detects when the specific barcode (in this case CO1) button has been pressed
        if(input$barcodeList[[1]] != "") { # If the input barcodeList is not empty (ie. the inputtextarea is not empty) then use the paste function to the add the barcode/s to the beginning
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("(CO1; COI; COX1),", input$barcodeList)) # Updates the text area input adds the barcode/s to the beginning of whatever is already in it
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "(CO1; COI; COX1)") # Here since the textarea is empty we just set its value to the barcode/s
        }
    })
    
    observeEvent(input$barcodeOption16S,{ 
        if(input$barcodeList[[1]] != "") { 
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("16S,", input$barcodeList)) 
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "16S")
        }
    })
    
    observeEvent(input$barcodeOptionITS2,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("ITS2,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "ITS2")
        }
    })
    
    observeEvent(input$barcodeOption18S,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("18S,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "18S")
        }
    })
    
    observeEvent(input$barcodeOptionITS1,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("ITS1,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "ITS1")
        }
    })
    
    observeEvent(input$barcodeOptiontrnl,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("trnl,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "trnl")
        }
    })
    
    observeEvent(input$barcodeOption12S,{
        if(input$barcodeList[[1]] != "") {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("12S,", input$barcodeList))
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "12S")
        }
    })
    

# * NCBIOutputTables ------------------------------------------------------------

    
    #outputs:
    output$seqLenInputs <- renderUI(seqLenList())
    
    output$NCBIcoverageResults <- DT::renderDataTable({
        # matrixGet(), rownames = NCBIorganismList(), colnames = barcodeList()
      barcodes <- barcodeList()
      promise_all(data_df = matrixGet(), rows = NCBIorganismList()) %...>% with({
        DT::datatable(data_df, rownames = rows, colnames = barcodes)
      })
    })
  

# * NCBInputFile ------------------------------------------------------------

  
    inputFileNCBI <- observeEvent(input$uploadNCBIButton,{
        isolate({
            req(input$uNCBIfile, file.exists(input$uNCBIfile$datapath))
            uploadinfo <- read.csv(input$uNCBIfile$datapath, header = TRUE)
            if(input$NCBIorganismList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = c(head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]), input$NCBIorganismList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "NCBIorganismList", value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
            }
            if(input$barcodeList[[1]] != "") {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = c(head(uploadinfo$Barcodes[uploadinfo$Barcodes != ""]), input$barcodeList))
            }
            else {
                updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = uploadinfo$Barcodes[uploadinfo$Barcodes != ""])
            }
        })
    })
    

# * NCBIDownloadTable -------------------------------------------------------
    
    # Download NCBI table
    output$download <- downloadHandler(
        filename = function() { # Create the file and set its name
          paste("NCBI_Table", ".csv", sep = "")
        },
        content = function(file) {
            columns <- barcodeList() # Gets the column names for the matrix
            NCBIorganismList() %...>% {
              rows <- . #Gets the row names for the matrix
              matrixGet() %...>% { # Gets the matrix for the NCBI results
                future_promise({
                  colnames(.) <- columns # Adds the column names to the matrix
                  rownames(.) <- rows # Adds the row names to the matrix
                  write.csv(., file) # Writes the dataframe to the CSV file
                })
              }
            }
        }
        
        
    )

# * NCBIDownloadSearchTerms -------------------------------------------------

    #Download Search Terms:
    output$downloadStatements <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste("NCBI_Search_Statements", ".csv", sep = "")
      },
      content = function(file) {
        
        columns <- barcodeList() # Gets the column names for the matrix
        NCBIorganismList() %...>% {
          rows <- . #Gets the row names for the matrix
          matrixGetSearchTerms() %...>% { # Gets the matrix for the NCBI results
            future_promise({
              colnames(.) <- columns # Adds the column names to the matrix
        
              rownames(.) <- rows # Adds the row names to the matrix
              write.csv(., file) # Writes the dataframe to the CSV file
            })
          }
        }
      }
    )

# * SummaryReport ----------------------------------------------
    
summary_report <- function(databaseFlag) {
  if(databaseFlag == 1) {
    matrixGet() %...>% {
      NCBIdata <- .
      NCBIorganismList() %...>% {
        columns <- barcodeList() # Gets the column names for the matrix
        colnames(NCBIdata) <- columns # Adds the column names to the matrix
        rownames(NCBIdata) <- . # Adds the row names to the matrix
        NCBIdata <- as.data.frame(NCBIdata) # Convert to Dataframe
        dataframe <- NCBIdata
        summary_report_dataframe(dataframe)
      }
    }
  } else {
    matrixGetCRUX() %...>% {
      CRUXmatrix <- . # Gets the matrix for the Crux results
      organismListGet() %...>% {
        columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") # Gets the column names for the matrix
        colnames(CRUXmatrix) <- columns # Adds the column names to the matrix
        rownames(CRUXmatrix) <- . # Adds the row names to the matrix
        dataframe <- CRUXmatrix
        #calls convert_CRUX()s
        dataframe <- convert_CRUX(dataframe)
        summary_report_dataframe(dataframe)
      }
    }
  }
}
    
# * * DownloadDataframe ----------------------------------------------

summary_report_dataframe <- function(dataframe)
{
  class(dataframe)
  class(dataframe[,1])
  options(scipen=999) #scientific notion
  new_row_names <- "total"
  new_row_names<-  c(new_row_names, colnames(dataframe))#doesn't include column with taxa snames
  
  statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
  new_col_names <- c("category","number of sequences found", "percent of total sequences found", "num of organism with at least one sequence", "num of organisms with no sequences")
  colnames(statistics_df) <- new_col_names
  #get list of columns + a column called "total"
  
  #add row names
  for(i in 1:length(new_row_names))
  {
    statistics_df[i,1]<-new_row_names[i]
  }
  
  barcodeSums <- colSums(dataframe) #doesn't include column with taxa snames
  
  Total_seq_found <- sum(barcodeSums)
  
  #hard code in the totals
  statistics_df[1,2] <- Total_seq_found
  statistics_df[1,3] <- 100
  
  for(i in 2:length(new_row_names))
  {
    x <- i - 1
    statistics_df[i,2] <- barcodeSums[x]
    statistics_df[i,3] <- (barcodeSums[x]/Total_seq_found)
  }
  
  #hard code in the totals
  output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, -1)
  statistics_df[1,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])    #list 2 is thee species without any seqs
  statistics_df[1,4] <-length(output_of_which_rows_are_empty_and_arenot[[1]])   #we know list 1 is the species with some seqs
  
  for(i in 2:length(new_row_names))
  {
    x <- i - 1
    output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, Which_Column = x)
    statistics_df[i,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])     #list 2 is the species without any seqs
    statistics_df[i,4] <- length(output_of_which_rows_are_empty_and_arenot[[1]])  #we know list 1 is the species with some seqs
  }
  statistics_df
}
  

# * * DownloadConvertCrux ----------------------------------------------


convert_CRUX <- function(crux_output #take a crux output matrix and  turn the characters "genus, spp, etc" into  0s/1s
                         #this function is used by which_rows_are_empty_and_arenot()
)
{
  crux_without_taxonomic_names <- crux_output
  crux_without_taxonomic_names<-  na.omit(crux_without_taxonomic_names)
  
  non_number_values <- c('genus', 'family', 'class', 'order', 'error')
  
  ncols <- ncol(crux_output)
  nrows <- nrow(crux_output)
  
  for(i in 1:ncols)
  {
    for(j in 1:nrows)
    {
      boolean <- crux_without_taxonomic_names[j,i]%in%non_number_values
      #if true, ie it matches genus, family, class, order
      if(isTRUE(boolean)) #if true, ie it matches genus, family, class, order
      {
        crux_without_taxonomic_names[j,i] <- as.numeric(0)
      } else {
        crux_without_taxonomic_names[j,i] <- as.numeric(crux_output[j,i])
      }
    }
  }
  
  firstcolumn <- crux_without_taxonomic_names[,1]
  
  crux_without_taxonomic_names <- as.matrix(crux_without_taxonomic_names)
  if(nrows > 1){
    crux_without_taxonomic_names <- as.data.frame(apply(crux_without_taxonomic_names, 2, as.numeric)) #apply(crux_without_taxonomic_names, 2, as.numeric)
  } else {
    crux_without_taxonomic_names <- as.data.frame(t(as.numeric(crux_without_taxonomic_names)))
    columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") # Gets the column names for the matrix
    colnames(crux_without_taxonomic_names) <- columns # Adds the column names to the matrix
  }
  crux_without_taxonomic_names
}

# * * DownloadEmptyRows ----------------------------------------------
#if which_column = -1 it means do all rows, if a column number is given the function will only run on said column of the dataframe
# returns list of 2 lists, one of species with seqs, and one of species without any sequences
which_rows_are_empty_and_arenot <- function(dataframe, Which_Column) 
{
  if(is.null(Which_Column))
  {
    Which_Column <- -1
  }
  Which_Column <- Which_Column
  #create two lists
  haveSomeSeq <- c()
  haveZeroSeq <- c()
  
  ncols <- ncol(dataframe)
  nrows <- nrow(dataframe)
  
  if(Which_Column < 0){
    
    for(i in 1:nrows) #we will skip the first column because it has names
    {
      total <- 0
      for(j in 1:ncols)
      {
        total <- total + as.numeric(dataframe[i,j])
      }
      
      if(!is.null(total) && total > 0)
      {
        haveSomeSeq <- c(haveSomeSeq, dataframe[i,1]) #add species name to list
      } else
      {
        haveZeroSeq <- c(haveZeroSeq, dataframe[i,1])#add species name to list
      }
    }
  }else #if a specific columnn
  {
    for(i in 1:nrows) #we will skip the first column because it has names
    {
      seqs <- 0
      seqs <- 0 + as.numeric(dataframe[i,Which_Column]) 
      
      if(!is.null(seqs) && seqs > 0)
      {
        haveSomeSeq <- c(haveSomeSeq, dataframe[i,1]) #add species name to list
      } else
      {
        haveZeroSeq <- c(haveZeroSeq, dataframe[i,1])#add species name to list
        
      }
    }
  }
  if(Which_Column < 0){
    results <- list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs =haveZeroSeq)
    results<- as.matrix(results)
  }else
  {
    COLNam <- colnames(dataframe)
    column_name <- paste0("Have",COLNam[Which_Column],"Seq")
    results <- list(single_Barcode_haveSomeseq = haveSomeSeq, single_Barcode_haveZeroSeqs =haveZeroSeq)
    results<- as.matrix(results)
  }
  results
}
    
})
