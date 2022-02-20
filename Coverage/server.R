#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Imports ----------------------------------------------------------------------

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
  # Full Genome ----------------------------------------------------------------
  
  # * FullGenomeSearchButton ---------------------------------------------------
  
  
  fullGenomeSearchButton <-
    eventReactive(input$genomeSearchButton, {
      # When searchButton clicked, update fGenOrgSearch to return the value 
      # input into genomeOrganismList
      input$genomeOrganismList #Returns as a string
    })
  
  # * FullGenomeInputCSV -------------------------------------------------------
  
  inputFileCrux <- observeEvent(input$uploadGenomeButton, {
    isolate({
      # It requires a file to be uploaded first
      req(input$uploadGenomeFile,
          file.exists(input$uploadGenomeFile$datapath)) 
      # Read the CSV and write all the Organism Names into the Text Area Input
      uploadinfo <-
        read.csv(input$uploadGenomeFile$datapath, header = TRUE)
      if (input$genomeOrganismList[[1]] != "") {
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "genomeOrganismList",
          value = c(
            head(uploadinfo$OrganismNames,-1),
            input$genomeOrganismList
          )
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "genomeOrganismList",
                            value = uploadinfo$OrganismNames)
      }
    })
  })
  
  
  # * FGenOrgSearch ------------------------------------------------------------
  
  # Split the string by commas to create a list of Species 
  # and do the Taxize operations
  fGenOrgSearch <- reactive({
    orgString <- fullGenomeSearchButton()
    # Get the string of all the species
    fullGenomeTaxizeOption <- input$fullGenomeTaxizeOption
    future_promise({
      # Break up the string using commas and delete an empty indexes in the list
      genomeOrgList <- strsplit(orgString, ",")[[1]]
      genomeOrgList <- unique(genomeOrgList[genomeOrgList != ""])
      # If the taxize option is selected
      if (fullGenomeTaxizeOption) {
        # Initialize an empty vector
        taxizeGenOrgList <- c() 
        
        for (i in 1:length(genomeOrgList))
        {
          err <- 1
          # Trim both leading and trailing whitespace
          organism <- trimws(genomeOrgList[[i]], "b") 
          while (err == 1) {
            # Repeat the search until we get some results
            NCBI_names <- tryCatch({
              if (!NCBIKeyFlag) {
                # Sleep to prevent Rate Limiting
                Sys.sleep(0.34) 
              }
              # Help user with various naming issues (spelling, synonyms, etc.)
              NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) 
              err <- 0
              NCBI_names # Return this variable
            }, error = function(err) {
              err <<- 1
            })
          }
          # Get number of rows in dataframe
          row_count <- nrow(NCBI_names) 
          
          # If a legitimate name was found
          if (row_count > 0)
          {
            for (j in 1:row_count)
            {
              # Store each matched name in taxa_name
              taxa_name <- NCBI_names[[j, 3]] 
              # Update the vector with all the taxa_names.
              taxizeGenOrgList <- c(taxizeGenOrgList, taxa_name) 
            }
          }
          else # If a legitimate name was not found
          {
            # Just append organism to the list, and return taxizeGenOrgList
            taxizeGenOrgList <- c(taxizeGenOrgList, organism) 
          }
        }
        taxizeGenOrgList
      } else{
        genomeOrgList # Return the list as is
      }
    })
  })
  
  # * Mitochondrial Search -----------------------------------------------------
  
  Organisms_with_Mitochondrial_genomes <- reactive({
    fGenOrgSearch() %...>% {
      # Get the list returned by fGenOrgSearch
      genomeList <- . 
      num_rows <- length(genomeList)
      # Create dataframe for Results
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
      # Create a vector to store the unique IDs
      uids <- c()
      
      parameters <- "set vector up"
      
      # If the Reference Sequences option is selected
      if (isTRUE(input$refSeq))
      {
        parameters <-
          " AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN] AND srcdb_refseq[PROP]"
        names(Results) <-
          c(
            'Num_RefSeq_Mitochondrial_Genomes_in_NCBI_Nucleotide',
            'SearchStatements'
          )
      }
      else
      {
        parameters <-
          " AND (mitochondrial[TITL] or mitochondrion[TITL]) AND 16000:17000[SLEN]"
        names(Results) <-
          c('Num_Mitochondrial_Genomes_in_NCBI_Nucleotide',
            'SearchStatements')
      }
      
      future_promise({
        for (i in 1:num_rows)
        {
          # Set up the Search Term
          Mitochondrial_genome_SearchTerm <-
            paste0('', genomeList[i], '[ORGN]', parameters, '') 
          searchResult <- tryCatch({
            if (!NCBIKeyFlag) {
              # Sleep to prevent Rate Limiting
              Sys.sleep(0.34)
            }
            # Search in NCBI
            genome_result <-
              entrez_search(db = "nucleotide",
                            term = Mitochondrial_genome_SearchTerm,
                            retmax = 5) 
            
            Results[i, 1] <- genome_result$count
            Results[i, 2] <- Mitochondrial_genome_SearchTerm
            # Save all the UIDs needed for downloading
            for (id in genome_result$ids) {
              uids <- c(uids, id) 
            }
          }, error = function(err) {
            # If an error occurred we mark it in the results so the user knows
            Results[i, 1] <<- "Error"
            Results[i, 2] <<- "Error"
          })
        }
        # Create a list containing the two vectors and return
        list(Results, uids) 
      })
    }
  })
  
  # * Chloroplast Search -------------------------------------------------------
  
  Organisms_with_Chloroplast_genomes <- reactive({
    fGenOrgSearch() %...>% {
      num_rows <- length(.)
      # Get the list returned by fGenOrgSearch
      genomeList <- . 
      # Create dataframe for Results
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows))
      # Create a vector to store the unique IDs
      uids <- c() 
      
      parameters <- "set vector up"
      
      # If the Reference Sequences option is selected
      if (isTRUE(input$refSeq))
      {
        parameters <-
          " AND Chloroplast[TITL] AND 120000:170000[SLEN] AND srcdb_refseq[PROP]"
        names(Results) <-
          c('Num_RefSeq_Chloroplast_Genomes_in_NCBI_Nucleotide',
            'SearchStatements')
      } else
      {
        parameters <- " AND Chloroplast[TITL] AND 120000:170000[SLEN]"
        names(Results) <-
          c(
            'Num_Chloroplast_Genomes_in_NCBI_Nucleotide',
            'Chloroplast_SearchStatements'
          )
      }
      
      future_promise({
        for (i in 1:num_rows)
        {
          # Set up the Search Term
          Chloroplast_genome_SearchTerm <-
            paste0('', genomeList[i], '[ORGN]', parameters, '') 
          searchResult <- tryCatch({
            if (!NCBIKeyFlag) {
              # Sleep to prevent Rate Limiting
              Sys.sleep(0.34) 
            }
            # Search in NCBI
            genome_result <-
              entrez_search(db = "nucleotide",
                            term = Chloroplast_genome_SearchTerm,
                            retmax = 5)
            # Get the count of the results that NCBI returned
            Results[i, 1] <- genome_result$count 
            Results[i, 2] <- Chloroplast_genome_SearchTerm
            
            # Save all the UIDs needed for downloading
            for (id in genome_result$ids) {
              uids <- c(uids, id) 
            }
          }, error = function(err) {
            # If an error occurred we mark it in the results so the User Knows
            Results[i, 1] <<- "Error"
            Results[i, 2] <<- "Error"
          })
        }
        list(Results, uids)
      })
    }
  })
  
  # * Is_the_taxa_in_the_NCBI_genome_DB ----------------------------------------
  
  Is_the_taxa_in_the_NCBI_genome_DB <- reactive ({
    fGenOrgSearch() %...>% {
      num_rows <- length(.)
      # Get the list returned by fGenOrgSearch
      genomeList <- . 
      # Create dataframe for Results
      Results <- data.frame(matrix(0, ncol = 2, nrow = num_rows)) 
      # Create a vector to store the unique IDs
      uids <- c() 
      
      names(Results) <-
        c('present_in_NCBI_Genome', 'GenomeDB_SearchStatements')
      future_promise({
        for (i in 1:num_rows)
        {
          # Set up the Search Term
          genome_SearchTerm <- paste0('', genomeList[i], '[ORGN]', '')
          searchResult <- tryCatch({
            if (!NCBIKeyFlag) {
              # Sleep to prevent Rate Limiting
              Sys.sleep(0.34) 
            }
            # Search in NCBI
            genome_result <-
              entrez_search(db = "genome",
                            term = genome_SearchTerm,
                            retmax = 5) 
            # Get and save the count of the results that NCBI returned
            Results[i, 1] <- genome_result$count 
            # Save the search term in the results dataframe
            Results[i, 2] <- genome_SearchTerm 
            # Save all the UIDs needed for downloading
            for (id in genome_result$ids) {
              uids <- c(uids, id) 
            }
          }, error = function(err) {
            # If an error occurred we mark it in the results so the User Knows
            Results[i, 1] <<- "Error"
            Results[i, 2] <<- "Error"
          })
        }
        # Create a list containing the two vectors and return
        list(Results, uids)
      })
    }
  })
  
  #  * selectFunction  ---------------------------------------------------------
  
  selectfunction <- reactive({
    # See which option has been selected call the right function and return 
    # the list
    if (input$gsearch == "Full mitochondrial genomes in NCBI Nucleotide")
    {
      Organisms_with_Mitochondrial_genomes() %...>% {
        genomes <- .
        genomes # list that gets returned if the if is entered
      }
    }
    else if (input$gsearch == "Full chloroplast genomes in NCBI Nucleotide")
    {
      Organisms_with_Chloroplast_genomes() %...>% {
        genomes <- .
        genomes # list that gets returned if the if is entered
      }
    }
    else if (input$gsearch == "Number of entries per taxa in NCBI Genome")
    {
      Is_the_taxa_in_the_NCBI_genome_DB() %...>% {
        genomes <- .
        genomes # list that gets returned if the if is entered
      }
    }
  })
  
  # * Output Table render ------------------------------------------------------
  
  # Output function which shows the table with the results to the User
  output$genomeResults <- DT::renderDataTable({
    promise_all(data_df = selectfunction(), 
                rows = fGenOrgSearch()) %...>% with({
      DT::datatable(data_df[[1]],
                    rownames = rows,
                    colnames = names(data_df[[1]]))
    })
  })
  
  # * Download Fastas ----------------------------------------------------------
  
  # Download Full Genome Fasta Files
  output$fullGenomeDownloadF <- downloadHandler(
    filename = function() {
      # Create the file
      paste("Full_Genome_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        # Get the Unique IDs from the selectfunction returned list
        uids <- .[[2]] 
        progLength <- length(uids)
        # Progress bar for the UI
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_Fasta <- c()
          for (uid in uids) {
            # Loop through the uids, download them and store them in a vector
            err <- 1
            while (err == 1) {
              # Repeat the search until we get some results
              File <-
                tryCatch({
                  # Try catch for determining if an error occurred while 
                  # fetching NCBI data
                  if (!NCBIKeyFlag) {
                    Sys.sleep(0.34) # Sleep to prevent NCBI rate limiting
                  }
                  # Get the fasta file with that uid
                  File_fasta <- entrez_fetch(db = "nucleotide",
                                             id = uid,
                                             rettype = "fasta") 
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            # Append the fasta file to a vector
            Vector_Fasta <- c(Vector_Fasta, File_fasta) 
            # Increase the progress bar
            progress$inc(amount = 1) 
          }
          # Writes the vector containing all the fasta file information 
          # into one fasta file
          write(Vector_Fasta, file)
          # Close progress bar
          progress$set(value = progLength) 
          progress$close()
        })
      }
    }
  )
  
  # * Download Genbank files ---------------------------------------------------
  
  # Download Full Genome Genbank Files
  output$fullGenomeDownloadG <- downloadHandler(
    filename = function() {
      # Create the file
      paste("Full_Genome_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        # Get the Unique IDs from the selectfunction returned list
        uids <- .[[2]] 
        progLength <- length(uids)
        # Progress bar for the UI
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_genbank <- c()
          for (uid in uids) {
            # Loop through the uids, download them and store them in a vector
            err <- 1
            while (err == 1) {
              # Repeat the search until we get some results
              File <-
                tryCatch({
                  # Try catch for determining if an error occurred while 
                  # fetching NCBI data
                  if (!NCBIKeyFlag) {
                    # Sleep to prevent NCBI rate limiting
                    Sys.sleep(0.34) 
                  }
                  # Get the genbank file with that uid
                  File_genbank <-
                    entrez_fetch(db = "nucleotide",
                                 id = uid,
                                 rettype = "gb") 
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            # Append the genbank file to a vector
            Vector_genbank <- c(Vector_genbank, File_genbank) 
            # Increase the progress bar
            progress$inc(amount = 1) 
          }
          # Writes the vector containing all the genbank file information
          # into one genbank file
          write(Vector_genbank, file) 
          progress$set(value = progLength)
          # Close the progress bar
          progress$close() 
        })
      }
    }
  )
  
  # * Download Full Genome Results Table ---------------------------------------
  
  # Download Full Genome Table
  output$fullGenomeDownloadT <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("Full_Genome_Table", ".csv", sep = "")
    },
    content = function(file) {
      selectfunction() %...>% {
        # Gets the matrix for the FullGenome search results
        FullGenmatrix <- .[[1]] 
        fGenOrgSearch() %...>% {
          # Adds the row names to the matrix
          rownames(FullGenmatrix) <- . 
          # Writes the dataframe to the CSV file
          write.csv(FullGenmatrix, file) 
        }
      }
    }
  )
  
  
  # CRUX ----------------------------------------------------------------------
  
  # * CRUXSearchButton --------------------------------------------------------
  
  cruxOrgSearch <-
    eventReactive(input$searchButton, {
      # When searchButton clicked, update CruxOrgSearch to return the value 
      # input into CRUXorganismList
      input$CRUXorganismList # Returns as a string
    })
  
  # * CRUXStrToList -----------------------------------------------------------
  
  cruxOrganismList <-
    reactive({
      # Converts string from cruxOrgSearch into a list of Strings
      # Get list of species
      cruxOrgSearch <- cruxOrgSearch()
      # Variable to store the user selection for the taxize option
      CRUXtaxizeOption <- input$CRUXtaxizeOption 
      future_promise({
        # Separate based on commas
        organismList <- strsplit(cruxOrgSearch[[1]], ",")[[1]]
        # Delete any empty 'species'
        organismList <- unique(organismList[organismList != ""]) 
        if (CRUXtaxizeOption) {
          # If the taxize option is selected
          # Initialize an empty vector
          taxize_organism_list <- c() 
          for (i in 1:length(organismList))
          {
            err <- 1
            # Trim both leading and trailing whitespace
            organism <- trimws(organismList[[i]], "b") 
            while (err == 1) {
              NCBI_names <- tryCatch({
                if (!NCBIKeyFlag) {
                  # Sleeping for 1/3 of a second each time gives us 3 queries 
                  # a second. If each user queries at this rate, we can service 
                  # 4-8 at the same time.
                  Sys.sleep(0.34) 
                }
                # Help user with various naming issues 
                # (spelling, synonyms, etc.)
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) 
                err <- 0
                NCBI_names
              }, error = function(err) {
                err <<- 1
              })
            }
            
            # Get number of rows in dataframe
            row_count <- nrow(NCBI_names) 
            
            # If a legitimate name was found
            if (row_count > 0)
            {
              for (j in 1:row_count)
              {
                # Store each matched name in taxa_name
                taxa_name <- NCBI_names[[j, 3]] 
                # Update the vector with all the taxa_names.
                taxize_organism_list <- c(taxize_organism_list, taxa_name)
              }
            }
            else
            {
              # Just append organism to the list, and return 
              # taxize_organism_list
              taxize_organism_list <- c(taxize_organism_list, organism) 
            }
          }
          taxize_organism_list
        } else{
          # Return the list as is
          organismList 
        }
      })
    })
  
  # * CRUXSearch ---------------------------------------------------------------
  cruxSearch <- function(results, searchTerm, organism) {
    # Make a list of db tables, each representing a marker
    dbList <-
      list("MB18S",
           "MB16S",
           "MBPITS",
           "MBCO1",
           "MBFITS",
           "MBtrnL",
           "MB12S") 
    # Connect to the db
    taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") 
    for (table in dbList) {
      # First check if the organism can be found at any level in the 
      # CRUX database
      location <-
        dbGetQuery(
          taxaDB,
          paste(
            "SELECT * from ",
            table,
            " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
          ),
          params = list(x = organism)
        )
      if (nrow(location) == 0) {
        # If not found go up by one level i.e. now search using the genus 
        # instead of Species Genus, when found only write which level it is 
        # not the number of results
        location <-
          dbGetQuery(
            taxaDB,
            paste(
              "SELECT * from ",
              table,
              " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
            ),
            params = list(x = searchTerm[1, 3])
          )
        if (nrow(location) == 0) {
          location <-
            dbGetQuery(
              taxaDB,
              paste(
                "SELECT * from ",
                table,
                " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
              ),
              params = list(x = searchTerm[1, 4])
            )
          if (nrow(location) == 0) {
            location <-
              dbGetQuery(
                taxaDB,
                paste(
                  "SELECT * from ",
                  table,
                  " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
                ),
                params = list(x = searchTerm[1, 5])
              )
            if (nrow(location) == 0) {
              location <-
                dbGetQuery(
                  taxaDB,
                  paste(
                    "SELECT * from ",
                    table,
                    " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
                  ),
                  params = list(x = searchTerm[1, 6])
                )
              if (nrow(location) == 0) {
                location <-
                  dbGetQuery(
                    taxaDB,
                    paste(
                      "SELECT * from ",
                      table,
                      " where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"
                    ),
                    params = list(x = searchTerm[1, 7])
                  )
                results <- c(results, nrow(location))
              } else {
                results <- c(results, "class")
              }
            } else {
              results <- c(results, "order")
            }
          } else {
            results <- c(results, "family")
          }
        } else {
          results <- c(results, "genus")
        }
      } else {
        results <- c(results, toString(nrow(location)))
      }
    }
    dbDisconnect(taxaDB) # Disconnect from the database
    results # Return results
  }
  
  # * CRUXCoverage -------------------------------------------------------------
  
  cruxCoverage <- reactive({
    cruxOrganismList() %...>% {
      organismList <-
        . # Get Organism list already processed by taxize if option was checked
      # Make sure at least one organism is being searched
      organismListLength <- length(organismList)
      validate(
        need(organismListLength > 0, 'Please name at least one organism'))
      
      searchTerm <- ""
      searchResult <- 0
      popuplist <- c()
      
      future_promise({
        # Error when trying to find if there are homonyms
        errorPopupList <- c() 
        # Error when we know there are homonyms but we could not finish 
        # the search
        errorPopupListFound <-c() 
        newOrgList <- c()
        err <- 0
        results <- c()
        search <- c()
        for (organism in organismList) {
          errorHomonym <- 0
          search <-
            tryCatch({
              # Try catch for determining if homonyms exist, if they do fill up 
              # the errorPopupList and activate the errorHomonym Flag
              if (!NCBIKeyFlag) {
                # Sleep to avoid rate limiting
                Sys.sleep(0.34) 
              }
              # Check to see if there are homonyms
              search <- get_uid_(sci_com = organism) 
            }, error = function(err) {
              errorHomonym <<- 1
            })
          if (errorHomonym == 1) {
            # There was an error add to pop up list
            errorPopupList <- c(errorPopupList, organism) 
          }
          else if (is.null(search[[1]])) {
            # If search ran into an error or it returned null
            results <- c(results, "0", "0", "0", "0", "0", "0", "0")
            newOrgList <- c(newOrgList, organism)
            next
          }
          # There are homonyms
          if (errorHomonym != 1 && nrow(search[[1]]) > 1) {
            # Add to the pop up list to inform the user of the homonyms
            popuplist <- c(popuplist, organism) 
            for (i in 1:nrow(search[[1]])) {
              # Loop through the tax_names
              errorHomonym <- 0
              # if there are more than 5 homonyms then break we are not 
              # interested in more than 5
              if (i > 5) {
                break
              }
              # Create new organism list since new organism are added
              newOrg <- paste(organism, search[[1]]$division[i], sep = " ")
              newOrgList <- c(newOrgList, newOrg)
              # Creating the same format as the other organisms so the Crux
              # search can be performed correctly
              
              # Try catch for when we know there are homonyms but we dont know 
              # which homonyms yet.
              hierarchy <- tryCatch({
                if (!NCBIKeyFlag) {
                  # Sleep to avoid rate limiting
                  Sys.sleep(0.34)
                }
                # Get information on those homonyms
                hierarchy <-
                  classification(search[[1]]$uid[i], db = "ncbi")[[1]] 
                hierarchy
              }, error = function(err) {
                # if there is an error, fill up errorPopupListFound and activate 
                # the errorHomonym Flag
                errorHomonym <<- 1
              })
              if (errorHomonym == 1) {
                # If an error happened during the search add to the error pop up
                # list and mark the errors so the user knows
                errorPopupListFound <- unique(c(errorPopupListFound, newOrg))
                results <-
                  c(
                    results,
                    "error",
                    "error",
                    "error",
                    "error",
                    "error",
                    "error",
                    "error"
                  )
                next
              }
              # Set the right format with all the information necessary for the 
              # CRUX search
              match <-
                hierarchy$name[match(tolower(
                  c(
                    "genus",
                    "family",
                    "order",
                    "class",
                    "phylum",
                    "domain"
                  )
                ), tolower(hierarchy$rank))]
              query <-
                c("db",
                  "query",
                  "genus",
                  "family",
                  "order",
                  "class",
                  "phylum",
                  "domain")
              match <- c("ncbi", organism, match)
              searchTerm <- stats::setNames(data.frame(t(match), 
                                            stringsAsFactors = FALSE),
                                            query)
              # Perform the CruxSearch
              results <- cruxSearch(results, searchTerm, organism)
            }
          } else {
            # There are no homonyms
            
            # Add to the new org list
            newOrgList <- c(newOrgList, organism) 
            searchTerm <- tryCatch({
              if (!NCBIKeyFlag) {
                # Sleep to avoid rate limiting
                Sys.sleep(0.34) 
              }
              searchTerm <-
                tax_name(
                  query = organism,
                  get = c(
                    "genus",
                    "family",
                    "order",
                    "class",
                    "phylum",
                    "domain"
                  ),
                  db = "ncbi",
                  messages = FALSE
                )
              searchTerm
            }, error = function(err) {
              # Mark the errors if the search didn't work
              results <<-
                c(results,
                  "error",
                  "error",
                  "error",
                  "error",
                  "error",
                  "error",
                  "error")
              err <<- 1
            })
            if (err == 1) {
              err <- 0
              next
            }
            # Perform the CruxSearch
            results <- cruxSearch(results, searchTerm, organism)
          }
        }
        # Create list with all the necessary information
        results <-
          list(
            organismList = newOrgList,
            data = results,
            popupinfo = popuplist,
            errorPopupList = errorPopupList,
            errorPopupListFound = errorPopupListFound
          )
        results
      })
    }
  })
  
  # * matrixGetCRUX ------------------------------------------------------------
  
  matrixGetCRUX <-
    reactive({
      # Creates and returns the matrix to be displayed with the count
      dbList <-
        list("MB18S",
             "MB16S",
             "MBPITS",
             "MBCO1",
             "MBFITS",
             "MBtrnL",
             "MB12S")
      cruxCoverage() %...>% {
        # Get the results from the SQL queries
        cruxCoverage <- . 
        results <- c()
        organismListLength <- length(cruxCoverage[[1]])
        # Extract the results
        for (i in cruxCoverage[[2]]) {
          results <- c(results, i)
        }
        # Convert results vector to dataframe
        data <-
          matrix(
            results,
            nrow = organismListLength,
            ncol = length(dbList),
            byrow = TRUE
          )
        data
      }
    })
  
  # * organismListGet ----------------------------------------------------------
  
  organismListGet <-
    reactive({
      # Returns the uids stored in the results from the NCBi query
      organismList <- c()
      cruxCoverage() %...>% {
        # Get the results from the NCBI query
        cruxCoverage <- . 
        for (i in cruxCoverage[[1]]) {
          # Extract the new organism list
          organismList <- c(organismList, i)
        }
        # Send the alert to the user related to homonyms
        cruxOrganismList() %...>% {
          if (length(organismList) > length(.)) {
            shinyalert("We have found Homonyms", 
                       cruxCoverage[[3]], 
                       type = "warning")
          }
          if (length(cruxCoverage[[4]]) > 0) {
            shinyalert(
              "Homonyms for the following species could not be checked properly. Try again later",
              cruxCoverage[[4]],
              type = "error"
            )
          }
          if (length(cruxCoverage[[5]]) > 0) {
            shinyalert(
              "Homonyms for the following species were found but were not able to be processed correctly",
              cruxCoverage[[5]],
              type = "error"
            )
          }
        }
        organismList
      }
    })
  
  
  # * CRUXInputCSV -------------------------------------------------------------
  
  inputFileCrux <-
    observeEvent(input$uploadCRUXButton, {
      # Load Input file into text box
      isolate({
        # It requires a file to be uploaded first
        req(input$uCRUXfile, file.exists(input$uCRUXfile$datapath))
        # Read the CSV and write all the Organism Names into the Text Area Input
        uploadinfo <- read.csv(input$uCRUXfile$datapath, header = TRUE)
        if (input$CRUXorganismList[[1]] != "") {
          updateTextAreaInput(
            getDefaultReactiveDomain(),
            "CRUXorganismList",
            value = c(
              head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]),
              input$CRUXorganismList
            )
          )
        }
        else {
          updateTextAreaInput(getDefaultReactiveDomain(),
                              "CRUXorganismList",
                              value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
        }
      })
    })
  
  
  # * CRUXDownload -------------------------------------------------------------
  
  # Download CRUX table
  output$downloadCrux <- downloadHandler(
    filename = function() {
      # Create the file
      paste("CRUX_Table", ".csv", sep = "")
    },
    content = function(file) {
      # Gets the column names for the matrix
      columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      cruxCoverage() %...>% {
        cruxCoverage <- .
        rows <- cruxCoverage[[1]]
        # Get the matrix for the Crux results
        matrixGetCRUX() %...>% {
          # Adds the column names to the matrix
          colnames(.) <-columns
          # Adds the row names to the matrix
          rownames(.) <- rows 
          
          # Writes the matrix to the CSV file
          write.csv(., file) 
        }
      }
    }
  )
  
  
  
  # * CRUXSummaryReportDownload ------------------------------------------------
  
  # Download CRUX Summary Report
  output$CRUXfileDownloadSD <- downloadHandler(
    filename = function() {
      # Create the file
      paste("CRUX_Summary_Report", ".csv", sep = "")
    },
    content = function(file) {
      promise_all(data_df = summary_report(0)) %...>% with({
        # Writes the dataframe to the CSV file
        write.csv(data_df, file) 
      })
    }
  )
  
  
  
  # * CRUXOutput ---------------------------------------------------------------
  
  output$CRUXcoverageResults <- DT::renderDataTable({
    promise_all(data_df = matrixGetCRUX(), 
                rows = organismListGet()) %...>% with({
      DT::datatable(
        data_df,
        rownames = rows,
        colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      )
    })
  })
  
  
  # NCBI -----------------------------------------------------------------------
  
  
  # * NCBISearchButton ---------------------------------------------------------
  
  NCBISearch <-
    eventReactive(input$NCBIsearchButton, {
      # When searchButton clicked, update NCBIOrgSearch to return the value 
      # input into NCBIorganismList
      list(input$NCBIorganismList, input$barcodeList) #Returns as a string
    })
  
  # * NCBI_Key -----------------------------------------------------------------
  NCBIKeyFlag <- FALSE
  observeEvent(input$SetKey, {
    #When NCBIKey is inputed
    key <- 0
    NCBI_names <- tryCatch({
      searchResult <-
        entrez_search(db = "nucleotide",
                      term = "Gallus Gallus",
                      api_key = input$NCBIKey)
    }, error = function(err) {
      shinyalert("Your API key has been rejected, please make sure it is correct",
                 type = "warning")
      key <<- 1
    })
    if (key == 0) {
      set_entrez_key(input$NCBIKey)
      shinyalert("Your API key has been accepted", type = "success")
      NCBIKeyFlag <- TRUE
    }
  })
  
  
  # * NCBIStrToList ------------------------------------------------------------
  
  NCBIorganismList <-
    reactive({
      #Converts string from NCBIorganismList into a list of Strings
      orgString <- NCBISearch()
      NCBItaxizeOption <- input$NCBItaxizeOption
      future_promise({
        #separate based on commas
        organismList <- strsplit(orgString[[1]], ",")[[1]]
        organismList <- unique(organismList[organismList != ""])
        
        #if the taxize option is selected
        if (NCBItaxizeOption) {
          #initialize an empty vector
          taxize_organism_list <- c()
          
          for (i in 1:length(organismList))
          {
            err <- 1
            #trim both leading and trailing whitespace
            organism <- trimws(organismList[[i]], "b")
            while (err == 1) {
              NCBI_names <- tryCatch({
                if (!NCBIKeyFlag) {
                  # sleeping for 1/3 of a second each time gives us 3 queries a 
                  # second. If each user queries at this rate, we can service 
                  # 4-8 at the same time.
                  Sys.sleep(0.34)
                }
                NCBI_names <-
                  gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
                err <- 0
                NCBI_names
              }, error = function(err) {
                err <<- 1
              })
            }
            # get number of rows in dataframe
            row_count <- nrow(NCBI_names)
            
            #If a legitimate name was found
            if (row_count > 0)
            {
              #Store each matched name in taxa_name
              for (j in 1:row_count)
              {
                taxa_name <- NCBI_names[[j, 3]]
                #update the vector with all the taxa_names.
                taxize_organism_list <- c(taxize_organism_list, taxa_name) 
              }
            }
            else
            {
              #just append organism to the list, and return taxize_organism_list
              taxize_organism_list <- c(taxize_organism_list, organism) 
            }
          }
          taxize_organism_list
        } else{
          #return the list as is
          organismList 
        }
      })
    })
  
  
  # * NCBIBarcodeList ----------------------------------------------------------
  
  barcodeList <- reactive({
    # separate based on comma
    barcodeList <- strsplit(NCBISearch()[[2]], ",") 
    barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
    barcodeList[[1]] <- unique(barcodeList[[1]][barcodeList[[1]] != ""])
    barcodeList[[1]]
  })
  
  
  # * NCBISequenceLength -------------------------------------------------------
  
  seqLenList <- reactive({
    #list of sequence length specifications
    
    #only present if the option is selected
    if (input$seqLengthOption) {
      #separate based on comma
      barcodeList <- strsplit(input$barcodeList, ",")
      barcodeList[[1]] <- trimws(barcodeList[[1]], "b")
      barcodeList[[1]] <-
        unique(barcodeList[[1]][barcodeList[[1]] != ""])
      
      #allow the user to specify a different length for every barcode
      textList <- list()
      for (marker in barcodeList[[1]]) {
        #add a numeric input
        textList <-
          list(textList,
               numericRangeInput(
                 inputId = marker,
                 label = paste("Min/max sequence length for", marker),
                 value = c(0, 2000)
               ))
      }
      #return the list of numeric inputs
      textList
    }
  })
  
  
  # * NCBICoverage -------------------------------------------------------------
  
  genBankCoverage <- reactive({
    NCBIorganismList() %...>% {
      #get species and barcode inputs
      organismList <- . 
      organismListLength <- length(organismList)
      
      codeList <- barcodeList()
      codeListLength <- length(barcodeList())
      validate(
        #verify that the  user has typed things into both inputs
        need(organismListLength > 0, 'Please name at least one organism'),
        need(codeListLength > 0, 'Please choose at least one barcode')
      )
      searchTerm <- ""
      #initialize empty vector
      countResults <- list()
      uids <- list()
      searchResult <- 0
      
      #Temp vars for search options
      NCBISearchOptionGene <- input$NCBISearchOptionGene
      NCBISearchOptionOrgn <- input$NCBISearchOptionOrgn
      downloadNumber <- input$downloadNum
      seqLengthOption <- input$seqLengthOption
      seq_len_list <- list()
      for (code in codeList) {
        seq_len_list[[code]] <- input[[code]]
      }
      
      future_promise({
        err <- 0              
        countResults <- list() 
        searchTerms <- list()   
        
        for (organism in organismList) {
          for (code in codeList) {
            # TODO: Add more sanitization to this
            
            # if there is a parenthesis
            code <- trimws(code)
            if (substring(code, 1, 1) == "(") {
              # code is in the format (loci1; loci2; loci3...)
              # We will make a combined query by substituting the ;s for 
              # other stuff
              
              # set up a replacement string
              replacement <- ""
              if (NCBISearchOptionGene) {
                replacement <- "[GENE]"
              }
              # Add organism info
              if (NCBISearchOptionOrgn) {
                #our query to GenBank
                replacement <-
                  paste(replacement, " AND ", organism, "[ORGN]", sep = "")
              }
              else {
                #our non-Metadata query to GenBank
                replacement <-
                  paste(replacement, " AND ", organism, sep = "")
              }
              # Add sequence length info
              if (seqLengthOption) {
                #if the user specified sequence length
                replacement <-
                  paste(
                    replacement,
                    " AND ",
                    seq_len_list[[code]][1],
                    ":",
                    seq_len_list[[code]][2],
                    "[SLEN]",
                    sep = ""
                  )
              }
              # Add the tail to the replacement string
              replacement <- paste(replacement, ") OR (", sep = "")
              print(replacement)
              # Now we finally set searchTerm by replacing the ;s.
              searchTerm <- gsub(";", replacement, code)
              # But the last synonym won't have a semicolon after it! Sub in 
              # one last time:
              
              # trim last parenthesis
              searchTerm <-
                substring(searchTerm, 1, nchar(searchTerm) - 1)
              # add in replacement string
              searchTerm <- paste(searchTerm, replacement, sep = "")
              # cut off the " OR ("
              searchTerm <- substring(searchTerm, 1, nchar(searchTerm) - 5)
              
            } else {
              print("Entering else block")
              if (NCBISearchOptionOrgn) {
                #our query to GenBank
                searchTerm <- paste(organism, "[ORGN] AND ", sep = "") 
              }
              else {
                #our non-Metadata query to GenBank
                searchTerm <- paste(organism, " AND ", sep = "")
              }
              print("past NCBISearchOptionOrgn")
              if (NCBISearchOptionGene) {
                #our query to GenBank
                searchTerm <- paste(searchTerm, code, "[GENE]", sep = "") 
              }
              else {
                #our query to GenBank
                searchTerm <- paste(searchTerm, code, sep = "") 
              }
              print("entering seqLenghtOption")
              
              #if the user specified sequence length
              if (seqLengthOption) {
                searchTerm <-
                  paste(
                    searchTerm,
                    " AND ",
                    seq_len_list[[code]][1],
                    ":",
                    seq_len_list[[code]][2],
                    "[SLEN]",
                    sep = ""
                  ) 
              }
              print("Past seqLengthOption")
            }
            searchResult <- tryCatch({
              if (!NCBIKeyFlag) {
                # sleeping for 1/3 of a second each time gives us 3 queries a 
                # second. If each user queries at this rate, we can service 4-8
                # at the same time.
                Sys.sleep(0.34) 
              }
              #only get back the number of search results
              searchResult <-
                entrez_search(db = "nucleotide",
                              term = searchTerm,
                              retmax = downloadNumber) 
            }, error = function(err) {
              countResults <<- list.append(countResults, "error")
              searchTerms <<- list.append(searchTerms, searchTerm)
              err <<- 1
            })
            if (err == 1) {
              err <- 0
              next
            } else {
              uids <- list.append(uids, searchResult$ids)
              searchTerms <- list.append(searchTerms, searchTerm) 
              #append the count to the vector of results
              countResults <- list.append(countResults, searchResult$count)
            }
          }
        }
        results <-
          list(count = countResults,
               ids = uids,
               searchTermslist = searchTerms)
        results
      })
    }
  })
  
  
  # * NCBIMatrix ---------------------------------------------------------------
  
  matrixGet <-
    reactive({
      # creates and returns the matrix to be displayed with the count
      NCBIorganismList() %...>% {
        #get species and barcode inputs
        organismList <- . 
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        genBankCoverage() %...>% {
          # Get the results from the NCBI query
          count <- c()
          for (i in .[[1]]) {
            count <- c(count, i)
          }
          
          #convert results vector to dataframe
          data <-
            matrix(count,
                   nrow = organismListLength,
                   ncol = codeListLength,
                   byrow = TRUE)
          data
        }
      }
    })
  
  # * NCBITableOutput ----------------------------------------------------------
  
  matrixGetSearchTerms <-
    reactive({
      # creates and returns the matrix to be displayed with the count
      NCBIorganismList() %...>% {
        #get species and barcode inputs
        organismList <- . 
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        genBankCoverage() %...>% {
          # Get the results from the NCBI query
          SearchStatements <- c()
          for (i in .[[3]]) {
            #3 is the 3rd list in genBankCovearage aka the searchterms list
            SearchStatements <- c(SearchStatements, i)
          }
          
          #convert results vector to dataframe
          data <-
            matrix(
              SearchStatements,
              nrow = organismListLength,
              ncol = codeListLength,
              byrow = TRUE
            )
          data
        }
      }
    })
  
  
  # *   NCBIGetIDs -------------------------------------------------------------
  
  uidsGet <-
    reactive({
      # Returns the uids stored in the results from the NCBi query
      genBankCoverage() %...>% {
        # Get the results from the NCBI query
        uids <- c()
        for (i in .[[2]]) {
          uids <- c(uids, i)
        }
        uids
      }
    })
  
  
  # * NCBIDownloadFASTA --------------------------------------------------------
  
  # Download Fasta Files
  output$fileDownloadF <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Fasta_File", ".fasta", sep = "")
    },
    content = function(file) {
      uidsGet() %...>% {
        progLength <- length(.)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_Fasta <- c()
          for (uid in .) {
            err <- 1
            while (err == 1) {
              File <-
                tryCatch({
                  # Try catch for determining if homonyms exist, if they do fill
                  # up the errorPopupList and activate the errorHomonym Flag
                  if (!NCBIKeyFlag) {
                    # sleeping for 1/3 of a second each time gives us 3 queries 
                    # a second. If each user queries at this rate, we can 
                    # service 4-8 at the same time.
                    Sys.sleep(0.34) 
                  }
                  # Get the fasta file with that uid
                  File_fasta <-
                    entrez_fetch(db = "nucleotide",
                                 id = uid,
                                 rettype = "fasta") 
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            Vector_Fasta <-
              c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
            progress$inc(amount = 1)
          }
          # Writes the vector containing all the fasta file information into 
          # one fasta file
          write(Vector_Fasta, file) 
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )
  
  
  # * NCBIDownloadGenbank ------------------------------------------------------
  
  # Download NCBI Genbank Files
  output$fileDownloadG <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Genbank_File", ".gb", sep = "")
    },
    content = function(file) {
      uidsGet() %...>% {
        progLength <- length(.)
        progress <-
          AsyncProgress$new(
            session,
            min = 0,
            max = progLength,
            message = "Downloading",
            value = 0
          )
        future_promise({
          Vector_genbank <- c()
          for (uid in .) {
            err <- 1
            while (err == 1) {
              File <-
                tryCatch({
                  # Try catch for determining if homonyms exist, if they do fill
                  # up the errorPopupList and activate the errorHomonym Flag
                  if (!NCBIKeyFlag) {
                    # sleeping for 1/3 of a second each time gives us 3 queries 
                    # a second. If each user queries at this rate, we can 
                    # service 4-8 at the same time.
                    Sys.sleep(0.34) 
                  }
                  
                  # Get the genbank file with that uid
                  File_genbank <-
                    entrez_fetch(db = "nucleotide",
                                 id = uid,
                                 rettype = "gb")  
                  err <- 0
                }, error = function(err) {
                  err <<- 1
                })
            }
            
            # Append the genbank file to a vector
            Vector_genbank <-
              c(Vector_genbank, File_genbank) 
            progress$inc(amount = 1)
          }
          # Writes the vector containing all the genbank file information into
          # one genbank file
          write(Vector_genbank, file, append = TRUE) 
          progress$set(value = progLength)
          progress$close()
        })
      }
    }
  )
  
  # * NCBISummaryReportDownload ------------------------------------------------
  output$NCBIfileDownloadSD <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Summary_Report", ".csv", sep = "")
    },
    content = function(file) {
      promise_all(data_df = summary_report(1)) %...>% with({
        # Writes the dataframe to the CSV file
        write.csv(data_df, file)
      })
    }
  )
  
  # * NCBIBarcodeButtons -------------------------------------------------------
  
  observeEvent(input$barcodeOptionCO1, {
    # Detects when the specific barcode (in this case CO1) button has been 
    # pressed
    
    # If the input barcodeList is not empty (ie. the inputtextarea is not 
    # empty) then use the paste function to the add the barcode/s to the 
    # beginning
    if (input$barcodeList[[1]] != "") {
      # Updates the text area input adds the barcode/s to the beginning of 
      # whatever is already in it
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("(CO1; COI; COX1),", input$barcodeList)
      ) 
    }
    else {
      # Here since the textarea is empty we just set its value to the barcode/s
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "(CO1; COI; COX1)") 
    }
  })
  
  observeEvent(input$barcodeOption16S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("16S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "16S")
    }
  })
  
  observeEvent(input$barcodeOptionITS2, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("ITS2,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "ITS2")
    }
  })
  
  observeEvent(input$barcodeOption18S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("18S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "18S")
    }
  })
  
  observeEvent(input$barcodeOptionITS1, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("ITS1,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "ITS1")
    }
  })
  
  observeEvent(input$barcodeOptiontrnl, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("trnl,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "trnl")
    }
  })
  
  observeEvent(input$barcodeOption12S, {
    if (input$barcodeList[[1]] != "") {
      updateTextAreaInput(
        getDefaultReactiveDomain(),
        "barcodeList",
        value = paste("12S,", input$barcodeList)
      )
    }
    else {
      updateTextAreaInput(getDefaultReactiveDomain(), 
                          "barcodeList", 
                          value = "12S")
    }
  })
  
  
  # * NCBIOutputTables ---------------------------------------------------------
  
  
  #outputs:
  output$seqLenInputs <- renderUI(seqLenList())
  
  output$NCBIcoverageResults <- DT::renderDataTable({
    barcodes <- barcodeList()
    promise_all(data_df = matrixGet(), rows = NCBIorganismList()) %...>% with({
      DT::datatable(data_df, rownames = rows, colnames = barcodes)
    })
  })
  
  
  # * NCBInputFile -------------------------------------------------------------
  
  
  inputFileNCBI <- observeEvent(input$uploadNCBIButton, {
    isolate({
      req(input$uNCBIfile,
          file.exists(input$uNCBIfile$datapath))
      uploadinfo <-
        read.csv(input$uNCBIfile$datapath, header = TRUE)
      if (input$NCBIorganismList[[1]] != "") {
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "NCBIorganismList",
          value = c(
            head(uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""]),
            input$NCBIorganismList
          )
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "NCBIorganismList",
                            value = uploadinfo$OrganismNames[uploadinfo$OrganismNames != ""])
      }
      if (input$barcodeList[[1]] != "") {
        updateTextAreaInput(
          getDefaultReactiveDomain(),
          "barcodeList",
          value = c(head(uploadinfo$Barcodes[uploadinfo$Barcodes != ""]), 
                    input$barcodeList)
        )
      }
      else {
        updateTextAreaInput(getDefaultReactiveDomain(),
                            "barcodeList",
                            value = uploadinfo$Barcodes[uploadinfo$Barcodes != ""])
      }
    })
  })
  
  
  # * NCBIDownloadTable --------------------------------------------------------
  
  # Download NCBI table
  output$download <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Table", ".csv", sep = "")
    },
    content = function(file) {
      # Gets the column names for the matrix
      columns <- barcodeList() 
      NCBIorganismList() %...>% {
        #Gets the row names for the matrix
        rows <- . 
        matrixGet() %...>% {
          # Gets the matrix for the NCBI results
          future_promise({
            # Adds the column names to the matrix
            colnames(.) <- columns 
            # Adds the row names to the matrix
            rownames(.) <- rows 
            # Writes the dataframe to the CSV file
            write.csv(., file) 
          })
        }
      }
    }
    
    
  )
  
  # * NCBIDownloadSearchTerms --------------------------------------------------
  
  #Download Search Terms:
  output$downloadStatements <- downloadHandler(
    filename = function() {
      # Create the file and set its name
      paste("NCBI_Search_Statements", ".csv", sep = "")
    },
    content = function(file) {
      # Gets the column names for the matrix
      columns <- barcodeList() 
      NCBIorganismList() %...>% {
        #Gets the row names for the matrix
        rows <- . 
        matrixGetSearchTerms() %...>% {
          # Gets the matrix for the NCBI results
          future_promise({
            # Adds the column names to the matrix
            colnames(.) <- columns 
            # Adds the row names to the matrix
            rownames(.) <- rows 
            # Writes the dataframe to the CSV file
            write.csv(., file) 
          })
        }
      }
    }
  )
  
  # * SummaryReport ------------------------------------------------------------
  
  summary_report <- function(databaseFlag) {
    if (databaseFlag == 1) {
      matrixGet() %...>% {
        NCBIdata <- .
        NCBIorganismList() %...>% {
          # Gets the column names for the matrix
          columns <- barcodeList() 
          # Adds the column names to the matrix
          colnames(NCBIdata) <- columns 
          # Adds the row names to the matrix
          rownames(NCBIdata) <- . 
          
          # Convert to Dataframe
          NCBIdata <- as.data.frame(NCBIdata) 
          dataframe <- NCBIdata
          summary_report_dataframe(dataframe)
        }
      }
    } else {
      matrixGetCRUX() %...>% {
        # Gets the matrix for the Crux results
        CRUXmatrix <- . 
        organismListGet() %...>% {
          # Gets the column names for the matrix
          columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") 
          # Adds the column names to the matrix
          colnames(CRUXmatrix) <- columns 
          # Adds the row names to the matrix
          rownames(CRUXmatrix) <- .
          dataframe <- CRUXmatrix
          #calls convert_CRUX()s
          dataframe <- convert_CRUX(dataframe)
          summary_report_dataframe(dataframe)
        }
      }
    }
  }
  
  # * * DownloadDataframe ------------------------------------------------------
  
  summary_report_dataframe <- function(dataframe)
  {
    class(dataframe)
    class(dataframe[, 1])
    options(scipen = 999) #scientific notion
    new_row_names <- "total"
    # doesn't include column with taxa snames
    new_row_names <- c(new_row_names, colnames(dataframe))
    
    statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
    new_col_names <-
      c(
        "category",
        "number of sequences found",
        "percent of total sequences found",
        "num of organism with at least one sequence",
        "num of organisms with no sequences"
      )
    colnames(statistics_df) <- new_col_names
    #get list of columns + a column called "total"
    
    #add row names
    for (i in 1:length(new_row_names))
    {
      statistics_df[i, 1] <- new_row_names[i]
    }
    
    #doesn't include column with taxa snames
    barcodeSums <- colSums(dataframe) 
    
    Total_seq_found <- sum(barcodeSums)
    
    #hard code in the totals
    statistics_df[1, 2] <- Total_seq_found
    statistics_df[1, 3] <- 100
    
    for (i in 2:length(new_row_names))
    {
      x <- i - 1
      statistics_df[i, 2] <- barcodeSums[x]
      statistics_df[i, 3] <- (barcodeSums[x] / Total_seq_found)
    }
    
    #hard code in the totals
    output_of_which_rows_are_empty_and_arenot <-
      which_rows_are_empty_and_arenot(dataframe,-1)
    #list 2 is thee species without any seqs
    statistics_df[1, 5] <-
      length(output_of_which_rows_are_empty_and_arenot[[2]])
    #we know list 1 is the species with some seqs
    statistics_df[1, 4] <-
      length(output_of_which_rows_are_empty_and_arenot[[1]])
    
    for (i in 2:length(new_row_names))
    {
      x <- i - 1
      output_of_which_rows_are_empty_and_arenot <-
        which_rows_are_empty_and_arenot(dataframe, Which_Column = x)
      #list 2 is the species without any seqs
      statistics_df[i, 5] <-
        length(output_of_which_rows_are_empty_and_arenot[[2]])
      #we know list 1 is the species with some seqs
      statistics_df[i, 4] <-
        length(output_of_which_rows_are_empty_and_arenot[[1]])
    }
    statistics_df
  }
  
  
  # * * DownloadConvertCrux ----------------------------------------------
  
  
  convert_CRUX <-
    function(crux_output) 
      # Take a crux output matrix and  turn the characters "genus, spp, etc" 
      # into  0s/1s. This function is used by which_rows_are_empty_and_arenot()
             {
               crux_without_taxonomic_names <- crux_output
               crux_without_taxonomic_names <-
                 na.omit(crux_without_taxonomic_names)
               
               non_number_values <-
                 c('genus', 'family', 'class', 'order', 'error')
               
               ncols <- ncol(crux_output)
               nrows <- nrow(crux_output)
               
               for (i in 1:ncols)
               {
                 for (j in 1:nrows)
                 {
                   boolean <- 
                     crux_without_taxonomic_names[j, i] %in% non_number_values
                   
                   #if true, ie it matches genus, family, class, order
                   if (isTRUE(boolean))
                   {
                     crux_without_taxonomic_names[j, i] <- as.numeric(0)
                   } else {
                     crux_without_taxonomic_names[j, i] <- 
                       as.numeric(crux_output[j, i])
                   }
                 }
               }
               
               firstcolumn <- crux_without_taxonomic_names[, 1]
               
               crux_without_taxonomic_names <-
                 as.matrix(crux_without_taxonomic_names)
               if (nrows > 1) {
                 crux_without_taxonomic_names <-
                   as.data.frame(apply(crux_without_taxonomic_names, 2, as.numeric)) 
               } else {
                 crux_without_taxonomic_names <-
                   as.data.frame(t(as.numeric(crux_without_taxonomic_names)))
                 
                 # Gets the column names for the matrix
                 columns <-
                   list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
                 # Adds the column names to the matrix
                 colnames(crux_without_taxonomic_names) <- columns
               }
               crux_without_taxonomic_names
             }
             
  
  
   # * * DownloadEmptyRows -----------------------------------------------------
  
   # if which_column = -1 it means do all rows, if a column number is given the 
   # function will only run on said column of the dataframe returns list of 2 
   # lists, one of species with seqs, and one of species without any sequences
   which_rows_are_empty_and_arenot <-
     function(dataframe, Which_Column)
     {
       if (is.null(Which_Column))
       {
         Which_Column <- -1
       }
       Which_Column <- Which_Column
       #create two lists
       haveSomeSeq <- c()
       haveZeroSeq <- c()
       
       ncols <- ncol(dataframe)
       nrows <- nrow(dataframe)
       
       if (Which_Column < 0) {
         #we will skip the first column because it has names
         for (i in 1:nrows)
         {
           total <- 0
           for (j in 1:ncols)
           {
             total <- total + as.numeric(dataframe[i, j])
           }
           
           if (!is.null(total) && total > 0)
           {
             #add species name to list
             haveSomeSeq <- c(haveSomeSeq, dataframe[i, 1])
           } else
           {
             #add species name to list
             haveZeroSeq <- c(haveZeroSeq, dataframe[i, 1])
           }
         }
       } else {         #if a specific columnn
         #we will skip the first column because it has names
         for (i in 1:nrows)
         {
           seqs <- 0
           seqs <- 0 + as.numeric(dataframe[i, Which_Column])
           
           if (!is.null(seqs) && seqs > 0)
           {
             #add species name to list
             haveSomeSeq <- c(haveSomeSeq, dataframe[i, 1]) 
           } else
           {
             #add species name to list
             haveZeroSeq <- c(haveZeroSeq, dataframe[i, 1])
           }
         }
       }
       if (Which_Column < 0) {
         results <-
           list(HaveSomeSeqs = haveSomeSeq, haveZeroSeqs = haveZeroSeq)
         results <- as.matrix(results)
       } else
       {
         COLNam <- colnames(dataframe)
         column_name <- paste0("Have", COLNam[Which_Column], "Seq")
         results <-
           list(
             single_Barcode_haveSomeseq = haveSomeSeq,
             single_Barcode_haveZeroSeqs = haveZeroSeq
           )
         results <- as.matrix(results)
       }
       results
     }
             
})
  