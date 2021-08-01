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

shinyServer(function(input, output) {
    

# CRUX --------------------------------------------------------------------


# * CRUXSearchButton --------------------------------------------------------

    cruxOrgSearch <- eventReactive(input$searchButton, { #When searchButton clicked, update CruxOrgSearch to return the value input into CRUXorganismList 
        input$CRUXorganismList #Returns as a string
    })
    

# * CRUXStrToList -----------------------------------------------------------
    
    cruxOrganismList <- reactive({ #Converts string from cruxOrgSearch into a list of Strings
        organismList <- strsplit(cruxOrgSearch(), ",")[[1]] #separate based on commas
        if(input$CRUXtaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #help user with various naming issues (spelling, synonyms, etc.)
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
    

# * CRUXCoverage ------------------------------------------------------------

    cruxCoverage <- reactive({
        organismList <- cruxOrganismList()
        organismListLength <- length(organismList)
        
        validate(
            need(organismListLength > 0, 'Please name at least one organism')
        )
        
        dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
        
        taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
        
        searchTerm <- ""
        searchResult <- 0
        results <- c()
        for(organism in organismList){
            searchTerm <- tax_name(query= organism, get = c("genus", "family", "order", "class","phylum", "domain"), db= "ncbi")
            for(table in dbList){
                # 
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=organism))
                if(nrow(location) == 0){
                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,3]))
                    if(nrow(location) == 0){
                        location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,4]))
                        if(nrow(location)==0){
                            location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,5]))
                            if(nrow(location) ==0){
                                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,6]))
                                if(nrow(location)==0){
                                    location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x or phylum= :x or classis= :x or ordo= :x or familia= :x or genus= :x or genusspecies= :x"), params=list(x=searchTerm[1,7]))
                                    results <- c(results, nrow(location))
                                } else { results <- c(results, "class")}
                            } else {results <- c(results, "order")}
                        } else {results <- c(results, "family")}
                    }else {results <- c(results, "genus") }
                } else {results <- c(results, toString(nrow(location)))}
            }
            
        }
        dbDisconnect(taxaDB)
        # unlink("taxa-db.sqlite")
        
        data <- matrix(results, nrow = organismListLength, ncol = length(dbList), byrow = TRUE) #store vector results in data matrix
        data #return data matrix
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
            paste(input$CRUXorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- list("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S") # Gets the column names for the matrix
            CRUXmatrix <- cruxCoverage() # Gets the matrix for the Crux results
            colnames(CRUXmatrix) <- columns # Adds the column names to the matrix
            rownames(CRUXmatrix) <- cruxOrganismList() # Adds the row names to the matrix
            write.csv(CRUXmatrix, file) # Writes the matrix to the CSV file
        }
    )
    

# * CRUXOutput --------------------------------------------------------------

    output$CRUXcoverageResults <- DT::renderDataTable(
      cruxCoverage(), rownames = cruxOrganismList(), colnames = c("18S", "16S", "PITS", "CO1", "FITS", "trnL", "Vert12S")
      
    )
    

# NCBI --------------------------------------------------------------------


# * NCBISearchButton --------------------------------------------------------
    
    NCBISearch <- eventReactive(input$NCBIsearchButton, { #When searchButton clicked, update NCBIOrgSearch to return the value input into NCBIorganismList 
        list(input$NCBIorganismList, input$barcodeList) #Returns as a string
    })
    

# * NCBIStrToList -----------------------------------------------------------

    NCBIorganismList <- reactive({ #Converts string from NCBIorganismList into a list of Strings
        organismList <- strsplit(NCBISearch()[[1]], ",")[[1]] #separate based on commas
        if(input$NCBItaxizeOption){ #if the taxize option is selected
            taxize_organism_list <- c() #initialize an empty vector
            
            for(i in 1:length(organismList))
            {
                organism <- trimws(organismList[[i]], "b") #trim both leading and trailing whitespace
                NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) #4 = NCBI
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
    

# * NCBIBarcodeList ---------------------------------------------------------

    barcodeList <- reactive({
        barcodeList <- strsplit(NCBISearch()[[2]], ",") #separate based on comma
        barcodeList[[1]]
    })
    

# * NCBISequenceLength ------------------------------------------------------

    seqLenList <- reactive({ #list of sequence length specifications
        if(input$seqLengthOption){ #only present if the option is selected
            textList <- list()
            for(marker in barcodeList()){ #allow the user to specify a different length for every barcode
                textList <- list(textList, numericInput(marker, paste("Minimum sequence length for", marker), 500, min= 0)) #add a numeric input
            }
            textList #return the list of numeric inputs
        }
    })
    

# * NCBICoverage ------------------------------------------------------------
    
    genBankCoverage <- reactive({
        
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        
        codeListLength <- length(barcodeList()) 
        validate( #verify that the  user has typed things into both inputs
            need(organismListLength > 0, 'Please name at least one organism'),
            need(codeListLength > 0, 'Please choose at least one barcode')
        )
        searchTerm <- ""
        searchResult <- 0
        err <- 0
        countResults <- list() #initialize empty vector
        uids <- list()
        searchTerms <- list() #list of search terms
        for(organism in organismList){
            for(code in barcodeList()){
                if(input$NCBISearchOptionOrgn){
                    searchTerm <- paste(organism, "[ORGN] AND ", sep="") #our query to GenBank
                }
                else {
                    searchTerm <- paste(organism, " AND ", sep="") #our non-Metadata query to GenBank
                }
                if(input$NCBISearchOptionGene) {
                    searchTerm <- paste(searchTerm, code, "[GENE]", sep="") #our query to GenBank
                }
                else {
                    searchTerm <- paste(searchTerm, code, sep="") #our query to GenBank
                }
                if(input$seqLengthOption){
                    searchTerm <- paste(searchTerm, " AND ", input[[code]],":99999999[SLEN]", sep="") #if the user specified sequence length
                }
                searchResult <- tryCatch({
                  searchResult <- entrez_search(db = "nucleotide", term = searchTerm, retmax = 5) #only get back the number of search results
                }, error = function(err) {
                  results <- c(results, "error", "error", "error", "error", "error", "error", "error")
                  err <- 1
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
    

# * NCBIMatrix --------------------------------------------------------------
    
    matrixGet <- reactive({ # creates and returns the matrix to be displayed with the count
        organismList <- NCBIorganismList() #get species and barcode inputs
        organismListLength <- length(organismList)
        codeListLength <- length(barcodeList())
        results <- genBankCoverage() # Get the results from the NCBI query
        count <- c()
        for (i in results[[1]]) {
            count <- c(count, i)
        }
        data <- matrix(count, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
        data
    })
    

# * NCBITableOutput ---------------------------------------------------------
    
    matrixGetSearchTerms <- reactive({ # creates and returns the matrix to be displayed with the count
      organismList <- NCBIorganismList() #get species and barcode inputs
      organismListLength <- length(organismList)
      codeListLength <- length(barcodeList())
      results <- genBankCoverage() # Get the results from the NCBI query
      SearchStatements <- c()
      for (i in results[[3]]) { #3 is the 3rd list in genBankCovearage aka the searchterms list
        SearchStatements <- c(SearchStatements, i)
      }
      data <- matrix(SearchStatements, nrow = organismListLength, ncol = codeListLength, byrow = TRUE) #convert results vector to dataframe
      data
    })
    

# *   NCBIGetIDs --------------------------------------------------------------

    uidsGet <- reactive({ # Returns the uids stored in the results from the NCBi query
        uids <- c()
        results <- genBankCoverage() # Get the results from the NCBI query
        for (i in results[[2]]) {
            uids <- c(uids, i)
        }
        uids
    })
    

# * NCBIDownloadFASTA -------------------------------------------------------
  
    # Download NCBI table
    output$fileDownloadF <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("TEST", ".fasta", sep = "")
        },
        content = function(file) {
            uids <- uidsGet()
            progLength <- length(uids)
            shiny::withProgress(message="Downloading", value=0, {
                Vector_Fasta <- c()
                for (uid in uids) {
                    File_fasta <- entrez_fetch(db = "nucleotide", id = uid, rettype = "fasta") # Get the fasta file with that uid
                    Vector_Fasta <- c(Vector_Fasta, File_fasta) # Append the fasta file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_Fasta, file) # Writes the vector containing all the fasta file information into one fasta file
                shiny::incProgress(1/progLength)
            })
            
        }
    )
    

# * NCBIDownloadGenbank -----------------------------------------------------

    # Download NCBI Genbank
    output$fileDownloadG <- downloadHandler(
        filename = function() { # Create the file and set its name
            paste("GenbankTEST", ".gb", sep = "")
        },
        content = function(file) {
            uids <- uidsGet()
            progLength <- length(uids)
            shiny::withProgress(message="Downloading", value=0,{
                Vector_genbank <- c()
                for (uid in uids) {
                    File_genbank <- entrez_fetch(db = "nucleotide", id = uid, rettype = "gb")  # Get the genbank file with that uid
                    Vector_genbank <- c(Vector_genbank, File_genbank) # Append the genbank file to a vector
                    shiny::incProgress(1/progLength)
                }
                write(Vector_genbank, file, append=TRUE) # Writes the vector containing all the genbank file information into one genbank file
                shiny::incProgress(1/progLength)
            })
            
        }
    )
    
    output$fileDownloadSD <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste("Summary Data", ".csv", sep = "")
      },
      content = function(file) {
          summary_data <- summary_report(matrixGet())
          write.csv(summary_data, file) # Writes the dataframe to the CSV file
        })
    
# * NCBIDownloadSummaryReport ----------------------------------------------
    
    summary_report <- function(dataframe)
    {
      print("hello")
      
      columns <- barcodeList() # Gets the column names for the matrix
      NCBIdata <- matrixGet() # Gets the matrix for the NCBI results
      colnames(NCBIdata) <- columns # Adds the column names to the matrix
      rownames(NCBIdata) <- NCBIorganismList() # Adds the row names to the matrix
      NCBIdata <- as.data.frame(NCBIdata)
      print(NCBIdata)
      print(typeof(NCBIdata))
      colnms=c("CO1", "COX1", "COI")
      print(NCBIdata[, colnms])
      #combine multiple COI NCBI names into 1
      #colnms=c("CO1", "COX1", "COI")
      NCBIdata$Combined_COI<-rowSums(NCBIdata[,colnms])
      #removing columns that were combined
      drops <- c("CO1", "COX1", "COI")
      #NCBIdata<-NCBIdata[ , !(names(NCBIdata) %in% drops)]
      print(NCBIdata)
      
      NCBIdata
      # #calls convert_CRUX()s
      # dataframe <- convert_CRUX(dataframe)
      # class(dataframe)
      # class(dataframe[,1])
      # class(dataframe[,2])
      # options(scipen=999) #scientific notion  
      # 
      # new_row_names <- "total"
      # new_row_names<-  c(new_row_names, colnames(dataframe[-1]))#doesn't include column with taxa snames
      # print(new_row_names)
      # 
      # statistics_df <- data.frame(matrix(ncol = 5, nrow = 0))
      # new_col_names <- c("category","number of sequences found", "percent of total sequences found", "num of organism with at least one sequence", "num of organisms with no sequences")
      # colnames(statistics_df) <- new_col_names
      # #get list of columns + a column callede "total"
      # 
      # print("test1")
      # #add row names
      # for(i in 1:length(new_row_names))
      # {
      #   statistics_df[i,1]<-new_row_names[i]
      # }
      # print("test1.5")
      # barcodeSums <- colSums(dataframe[,-1]) #doesn't include column with taxa snames
      # print("test1.6")
      # 
      # Total_seq_found <- sum(barcodeSums)
      # print("test1.7")
      # 
      # #hard code in the totals
      # statistics_df[1,2] <- Total_seq_found
      # print("test1.8")
      # statistics_df[1,3] <- 100
      # print("test2")
      # for(i in 2:length(new_row_names))
      # {
      #   x <- i-1
      #   statistics_df[i,2] <- barcodeSums[x]
      #   statistics_df[i,3] <- (barcodeSums[x]/Total_seq_found)
      # }
      # print(barcodeSums)
      # 
      # #hard code in the totals
      # output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, -1)
      # statistics_df[1,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])    #list 2 is thee species without any seqs
      # statistics_df[1,4] <-length(output_of_which_rows_are_empty_and_arenot[[1]])   #we know list 1 is the species with some seqs
      # print("almosthome")
      # 
      # for(i in 2:length(new_row_names))
      # {
      #   x <- i#-1
      #   output_of_which_rows_are_empty_and_arenot <- which_rows_are_empty_and_arenot(dataframe, Which_Column = x)
      #   statistics_df[i,5] <- length(output_of_which_rows_are_empty_and_arenot[[2]])     #list 2 is the species without any seqs 
      #   statistics_df[i,4] <- length(output_of_which_rows_are_empty_and_arenot[[1]])  #we know list 1 is the species with some seqs
      # }
      # statistics_df
    }
    
    # * * NCBIDownloadConvertCrux ----------------------------------------------
    
    
    convert_CRUX <- function(crux_output #take a crux output matrix and  turn the characters "genus, spp, etc" into  0s/1s
                             #this function is used by which_rows_are_empty_and_arenot()
    )
    {
      print("I'm in")
      crux_without_taxonomic_names <- crux_output
      crux_without_taxonomic_names<-  na.omit(crux_without_taxonomic_names)
      
      non_number_values <- c('genus', 'family', 'class', 'order')
      
      ncols <- ncol(crux_output)
      nrows <- nrow(crux_output)
      print("hello")
      
      for(i in 1:ncols)
      {
        for(j in 1:nrows)
        {
          boolean <- crux_without_taxonomic_names[j,i]%in%non_number_values
          #  if(!is.null(boolean) &  isTRUE(boolean)) #if true, ie it matches genus, family, class, order
          if(isTRUE(boolean)) #if true, ie it matches genus, family, class, order
          {
            # print(paste(i,'space',j))
            #    print(class(crux_without_taxonomic_names[j,i]))
            crux_without_taxonomic_names[j,i] <- as.numeric(0)
            #       print(class(crux_without_taxonomic_names[j,i]))
          }
          print(class(crux_without_taxonomic_names[j,i]))
          
          # print(paste("WORKING", i))
        }
      }
      # print(crux_without_taxonomic_names)
      firstcolumn <- crux_without_taxonomic_names[,1]
      crux_without_taxonomic_names <- as.matrix(crux_without_taxonomic_names[,-1])
      
      crux_without_taxonomic_names <- as.data.frame(apply(crux_without_taxonomic_names, 2, as.numeric)) 
      
      print("I'm outish1")
      
      print(class(crux_without_taxonomic_names[2,2]))
      print(class(crux_without_taxonomic_names[1,2]))
      
      crux_without_taxonomic_names<-cbind.data.frame(firstcolumn,crux_without_taxonomic_names)
      print("I'm outish")
      
      print(class(crux_without_taxonomic_names[2,2]))
      print(class(crux_without_taxonomic_names[1,2]))
      
      crux_without_taxonomic_names
    }
    
    # * * NCBIDownloadEmptyRows ----------------------------------------------
    
    which_rows_are_empty_and_arenot <- function(dataframe, Which_Column#-1 means do all rows, a column number is gvenn the function will only run on said column of the dataframe
    ) #returns list of 2 lists, one of species with seqs, and one of species without any sequences
    {
      #print(Which_Column)
      if(is.null(Which_Column))
      {
        Which_Column <- -1
      }
      Which_Column <- Which_Column
      dataframe <- convert_CRUX(dataframe)
      
      #create two lists
      haveSomeSeq <- c()
      haveZeroSeq <- c()
      
      ncols <- ncol(dataframe)
      nrows <- nrow(dataframe)
      
      if(Which_Column < 0){
        
        for(i in 1:nrows) #we will skip the first column because it has names
        {
          total <- 0
          for(j in 2:ncols)
          {
            total <- total + as.numeric(dataframe[i,j])
          }
          
          if(!is.null(total) && total > 0)
          {
            haveSomeSeq <- c(haveSomeSeq, dataframe[i,1]) #add species name to list
            #haveSomeSeq <- c(haveSomeSeq, total)
            
            #     print(dataframe[i,1])
          } else
          {
            #    print(dataframe[i,1])
            #haveZeroSeq <- c(haveZeroSeq, total)
            # print(i)
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
            #haveSomeSeq <- c(haveSomeSeq, dataframe[i,Which_Column])
            haveSomeSeq <- c(haveSomeSeq, dataframe[i,1]) #add species name to list
            
            #  print(dataframe[i,Which_Column])
            #print(seqs)
          } else
          {
            # print(seqs)
            # haveZeroSeq <- c(haveZeroSeq, dataframe[i,Which_Column])
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
        #print(column_name)
        results <- list(single_Barcode_haveSomeseq = haveSomeSeq, single_Barcode_haveZeroSeqs =haveZeroSeq)
        results<- as.matrix(results)
      }
      results
    }

# * NCBIBarcodeButtons -----------------------------------------------------

    observeEvent(input$barcodeOptionCO1,{ # Detects when the specific barcode (in this case CO1) button has been pressed
        if(input$barcodeList[[1]] != "") { # If the input barcodeList is not empty (ie. the inputtextarea is not empty) then use the paste function to the add the barcode/s to the beginning
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = paste("CO1, COI, COX1,", input$barcodeList)) # Updates the text area input adds the barcode/s to the beginning of whatever is already in it
        }
        else {
            updateTextAreaInput(getDefaultReactiveDomain(), "barcodeList", value = "CO1, COI, COX1") # Here since the textarea is empty we just set its value to the barcode/s
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
    
    output$NCBIcoverageResults <- DT::renderDataTable(
        matrixGet(), rownames = NCBIorganismList(), colnames = barcodeList()
    )
  

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
            paste(input$NCBIorganismList, ".csv", sep = "")
        },
        content = function(file) {
            columns <- barcodeList() # Gets the column names for the matrix
            NCBImatrix <- matrixGet() # Gets the matrix for the NCBI results
            colnames(NCBImatrix) <- columns # Adds the column names to the matrix

            rownames(NCBImatrix) <- NCBIorganismList() # Adds the row names to the matrix
            write.csv(NCBImatrix, file) # Writes the dataframe to the CSV file
        }
        
        
    )

# * NCBIDownloadSearchTerms -------------------------------------------------

    #Download Search Terms:
    output$downloadStatements <- downloadHandler(
      filename = function() { # Create the file and set its name
        paste(input$NCBIorganismList, ".csv", sep = "")
      },
      content = function(file) {
        columns <- barcodeList() # Gets the column names for the matrix
        NCBImatrix <- matrixGetSearchTerms() # Gets the matrix for the NCBI results
        colnames(NCBImatrix) <- columns # Adds the column names to the matrix
        
        rownames(NCBImatrix) <- NCBIorganismList() # Adds the row names to the matrix
        write.csv(NCBImatrix, file) # Writes the dataframe to the CSV file
      }
    )
    
})
