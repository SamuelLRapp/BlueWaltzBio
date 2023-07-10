import(future)
import(promises)
import(ipc)
import(shiny)
import(rentrez)
import(taxize)

taxizeHelper <- function(orgSearch, taxizeSelected){
    if(orgSearch == ""){
      return(c())
    }
    # trim both leading and trailing whitespace
    orgSearch <- trimws(orgSearch[[1]], "b")
    # remove any newline delimiting
    orgSearch <- gsub("\n", ",", orgSearch)
    # separate based on commas
    organismList <- strsplit(orgSearch, ",")[[1]]
    
    # trim both leading and trailing whitespace
    for(i in 1:length(organismList)){
      organismList[[i]] <- trimws(organismList[[i]], "b")
    }
    # deduplicate
    organismList <- unique(organismList[organismList != ""])
    
    #if the taxize option is selected
    if(taxizeSelected){ 
      #initialize an empty vector
      taxize_organism_list <- c() 
      
      for(organism in organismList)
      {
        NCBI_names <- 1
        tries <- 1
        while(NCBI_names == 1) {
          tries <- tries + 1
          if (tries == 6){
            return(list(status=1,results=organismList))
          }
          NCBI_names <- tryCatch({
            # sleeping for 1/3 of a second each time gives us 3 queries a 
            # second. If each user queries at this rate, we can service 4-8 at 
            # the same time.
            Sys.sleep(0.34) 
            # help user with various naming issues (spelling, synonyms, etc.)
            NCBI_names <- gnr_resolve(sci = organism, data_source_ids = 4) 
            NCBI_names
          }, error = function(err) {
            err <- 1
          })
        }
        
        # get number of rows in dataframe
        row_count <- nrow(NCBI_names)
        
        #If a legitimate name was found
        if(row_count > 0) 
        {
          for(j in 1:row_count)
          {
            #Store each matched name in taxa_name
            taxa_name <- NCBI_names[[j,3]] 
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
      list(status=0,results=taxize_organism_list)
      
    } else{
      #if the checkbox wasn't selected, return the list as is
      organismList 
      list(status=0,results=organismList)
    }
}