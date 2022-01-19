import(future)
import(promises)
import(ipc)
import(shiny)
import(rentrez)
import(taxize)

taxizeHelper <- function(orgSearch, taxizeSelected){
  future_promise({
    organismList <- strsplit(orgSearch[[1]], ",")[[1]] #separate based on commas
    organismList <- unique(organismList[organismList != ""])
    if(taxizeSelected){ #if the taxize option is selected
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
}