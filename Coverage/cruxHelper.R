import(shiny)
import(taxize)

cruxSearch <- function(results, searchTerm, organism) {
  dbList <- list("MB18S", "MB16S", "MBPITS", "MBCO1","MBFITS","MBtrnL","MB12S") #List of db tables each representing a marker
  taxaDB <- dbConnect(RSQLite::SQLite(), "taxa-db.sqlite") #connect to the db
  #results <- c()
  for(table in dbList){
    #
    if(table=="MB12S") print(searchTerm)
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
              if(nrow(location)==0){
                location <- dbGetQuery(taxaDB, paste("SELECT * from ",table," where regio= :x"), params=list(x=searchTerm[1,8]))
                if(nrow(location)==0){results <- c(results, 0)} else {results <- c(results, "kingdom")}
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


checkForHomonyms <- function(organism){
  errorHomonym <- 0
  search <- c()
  returnStatus <- 0
  newOrgList <- c(organism)
  search <- tryCatch({ # Try catch for determining if homonyms exist, if they do fill up the errorPopupList and activate the errorHomonym Flag
    Sys.sleep(0.34)
    search <- get_uid_(sci_com = organism) # Check to see if there are homonyms
  }, error = function(err) {
    errorHomonym <<- 1
    returnStatus <<- 2
  })
  if(errorHomonym == 1){
    errorPopupList <- c(errorPopupList, organism)
  }
  else if(is.null(search[[1]])){
    results <- c(results, "0", "0", "0", "0", "0", "0", "0")
    newOrgList <- c(newOrgList, organism)
    next
  }
  c(returnStatus, search)
}

getHomomynmSearchTerm <- function(organism, search){
  errorHomonym <- 0

  # Creating the same format as the other organisms so the Crux search can be performed correctly
  hierarchy <- tryCatch({ # Try catch for when we know there are homonyms but we dont know which homonyms yet, if there is an error fill up errorPopupListFound and activate the errorHomonym Flag
    Sys.sleep(0.34)
    hierarchy <- classification(search[[1]]$uid[i], db = "ncbi")[[1]] # Check to see if there are homonyms
    hierarchy
  }, error = function(err) {
    errorHomonym <<- 1
  })
  match <- hierarchy$name[match(tolower(c("genus", "family", "order", "class","phylum", "domain")), tolower(hierarchy$rank))]
  query <- c("db", "query", "genus", "family", "order", "class","phylum", "domain")
  match <- c("ncbi", organism, match)
  searchTerm <- stats::setNames(
    data.frame(t(match), stringsAsFactors = FALSE), 
    query
  )
  c(searchTerm, errorHomonym)
}