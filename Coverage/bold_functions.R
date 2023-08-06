import(dplyr)

is_missing <- function(val) {
  return(val == "" | is.na(val))
}

# * BOLDCountryFilter -------------------------------------------


country_summary <- function(bold_coverage){
    #table() gets the number of times a species and country co-occur
    #as.data.frame.matrix() turns that into a co-occurrence count dataframe
    #this idea is from, and nicely explained, here: https://stackoverflow.com/a/49217363
    summary_df <- bold_coverage %>%
      subset(subset = !is_missing(species_name) & !is_missing(markercode) & !is_missing(country),
             select = c("species_name", "country")) %>%
      table %>%
      as.data.frame.matrix
    
    summary_df
}

# * NA Filter -------------------------------------------


naBarcodes <- function(bold_coverage){
  #summarise gets the processids and converts them to a list string with paste
  #for each group (species_name)
  #check.names = FALSE in data.frame makes sure spaces aren't replaced with '.'
  summary_df <- bold_coverage %>%
    subset(subset = !is_missing(species_name) & !is_missing(processid) & is_missing(markercode),
    select = c("species_name", "processid")) %>%
    group_by(species_name) %>%
    summarise("Entries where Barcode was NA" = paste(processid, collapse = ", ")) %>%
    data.frame(row.names = .$species_name, check.names = FALSE)
  
  #remove species_name column
  summary_df$species_name <- NULL 

  summary_df
}

presentMatrix <- function(bold_coverage, countries){
    present_df <- country_summary(bold_coverage)
    
    #remove all columns that are not in filter
    present_df <- present_df[ , which(names(present_df) %in% countries), drop=FALSE]
    
    #remove all rows that have all 0s as values and return
    #Need to include drop=false to prevent R from dropping dataframe structure when numcolumns is 1, otherwise rowsums will complain
    #https://stackoverflow.com/questions/32330818/r-row-sums-for-1-or-more-columns
    present_df <- present_df[rowSums(present_df[drop=FALSE])>0, ,drop=FALSE]
    present_df
}

absentMatrix <- function(bold_coverage, countries, country_names){
    # Get data frame with species as rows and countries as columns (with each number of sequences)
    country_summary_df <- country_summary(bold_coverage)
    country_filtered <- country_summary_df %>% filter(across(all_of(countries), ~ . == 0))

    # the apply funcyion, gets the top 3 countries with the most entries for each row (species)
    absent_top_countries <- apply(country_filtered, 1, function(x) names(sort(x, decreasing = TRUE)[1:3]))
    
    # Set the names of the vector as the species names
    names(absent_top_countries) <- absent_top_countries[, 1]
    top_countries_df <- as.data.frame(t(absent_top_countries))
    colnames(top_countries_df) <- c("1st", "2nd", "3rd")
    top_countries_df
}  



# * SummaryReport ------------------------------------------------------------

barcode_summary <- function(bold_coverage) {
    summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
    records_bold <- bold_coverage
    if (length(records_bold$species_name) == 0){
        return(summary_df)  
    }
    for(i in 1:length(records_bold$species_name)){
        #if species name not in dataframe
        if (!is.na(records_bold$species_name[i]) && (records_bold$species_name[i] != "") && !(records_bold$species_name[i] %in% rownames(summary_df)))
            #add a row to summary_df
            summary_df[records_bold$species_name[i],] <- integer(ncol(summary_df))
        #add data to summary_df to get summary data
        if (!is.na(records_bold$markercode[i]) && records_bold$markercode[i] != '' && (records_bold$species_name[i] != "")){
            #if markercode is not yet in the dataframe, initiate new col
            if (!(records_bold$markercode[i] %in% colnames(summary_df))){
                #create a new column of 0s
                summary_df[records_bold$markercode[i]] <- integer(nrow(summary_df))
            }
            #add 1 to existing count
            summary_df[records_bold$species_name[i], records_bold$markercode[i]] <- summary_df[records_bold$species_name[i], records_bold$markercode[i]] + 1
        }
        else {
          
        }
    }
    
    summary_df
}

reduce_barcode_summary <- function(b_summary) {
    #number of non-zeros in each column
    summary <- b_summary
    count <- apply(summary, 2, function(c)sum(c!=0))
    sums <- apply(summary, 2, sum)
    
    #find most representative 
    
    #result <- c()
    #for (i in 1:length(sums)){
    #    result <- sums[[i]] * count[[i]]
    # }
    #print(c)
    
    
    #calculate and sort by relavance
    calculated <- names(rev(sort(sums * count)))
    
    #if (ncol(summary) > 3){
    #  summary <- subset(summary, select = c(names(calculated[1]), names(calculated[2]), names(calculated[3])))
    #}
    summary <- subset(summary, select = (calculated))
    summary
}

missingSpecies <- function(missingList) {
  missing_df <- data.frame(matrix(ncol = 0, nrow = 0))
  newStr <- ""
  for (i in 1:length(missingList)) {
    if (newStr == "") {
      newStr <- paste(newStr, missingList[i], sep="")
    } else {
      newStr <- paste(newStr, missingList[i], sep=", ")
    }
  }
  print(newStr)
  missing_df[" ", " "] <- newStr
  missing_df
}

