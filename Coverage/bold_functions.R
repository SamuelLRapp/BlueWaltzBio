import(dplyr)

is_missing <- function(val) {
  return(val == "" | is.na(val))
}

# * BOLDCountryFilter -------------------------------------------


country_summary <- function(bold_coverage_df){
    #table() gets the number of times a species and country co-occur
    #as.data.frame.matrix() turns that into a co-occurrence count dataframe
    #this idea is from, and nicely explained, here: https://stackoverflow.com/a/49217363
    summary_df <- bold_coverage_df %>%
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


#####
# Input:
#   - The BOLD Coverage Matrix from bold_coverage function
#   - A list of the filtered countries
#.  - A list of all countries
absentMatrix <- function(bold_coverage, countries){
    # Get data frame with species as rows and countries as columns (with each number of sequences)
    country_summary_df <- country_summary(bold_coverage)
    country_filtered <- country_summary_df %>% filter(if_all(all_of(countries), ~ . == 0) )
    
    # the apply function, gets the top 3 countries with the most entries for each row (species)
    absent_top_countries_values <- apply(country_filtered, 1, function(x) sort(x, decreasing = TRUE)[1:3])
    absent_top_countries <- apply(country_filtered, 1, function(x) names(sort(x, decreasing = TRUE))[1:3])
    absent_top_countries[absent_top_countries_values == 0] <- 'NA'

    # Check if the table is empty or not
    if (is.null(absent_top_countries)){
      top_countries_df <- data.frame(matrix(ncol = 3, nrow = 0))
    } else {
      top_countries_df <- as.data.frame(t(absent_top_countries))
    }
    
    # Name the columns and return
    colnames(top_countries_df) <- c("1st", "2nd", "3rd")
    top_countries_df
}  



# * SummaryReport ------------------------------------------------------------

barcode_summary <- function(bold_coverage) {
    summary_df <- bold_coverage %>%
      subset(subset = !is_missing(species_name) & !is_missing(markercode),
             select = c("species_name", "markercode")) %>%
      table %>%
      as.data.frame.matrix
    
    summary_df
}

reduce_barcode_summary <- function(b_summary) {
    #number of non-zeros in each column
    summary <- b_summary
    count <- apply(summary, 2, function(c)sum(c!=0))
    sums <- apply(summary, 2, sum)
  
    #calculate and sort by relavance
    calculated <- names(rev(sort(sums * count)))
  
    summary <- subset(summary, select = (calculated))
    summary
}

missingSpecies <- function(missingList) {
  #check for empty missing species list
  if (is.null(missingList[["Missing Species"]])) {
    missing_df <- data.frame(" " <- c("NULL"))
  } else {
    missing_df <- data.frame(" " <- missingList[["Missing Species"]])
  }
  
  #remove column name for species column
  colnames(missing_df) <- c(" ")
  
  missing_df
}

