# -----------------------------------------------------------------------------#
# Name: bold_functions.R
# Last Date Updated : September 22, 2024
#
# This code was build by Sriram Ramesh and Jorge Tapias Gomez
# With help from Dickson Chung and Zia Truong
#
# In collaboration with Samuel Rapp, Benjamine Levine, and Daniel Tapias Gomez
# Also, a big thanks to all those that helped us along the project.
# -----------------------------------------------------------------------------#

suppressPackageStartupMessages({
  import(dplyr)
})

is_missing <- function(val) {
  return(val == "" | is.na(val))
}

# * BOLDCountryFilter -------------------------------------------

## ----#
# Country Summary function
#   - Input: 
#         bold_coverage_df: The dataframe returned by the bold api with all the results 
#   - Output:
#         New dataframe with the number of times a species and country co-occur
#         Used for our country summary data displaying #of species per country 
## ----#
country_summary <- function(bold_coverage_df){
    # The table() gets the number of times a species and country co-occur
    # as.data.frame.matrix() turns that into a co-occurrence count dataframe
    # this idea is from, and nicely explained, here: https://stackoverflow.com/a/49217363
    summary_df <- bold_coverage_df %>%
      subset(subset = !is_missing(species_name) & !is_missing(markercode) & !is_missing(country),
             select = c("species_name", "country")) %>%
      table %>%
      as.data.frame.matrix
    
    summary_df
}

# * NA Filter -------------------------------------------
## ----#
# Country Summary function
#   - Input: 
#         bold_coverage_df: The dataframe returned by the bold api with all the results 
#                           (filtered if the user activated any filter)
#   - Output:
#         New dataframe with the number of times a species and country co-occur
#         Used for our country summary data displaying #of species per country 
## ----#
naBarcodes <- function(bold_coverage){
  # Summarise gets the processids and converts them to a list string with paste
  # for each group (species_name)
  # check.names = FALSE in data.frame makes sure spaces aren't replaced with '.'
  summary_df <- bold_coverage %>%
    subset(subset = !is_missing(species_name) & !is_missing(processid) & is_missing(markercode),
    select = c("species_name", "processid")) %>%
    group_by(species_name) %>%
    summarise("Entries where Barcode was NA" = paste(processid, collapse = ", ")) %>%
    data.frame(row.names = .$species_name, check.names = FALSE)
  
  # Remove species_name column
  summary_df$species_name <- NULL 

  summary_df
}

## ----#
# Present Matrix function
#   - Input: 
#         bold_coverage: The dataframe returned by the bold api with all the results
#                        (filtered if the user activated any filter)
#         countries: The countries the user has chosen to filter by
#   - Output:
#         New dataframe that contains only the countries the user desires
## ----#
presentMatrix <- function(bold_coverage, countries){
    present_df <- country_summary(bold_coverage)
    
    # Remove all columns that are not in filter
    if (!is.null(countries)) {
      present_df <- present_df[ , which(names(present_df) %in% countries), drop=FALSE]
    }
    
    # Remove all rows that have all 0s as values and return
    # Need to include drop=false to prevent R from dropping dataframe structure when numcolumns is 1, otherwise rowsums will complain
    # https://stackoverflow.com/questions/32330818/r-row-sums-for-1-or-more-columns
    present_df <- present_df[rowSums(present_df[drop=FALSE])>0, ,drop=FALSE]
    present_df
}

## ----#
# Absent Matrix function
#   - Input: 
#       bold_coverage: The dataframe returned by the bold api with all the results
#                      (filtered if the user activated any filter)
#       countries: The countries the user has chosen to filter by
#   - Output:
#         New dataframe that, for those organism that have no results in the countries filtered
#         will return the top 3 countries that do contain results for that organism
#         This is meant to aid users to find good coverage
## ----#
absentMatrix <- function(bold_coverage, countries){
    # Get data frame with species as rows and countries as columns (with each number of sequences)
    if (!is.null(countries)) {
      country_summary_df <- country_summary(bold_coverage)
      country_filtered <- country_summary_df %>% filter(if_all(all_of(countries), ~ . == 0) )
      
      # The apply function, gets the top 3 countries with the most entries for each row (species)
      absent_top_countries_values <- apply(country_filtered, 1, function(x) sort(x, decreasing = TRUE)[1:3])
      absent_top_countries <- apply(country_filtered, 1, function(x) names(sort(x, decreasing = TRUE))[1:3])
      absent_top_countries[absent_top_countries_values == 0] <- 'NA'
      
      # Check if the table is empty or not
      if (is.null(nrow(absent_top_countries))){
        top_countries_df <- data.frame(matrix(ncol = 3, nrow = 0))
      } else {
        top_countries_df <- as.data.frame(t(absent_top_countries))
      }
    }
    else {
      top_countries_df <- data.frame(matrix(ncol = 3, nrow = 0))
    }
    
    # Name the columns and return
    colnames(top_countries_df) <- c("1st", "2nd", "3rd")
    top_countries_df
}  



# * SummaryReport ------------------------------------------------------------

## ----#
# Barcode Summary function
#   - Input: 
#       bold_coverage: The dataframe returned by the bold api with all the results
#                      (filtered if the user activated any filter)
#   - Output:
#         New dataframe that, generates the coverage matrix which contains 
#         the total number of entries found in BOLD with that species and barcode pair.
## ----#
barcode_summary <- function(bold_coverage) {
    summary_df <- bold_coverage %>%
      subset(subset = !is_missing(species_name) & !is_missing(markercode),
             select = c("species_name", "markercode")) %>%
      table %>%
      as.data.frame.matrix
    
    summary_df
}

## ----#
# Reduce Barcode Summary function
#   - Input: 
#       b_summary: The dataframe returned by the barcode summary function above
#   - Output:
#         At times BOLD has too many barcodes as user don't choose them, therefore
#         to help users parse them we sort them by "relevance",
#         which we define as having the most results/coverage
## ----#
reduce_barcode_summary <- function(b_summary) {
    # Number of non-zeros in each column
    summary <- b_summary
    count <- apply(summary, 2, function(c)sum(c!=0))
    sums <- apply(summary, 2, sum)
  
    # Calculate and sort by relavance
    calculated <- names(rev(sort(sums * count)))
  
    summary <- subset(summary, select = (calculated))
    summary
}

## ----#
# Missing Species function
#   - Input: 
#       missingList: List of missing species i.e those that were not found while searching
#   - Output:
#         Simply format the list into a dataframe that can then be displayed to the user
## ----#
missingSpecies <- function(missingList) {
  # Check for empty missing species list
  if (is.null(missingList[["Missing Species"]])) {
    missing_df <- data.frame(" " <- c("No Manual Processing Required!"))
  } else {
    missing_df <- data.frame(" " <- missingList[["Missing Species"]])
  }
  
  # Remove column name for species column
  colnames(missing_df) <- c(" ")
  
  missing_df
}

