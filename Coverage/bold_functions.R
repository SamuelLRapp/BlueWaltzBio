import(tidyverse)


# * BOLDCountryFilter -------------------------------------------


country_summary <- function(bold_coverage){
    summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
    records_bold <- bold_coverage
    if (length(records_bold$species_name) == 0){
        return(summary_df)
    }
    for(i in 1:length(records_bold$species_name)){
        if (!is.na(records_bold$species_name[i]) && (records_bold$species_name[i] != "") && !(records_bold$species_name[i] %in% rownames(summary_df)))
            #add a row to summary_df
            summary_df[records_bold$species_name[i],] <- integer(ncol(summary_df))
        #add data to summary_df to get summary data
        if (!is.na(records_bold$country[i]) && records_bold$country[i] != '' && (records_bold$species_name[i] != "") && 
            !is.na(records_bold$markercode[i]) && records_bold$markercode[i] != '' && (records_bold$species_name[i] != "")){
            #if country is not yet in the dataframe, initiate new col
            if (!(records_bold$country[i] %in% colnames(summary_df))){
                #create a new column of 0s
                summary_df[records_bold$country[i]] <- integer(nrow(summary_df))
            }
            #add 1 to existing count
            summary_df[records_bold$species_name[i], records_bold$country[i]] <- summary_df[records_bold$species_name[i], records_bold$country[i]] + 1
        }
    }
    # print("COUNTRY SUMMARY")
    # print(summary_df)
    summary_df
    
}

# * NA Filter -------------------------------------------


naBarcodes <- function(bold_coverage){
  summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
  records_bold <- bold_coverage
  if (length(records_bold$species_name) == 0){
    return(summary_df)
  }
  
  for(i in 1:length(records_bold$species_name)){
    if (is.na(records_bold$species_name[i]) && (records_bold$species_name[i] == "") && !(records_bold$species_name[i] %in% rownames(summary_df)))
      #add a row to summary_df
      summary_df[records_bold$species_name[i],] <- integer(ncol(summary_df))
    #add data to summary_df to get summary data
    if (is.na(records_bold$markercode[i]) || records_bold$markercode[i] == '' || (records_bold$species_name[i] == "")){
      #if country is not yet in the dataframe, initiate new col
      #add 1 to existing count
      if (ncol(summary_df) == 0) {
        summary_df["Entries where Barcode was NA"] <- integer(ncol(summary_df))
        summary_df[records_bold$species_name[i], "Entries where Barcode was NA"] <- records_bold$processid[i]
      } else {
        newStr = paste(summary_df[records_bold$species_name[i], "Entries where Barcode was NA"], records_bold$processid[i], sep=", ")
        summary_df[records_bold$species_name[i], "Entries where Barcode was NA"] <- newStr
      }
    }
  }
  summary_df
}

presentMatrix <- function(bold_coverage, countries){
    present_df <- country_summary(bold_coverage)
    
    #remove all columns that are not in filter
    present_df <- present_df[ , which(names(present_df) %in% countries), drop=FALSE]
    
    #remove all rows that have all 0s as values and return
    #present_df <- present_df

    #Need to include drop=false to prevent R from dropping dataframe structure when numcolumns is 1, otherwise rowsums will complain
    #https://stackoverflow.com/questions/32330818/r-row-sums-for-1-or-more-columns
    present_df <- present_df[rowSums(present_df[drop=FALSE])>0, drop=FALSE]
    present_df
    
}

absentMatrix <- function(bold_coverage, countries, country_names){
    all_names <- rownames(country_summary(bold_coverage))
    #get rownames of presentMatrix  
    present_names <- rownames(presentMatrix(bold_coverage, countries))
    #get complement of names
    absent_names <- all_names[is.na(pmatch(all_names, present_names))]
    
    country_names <- country_names[!duplicated(country_names)]
    summary_df <- country_summary(bold_coverage)
    absent_df <- data.frame(matrix(ncol = 3, nrow = 0))
    colnames(absent_df) <- c("1st", "2nd", "3rd")
    if(length(absent_names) == 0){
        return(absent_df)
    }
    for(i in 1:length(absent_names)){
        sig_countries <- c("NA" = 0,"NA" = 0,"NA" = 0)
        for (j in 1:length(country_names)){
            
            val <- summary_df[absent_names[i], country_names[j]]
            #if value is bigger than the most significant
            if(val > sig_countries[[1]]){
                #move col 2 to col 3
                names(sig_countries)[3] <- names(sig_countries)[2]
                sig_countries[[3]] <- sig_countries[[2]]
                #move col 1 to col 2
                names(sig_countries)[2] <- names(sig_countries)[1]
                sig_countries[[2]] <- sig_countries[[1]]
                #replace col 1 with new val
                names(sig_countries)[1] <- country_names[j]
                sig_countries[[1]] <- val
                
            } else if (val > sig_countries[[2]]){
                #move col 2 to col 3
                names(sig_countries)[3] <- names(sig_countries)[2]
                sig_countries[[3]] <- sig_countries[[2]]
                #replace col 2 with new val
                names(sig_countries)[2] <- country_names[j]
                sig_countries[[2]] <- val
            } else if (val > sig_countries[[3]]){
                #replace col3 with new val
                names(sig_countries)[3] <- country_names[j]
                sig_countries[[3]] <- val
            }
        }
        absent_df[absent_names[i],] <- names(sig_countries)
    }
    absent_df
    
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
    # print("Sorted Summary")
    # print(summary)
    summary
}

missingSpecies <- function(missingList) {
  summary_df <- data.frame(matrix(ncol = 0, nrow = 0))
  newStr <- ""
  for (i in 1:length(missingList)) {
    if (newStr == "") {
      newStr <- paste(newStr, missingList[i], sep="")
    } else {
      newStr <- paste(newStr, missingList[i], sep=", ")
    }
  }
  summary_df[" ", " "] <- newStr
  summary_df
}

