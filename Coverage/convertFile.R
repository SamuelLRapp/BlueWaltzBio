# input: .xlsx or .csv files
# output: comma seperated list
# running this file will open up a file directory that allows
# user to choose a file
library(readxl)
library(readr)
library(tools)


file <- file.choose() #choose a file
fileExt <- file_ext(file) #gets the file extension 

#checks the extension of the file
if (fileExt == "xlsx") { # checks for xlsx - excel file extension
  speciesList <- read.xlsx(file) 

  
} else if ( fileExt == "csv") { #checks for comma-separated values extension
  speciesList <- read.csv(file = file, header = T, sep = ",", 
                          na.strings = "`", stringsAsFactors = F)
  
} else { #if neither is present, quits
  quit()
}

speciesData <- (c(speciesList$Species.Name)) 
#change Species.Name as needed - might have to remove the "."


cat(speciesData, sep = ", ") #separates by commas
