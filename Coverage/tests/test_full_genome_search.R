library(modules)
library(future)
library(promises)
library(ipc)

server_functions <- modules::use("Coverage/server_functions.R")

# There are X possible cases


function_template <- function(dbOption, orgList, taxizeOption, refSeqChecked, testName){
  print("------------------------------------")
  print("Beginning test ", testName)
  genSearchOutput <- server_functions$getGenomeSearchFullResults(dbOption, orgList, taxizeOption, refSeqChecked)
  fileName <- paste("Coverage/tests/test_output/fullGenomeSearchTestOutput/", 
                    testName, ".csv", sep="")
  counts <- genSearchOutput[[1]][[1]]
  print(counts)
  orgNames <- genSearchOutput[[2]]
  uids <- genSearchOutput[[1]][[2]]
  # for(i in range(nrow()))
  write.table(genSearchOutput, 
              file=fileName,
              row.names = FALSE,
              col.name = FALSE)
  # orgVector <- c()
  # if(file.info(fileName)$size != 0){
  #   orgVector <- read.table(fileName)[1]$V1
  # }
  print(paste("results of test ", testName, ":"))
  print(identical(orgVector, expectedOutput))
}

# simple_test_taxize_off -------------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide", 
                  "Gallus gallus, Canis Lupus",
                  FALSE,
                  TRUE,
                  "simple_test_taxize_off")




