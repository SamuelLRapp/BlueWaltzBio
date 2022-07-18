library(modules)
library(future)
library(promises)
library(ipc)

server_functions <- modules::use("Coverage/server_functions.R")



function_template <- function(dbOption, orgList, taxizeOption, refSeqChecked, testName){
  print("------------------------------------")
  print(paste("Beginning test ", testName))
  genSearchOutput <- server_functions$getGenomeSearchFullResults(dbOption, orgList, taxizeOption, refSeqChecked)
  fileName <- paste("Coverage/tests/test_output/fullGenomeSearchTestOutput/", 
                    testName, ".csv", sep="")
  counts <- genSearchOutput[[1]][[1]][[1]]
  orgNames <- genSearchOutput[[2]]
  uids <- genSearchOutput[[1]][[2]]
  idIter <- 1
  correct <- TRUE
  for(orgIter in seq.int(1, length(counts))){
    for(i in seq.int(1, counts[[orgIter]])){
      # check if the corresponding uid matches the organism name
      esum <- entrez_summary(db="nucleotide", id=uids[[idIter]])
      # print(names(esum))
      if(!identical(esum$organism, orgNames[[orgIter]])){
        write(paste(esum$organism, ", ", orgNames[[orgIter]]),
                    file=fileName, append=TRUE)
        correct <- FALSE
      }
      idIter <- idIter + 1
    }
  }
  print(paste("results of test ", testName, ":"))
  print(correct)
}

# simple_test_taxize_off -------------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide", 
                  "Gallus gallus, Canis Lupus",
                  FALSE,
                  TRUE,
                  "simple_test_taxize_off")




