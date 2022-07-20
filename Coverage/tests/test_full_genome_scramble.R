library(modules)
library(future)
library(promises)
library(ipc)
library(rentrez)
library(taxize)

server_functions <- modules::use("Coverage/server_functions.R")

#UNHANDLED TEST CASES:
# Homonyms
# Higher taxa rank inputs than species
# User asks for a subspecies rather than a main species
# There were 0 found sequences

function_template <- function(dbOption, orgList, taxizeOption, refSeqChecked, testName){
  print("------------------------------------")
  print(paste("Beginning test ", testName))
  genSearchOutput <- server_functions$getGenomeSearchFullResults(dbOption, orgList, taxizeOption, refSeqChecked)
  fileName <- paste("Coverage/tests/test_output/fullGenomeScrambleTestOutput/", 
                    testName, ".csv", sep="")
  close( file( fileName, open="w" ) )
  counts <- genSearchOutput[[1]][[1]][[1]]
  orgNames <- genSearchOutput[[2]]
  uids <- genSearchOutput[[1]][[2]]
  idIter <- 1
  correct <- TRUE
  for(orgIter in seq.int(1, length(counts))){
    if(counts[[orgIter]] == 0){
      next
    }
    for(i in seq.int(1, counts[[orgIter]])){
      Sys.sleep(0.34)
      # check if the corresponding uid matches the organism name
      esum <- entrez_summary(db="nucleotide", id=uids[[idIter]])
      # print(names(esum))
      species <- tax_name(query = esum$organism,
                          get = c("species"),
                          db = "ncbi",
                          messages = FALSE)$species
      Sys.sleep(0.34)
      
      foundTaxId <- get_uid_(sci_com=species, messages=FALSE)   # THIS WON'T WORK FOR HOMONYMS CASE
      foundTaxId <- strtoi(foundTaxId[[species]]$uid, 10)
      
      Sys.sleep(0.34)
      trueTaxId <- get_uid_(sci_com=orgNames[[orgIter]], messages=FALSE)   # THIS WON'T WORK FOR HOMONYMS CASE
      trueTaxId <- strtoi(trueTaxId[[orgNames[[orgIter]]]]$uid, 10)
      # print(typeof(trueTaxId))
      if(!identical(foundTaxId, trueTaxId)){
        write(paste(esum$organism, ": ", foundTaxId,              # maybe we can change the "found species" printed here
                    ", ", orgNames[[orgIter]], ": ", trueTaxId),
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

# simple_test_taxize_on --------------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide", 
                  "Homo saapiens",
                  TRUE,
                  TRUE,
                  "simple_test_taxize_on")

# Animals_and_chloroplasts -----------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide", 
                  "Gallus gallus, Canis Lupus, Homo saapiens",
                  TRUE,
                  TRUE,
                  "animals_and_chloroplasts")


