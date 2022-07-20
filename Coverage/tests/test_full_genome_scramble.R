library(modules)
library(future)
library(promises)
library(ipc)
library(rentrez)
library(taxize)

server_functions <- modules::use("Coverage/server_functions.R")

#UNHANDLED TEST CASES:
# Homonyms
# The max value of sequences to be retrieved per query is not 5


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
    for(i in seq.int(1, min(c(counts[[orgIter]], 5)))){     #5 needs to be replaced with # of sequences retrieved
      Sys.sleep(0.34)
      trueTax <- get_uid_(sci_com=orgNames[[orgIter]], messages=FALSE)   # THIS WON'T WORK FOR HOMONYMS CASE
      trueTaxId <- strtoi(trueTax[[orgNames[[orgIter]]]]$uid, 10)
      trueTaxRank <- trueTax[[orgNames[[orgIter]]]]$rank
      
      # check if the corresponding uid matches the organism name
      Sys.sleep(0.34)
      esum <- entrez_summary(db="nucleotide", id=uids[[idIter]])
      foundOrg <- tax_name(query = esum$organism,
                          get = c(trueTaxRank),
                          db = "ncbi",
                          messages = FALSE)[[trueTaxRank]]
      Sys.sleep(0.34)
      
      foundTax <- get_uid_(sci_com=foundOrg, messages=FALSE)   # THIS WON'T WORK FOR HOMONYMS CASE
      foundTaxId <- strtoi(foundTax[[foundOrg]]$uid, 10)


      if(!identical(foundTaxId, trueTaxId)){
        write(paste(esum$organism, ": ", foundTaxId,              # maybe change the "found species" printed here
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
function_template("Full chloroplast genomes in NCBI Nucleotide",
                  "Gallus gallus, Canis Lupus, Homo saapiens",
                  TRUE,
                  TRUE,
                  "animals_and_chloroplasts")

# Subspecies -------------------------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide",
                  "Canis lupus chanco",
                  TRUE,
                  TRUE,
                  "subspecies")

# Higher_taxa_ranks ------------------------------------------------------------
function_template("Full mitochondrial genomes in NCBI Nucleotide", 
                  "Haliotis",
                  TRUE,
                  TRUE,
                  "higher_taxa_ranks")

