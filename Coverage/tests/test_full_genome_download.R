library(modules)
library(ipc)
library(seqinr)

#source("./BlueWaltzBio/Coverage/server_functions.R")

server_functions <- modules::use("./BlueWaltzBio/Coverage/server_functions.R")


fullGenomeDownloadTest <- function(fileType, uids, expectedOutput) 
{
  # download files from ncbi and save them in a expected file
  # load expected files into a single dataframe (using read.fasta, hopefully something
  # similar exists for gb files)
  # download files using server_functions$... into an output file
  # load output file into a df
  # check if files are identical
  
  # what uids ?
  
  # uids found for species, simply search the species on the website.
  # the problem is, which fasta to download, since there are multiple links
  # for each species.
  expected <- read.fasta("./BlueWaltzBio/Coverage/tests/test_output/fullGenomeDownloadTestOutput/full_genome_test_download.fasta")
  print(expected)
  downloadFilePath <- paste("./BlueWaltzBio/Coverage/tests/test_output/fullGenomeDownloadTestOutput/full_genome_test_download.", fileType, sep="")
  server_functions$fullGenomeDownload(fileType, uids, downloadFilePath, NULL)
  
  # read.fasta converts to data frame
  coverageGenerated <- read.fasta(downloadFilePath)
  expected <- read.fasta(expectedOutput)
  # print(coverageGenerated)
  # print(expected)
  print("Test results:")
  print(identical(coverageGenerated, expected))
}


fullGenomeDownloadTest("fasta", 9031, "./BlueWaltzBio/Coverage/tests/test_output/fullGenomeDownloadTestOutput/full_genome_test_download.fasta")

