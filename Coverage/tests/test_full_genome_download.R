library(modules)
library(ipc)
library(seqinr)

server_functions <- modules::use("./BlueWaltzBio/Coverage/server_functions.R")


fullGenomeDownloadTest <- function(fileType, uids, expectedOutput) 
{
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


fullGenomeDownloadTest("fasta", "", "./BlueWaltzBio/Coverage/tests/test_output/fullGenomeDownloadTestOutput/full_genome_test_download.fasta")

