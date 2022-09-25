library(modules)
library(ipc)
library(seqinr)
library(rentrez)
library(read.gb)
library(stringr)

server_functions <- modules::use("./BlueWaltzBio/Coverage/server_functions.R")

# fileConn <- file("./BlueWaltzBio/Coverage/tests/full_genome_download/discrepancies.txt")
# lines <- readLines(fileConn)
# close(fileConn)
# for (line in lines) {
#   spltstr <- strsplit(line, ".", fixed=TRUE)
#   fileType <- spltstr[[1]][length(spltstr[[1]])]
#   uid <- strsplit(line, paste(".", fileType, sep=""), fixed=TRUE)
#   print(paste("filetype:", fileType, sep=" "))
# }



# applies readFunc to file and returns the value.
# this function returns the empty string if either an error
# or a warning was thrown while applying readFunc and prints
# the error to console. This takes the error handling away from
# R so the program doesn't quit before checking every uid.
readFile <- function(file, readFunc) {
  f <- try(readFunc(file), silent=TRUE)
  if (inherits(f, "try-error")) {
    print(paste("Error in applying read function to file: ", f, sep=""))
    return("")
  } else {
    return(f)
  }

}


# takes a fileType as string - expected one of ["gb", "fasta"],
# a string of space-separated uids, the web downloaded version of
# the file read in to a dataframe, and the read function to apply
# to the file to be downloaded. saves the downloaded file in 
# ./BlueWaltzBio/Coverage/tests/full_genome_download/fetched/
# returns True if the downloaded file matches the expected output, false otherwise.
fullGenomeDownloadTest <- function(fileType, uids, expectedOutput, readFunc){
    serverDownloadPath <- paste("./BlueWaltzBio/Coverage/tests/full_genome_download/fetched/", uids, "." , fileType, sep="")
    fileVector <- c()
    for (uid in uids) {
      Sys.sleep(0.4)
      fileVector <- c(fileVector, entrez_fetch(db='nucleotide', uid, rettype=fileType))
    }
    write(fileVector, serverDownloadPath)
    serverDownloaded <- readFile(serverDownloadPath, readFunc)
    identical(serverDownloaded, expectedOutput)
}


# abstract the loop to run both fasta and genbank.
# also writes the uids of any discrepancies found to the discrepancy file.
accessionLoop <- function(accessionNumbers, fileType) {
  for (num in accessionNumbers){
    func <- switch(fileType, "fasta" = read.fasta, "gb" = read.gb)
    file <- paste("./BlueWaltzBio/Coverage/tests/full_genome_download/web_downloaded/", num, ".", fileType, sep="")
    manuallyDownloaded <- readFile(file, func)
    if (manuallyDownloaded != "") {
      if (!fullGenomeDownloadTest(fileType, num, manuallyDownloaded, func)) {
        print(paste("Discrepancy in", num, sep=" "))
        write(paste(num, ".", fileType, sep=""), file="./BlueWaltzBio/Coverage/tests/full_genome_download/discrepancies.txt", append=TRUE)
      }
    }
  }
}


# text file with copy-pasted accession numbers for a few different species.
# includes multiple ids in one line.
# every accession number in this file must have an accompanying
# file already downloaded and save in
# "./BlueWaltzBio/Coverage/tests/full_genome_download/web_downloaded/
accessionNumbers = readLines("./BlueWaltzBio/Coverage/tests/full_genome_download/accession_numbers.txt")

# clear the discrepancy file
close(file("./BlueWaltzBio/Coverage/tests/full_genome_download/discrepancies.txt", open="w"))

accessionLoop(accessionNumbers, 'gb')
#accessionLoop(accessionNumbers, 'fasta')


