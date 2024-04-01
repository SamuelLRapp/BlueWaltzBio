#idea to use SQLite from https://shiny.posit.co/r/articles/build/persistent-data-storage/
#DON'T USE in website version 
import(RSQLite)

#backup for uids-matrix
backupUids <- function(backup, fasta_db, uids) {
  dbWriteTable(backup, paste0(fasta_db,"uids"), uids, overwrite = TRUE)
}

#backup fetched content 
backupDownload <- function(backup, fasta_db, fasta, filename) {
  #backup fasta
  dbWriteTable(backup, paste0(fasta_db,"fasta"), data.frame(filename = c(filename), fasta = c(fasta)), append = TRUE)
}

deleteDownloadBackup <- function(backup, fasta_db) {
  dbRemoveTable(backup, paste0(fasta_db,"uids"))
  dbRemoveTable(backup, paste0(fasta_db,"fasta"))
}