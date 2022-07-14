# Imports:
library(RSQLite)
library(dplyr)
library(dbplyr)
library(DBI)
library(tidyverse)

#Creating databases from taxonomy files/datafames:
file_to_DF <- function(filepath) {
  taxonomytable<-read.delim(filepath, header=FALSE)
  taxonomy_only_table <-select(taxonomytable, V2) %>%
  separate(V2, c("regio", "phylum", "classis", "ordo",  "familia", "genus", "genusspecies"), sep = ";",remove=FALSE)
}

#running function to create dataframes
df_18S <- file_to_DF("Coverage/18S_taxonomy.txt")
df_16S <- file_to_DF("Coverage/16S_taxonomy.txt")
df_PITS <- file_to_DF("Coverage/PITS_taxonomy.txt")
df_CO1 <- file_to_DF("Coverage/CO1_taxonomy.txt")
df_FITS <- file_to_DF("Coverage/FITS_taxonomy.txt")
df_trnL <- file_to_DF("Coverage/trnL_taxonomy.txt")
df_Vert12S <- file_to_DF("Coverage/Vert12S_taxonomy.txt")

#create SQLite DB
taxaDB <- dbConnect(RSQLite::SQLite(), "Coverage/taxa-db.sqlite")

#populate SQLite Db with dataframes
dbWriteTable(taxaDB, "MB18S", df_18S)
dbWriteTable(taxaDB, "MB16S", df_16S)
dbWriteTable(taxaDB, "MBPITS", df_PITS)
dbWriteTable(taxaDB, "MBCO1", df_CO1)
dbWriteTable(taxaDB, "MBFITS", df_FITS)
dbWriteTable(taxaDB, "MBtrnL", df_trnL)
dbWriteTable(taxaDB, "MB12S", df_Vert12S)

dbDisconnect(taxaDB)
unlink("taxa-db.sqlite")
