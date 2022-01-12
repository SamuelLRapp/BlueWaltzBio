file_to_DF <- function(filepath) {
  taxonomytable<-read.delim(filepath, header=FALSE)
  taxonomy_only_table <-select(taxonomytable, V2) %>%
  separate(V2, c("regio", "phylum", "classis", "ordo",  "familia", "genus", "genusspecies"), sep = ";",remove=FALSE)
}

# To use this script just change the name of the file you want to create the CSV from.
# If you want 18S then it should look like:
# write.csv(file_to_DF("Coverage/18S_taxonomy.txt"), "18S_CSV.csv")
write.csv(file_to_DF("../Coverage/FITS_taxonomy.txt"), "FITS_CSV.csv")