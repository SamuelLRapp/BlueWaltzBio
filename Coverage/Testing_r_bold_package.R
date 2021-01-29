install.packages("bold")    # R package to pull sequences from BOLD
require("bold") 

#Taylor Wilcox's code: We interviewed him early in BWB and the notes are on the drive
#https://github.com/taylormwilcox/pull-genbank-bold/blob/master/genbank_bold_pull_recursive.R

#note: line 40 of this code he searches GenBank using  

genbank_term <- paste("mitochondrion[FILT] AND ", 
taxon_list, 
"[ORGN]",
sep = "", collapse = "")  :
-----------
  

#records_bold below was copy pasted from Taylor's code

records_bold <- bold_seqspec(taxon = "canis lupus")[, c('species_name',   #this vector is the dataframe's column"
                                                        'processid',             # BOLD identifier
                                                        'genbank_accession', 
                                                        'lat', 
                                                        'lon')]

#https://cran.r-project.org/web/packages/bold/bold.pdf
#more testing R_Bold_package
bold_seq <- bold_seq(taxon = "Pisaster giganteus")
bold_seq2 <- bold_specimens(taxon = "Pisaster giganteus")
records_bold_pi <- bold_seqspec(taxon = "Pisaster giganteus")[, c('species_name',   
                                                                  'processid',             # BOLD identifier
                                                                  'genbank_accession', 
                                                                  'lat', 
                                                                  'lon')]
