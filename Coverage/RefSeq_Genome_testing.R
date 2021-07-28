
# title -------------------------------------------------------------------
## this is a script to test using the entrez package to search inside the refseq and genome databases

##created by Sam on march 9th, 2021

# libraries ---------------------------------------------------------------

library(rentrez)


# code --------------------------------------------------------------------

#entrez functions
entrez_dbs() ##genome database is available, 
entrez_db_searchable("genome") #search terms that can be used
entrez_db_searchable("nucleotide") #search terms that can be used

#standard search term  
entrez_search(db = "nucleotide", term = searchTerm, retmax = 5)
searchTerm <- "apis"

#search terms for refseq
entrez_search(db = "nucleotide", term = RefseqsearchTerm, retmax = 5)
RefseqsearchTerm <- "apis AND srcdb_refseq[PROP]"

#search terms for refseq with additional limitations
entrez_search(db = "nucleotide", term = RefseqsearchTerm, retmax = 5)
RefseqsearchTerm <- "apis AND srcdb_refseq[PROP]"
#we know that mitochondrial genomes are about 16,000bp, 159,272 bp in chloroplast, should be 10s to 100s of millions of bp per chromosome of nuclear DNA
#we can add s '15000:18000[SLEN]' for cholorplast etc...


#Gallus gallus AND srcdb_refseq[PROP] AND 15000:18000[Sequence Length] AND mitochondrial


#All refseq information lives in nucleotide database. It can be accessed in nucleotide by adding 'AND srcdb_refseq[property]'
#""""Accession format: The most distinguishing feature of a RefSeq record is the distinct accession number format that begins with two characters followed by an underscore (e.g., NP_). INSDC accession numbers never include an underscore. Please see the description of RefSeq accession prefixes.""""
#https://www.ncbi.nlm.nih.gov/books/NBK21091/table/ch18.T.refseq_accession_numbers_and_mole/?report=objectonly,  
#The accension numbers for refseq nucleotide entries have two characters then a underscore at the front and a comment that says they are a refseq

#search for organisms will full genomes available online
genome_result<- entrez_search(db = "genome", term = genome_SearchTerm, retmax = 5)
genome_SearchTerm <- "Oreochromis niloticus"
#https://www.ncbi.nlm.nih.gov/genome/browse#!/overview/
#there are only 1000+ species that qualify being in the genome database. The genome databases, links back to Nucleotide at the end of the day tho.

#this query allows us to answer, yes or no is the full genome available online. 
#The number of nucleotide entrise associtaed with a full genome can be 1 or 40. It just depends. 
#genomee entries seeem to be made up of refseq genomes, which are ncbi trusted and annotated nucleotide entries.
#If we want addtional information search for refseqs bc genome is made of refseq nucleotide database entries


# notes on 'genome' and 'refseq' ------------------------------------------

#All the various databases in NCBI are very interconnected. They service unique and overlapping purposes but they all link between each other.
#Both genome and refseq just link/lead back to nucleotide db at the end of the day.
#refseq FAQ: https://www.ncbi.nlm.nih.gov/books/NBK50679/#RefSeqFAQ.how_frequently_are_refseq_reco
#If you search on the webbrowser using the 'refseq' database it will search the 'nucleotide database' with the original search term plus 'AND srcdb_refseq[property]'
#IE refseqs are all inside nucleotide database
#