The NCBI Nucleotide pipeline of RSB takes in a list of organism(s) and barcode-gene(s) of interest and then directly queries the Nucleotide database using the “Rentrez” package to find how many records match the search. To further tailor the search, users are able to:

1. Set minimum and maximum sequence lengths parameters.
2. Use the package “Taxize” to append synonyms and correct spelling mistakes of organism names.
3. Choose to search for results either within all fields or specifically in the organism and/or gene metadata fields of NCBI data.

The tool then showcases a Coverage Matrix (CM), described on the RSB home page, a statistical summary of the CM, and the search statements used to query NCBI Nucleotide. Users are able to download the results showcased and FASTA and GenBank files up to a preset limit. Greater detail for the options and displays are provided in the relevant portions of the pipeline.
