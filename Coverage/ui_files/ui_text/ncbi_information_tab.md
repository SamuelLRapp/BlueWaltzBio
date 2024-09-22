#### What does the tool do?

The 'NCBI Nucleotide Coverage Matrix' was designed to screen the Nucleotide database for genetic barcode coverage prior to environmental DNA metabarcoding studies. Before conducting a metabarcoding study, scientists need to be aware of which organisms have reference sequences at known genetic barcoding loci. The tool finds out if the Nucleotide database contains sequences labeled with a specific gene and organism name. Numerous searches can be done in parallel instead of manually searching for each organism-gene combination on the NCBI Nucleotide website.

The 'NCBI Nucleotide Coverage Matrix' tool takes in a list of organisms and genes of interest and then queries the Nucleotide database to find how many records match the search. The tool then produces a table where the organism names are rows, gene names are columns, and each intersection of a row and column shows how many records are in the NCBI Nucleotide database. All of the search options are detailed in the 'Search fields' section below. The power and flexibility of this tool allows scientists to check the NCBI Nucleotide database for genetic coverage in ways that aren’t possible without knowledge of the NCBI "Entrez" coding package.

#### Limitations:

This tool may not find all possible entries that the user desires. Some limitations of this text-based search include, but are not limited to:

- Alternative names of the listed gene in NCBI Nucleotide database
- Incorrect or missing metadata
- Full genomes entries with unlabeled individual genes

Searching by Blast or in Silico PCR are more empirical ways of identifying quality reference sequences, but are not implemented here. In addition, this tool only searches against the NCBI Nucleotide database, and thus doesn’t include other genetic databases such as BOLD, Silva, Fishbase, etc.
