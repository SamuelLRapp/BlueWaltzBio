#### Introduction and the role of reference databases

The 'CRUX Coverage Matrix' tool identifies how well sets of organisms are represented in the public CALeDNA reference databases. This tool may be used by either scientists interested in using the public CALeDNA reference database or parties interested in working with CALeDNA scientists for conducting a study. The site displays the taxonomic resolution and reference sequence abundance available within the CALeDNA databases for different organisms, which informs the user how well the databases may fulfill the needs of their study.

The databases were generated using the CRUX Pipeline, part of the Anacapa Toolkit (Curd et al., 2019 in MEE). The full databases and further documentation can be found here: [https://ucedna.com/reference-databases-for-metabarcoding](https://ucedna.com/reference-databases-for-metabarcoding).

Reference databases allow for the taxonomic assignment of metagenomic sequences. DNA sequences are taxonomically identified by matching them to previously identified reference sequences. Large swaths of organisms and taxonomic groups have yet to be DNA barcoded, and thus cannot be detected using metagenomic sequencing. However, organizations like BOLD and the Smithsonian are currently working to fill these holes in our reference libraries.

You can find the public CRUX databases at this link: [https://ucedna.com/reference-databases-for-metabarcoding](https://ucedna.com/reference-databases-for-metabarcoding)

#### What does the CRUX Coverage Matrix do?

The 'CRUX Coverage Matrix' returns a value that represents how many reference sequences exist for the user’s organism search term(s) in each public database. When direct matches are not found in a database, the tool will instead search for lower taxonomic ranks until a match is found. When a metabarcoding study is being performed, it is critical to confirm the existence of and obtain the reference sequences of organisms of interest, as well as the taxonomic resolution of said sequences, and what metabarcoding loci the reference sequences belong to. (See additional information for more details) CRUX databases are designed to be shared, and this tool allows users to assess whether the public CRUX databases meet their study’s taxonomic requirements.

The 'CRUX Coverage Matrix' searches by taxonomic ranks: domain, phylum, class, order, family, genus, genus-spp. The rows of the table produced are the organism search terms, and the columns are CRUX databases: 16S, 12S, 18S, PITS, CO1, FITS, trnL, Vertebrate.

The cells will show one of the following:
1. The number of sequences in a database, if direct matches are found
2. If no direct matches are found, the next most specific taxonomic rank found
3. “0” if nothing is found at any taxonomic rank.
