<p align="center">
  <img src="https://github.com/SamuelLRapp/BlueWaltzBio/blob/master/BlueWaltzBioIcon.jpg" />
</p>

# BlueWaltzBio Reference Sequence Browser

Land managers, researchers, and regulators increasingly utilize environmental DNA (eDNA) techniques to monitor species richness, presence, and absence. In order to properly develop a biological assay for eDNA metabarcoding or quantitative PCR, scientists must be able to find not only reference sequences (previously identified sequences in a genomics database) that match their target taxa but also reference sequences that match non-target taxa. Determining which taxa have publicly available sequences in a time-efficient and accurate manner currently requires computational skills to search, manipulate, and parse multiple unconnected DNA sequence databases. 

Our team iteratively designed a Graphic User Interface (GUI) Shiny application called the Reference Sequence Browser (RSB) that provides users efficient and intuitive access to multiple genetic databases regardless of computer programming expertise. The application returns the number of publicly accessible barcode markers per organism in the NCBI Nucleotide, BOLD, or CALeDNA CRUX Metabarcoding Reference Databases. Depending on the database, we offer various search filters such as min and max sequence length or country of origin. Users can then download the FASTA/GenBank files from the RSB web tool, view statistics about the data, and explore results to determine details about the availability or absence of reference sequences.

To learn more about our the purpose of this tool please refer to our paper published in PLOS ONE:

Paper link:

To learn more about the tool itself and how to use it please refer to the Protocol/User guide found here:

Protocol/User Guide: dx.doi.org/10.17504/protocols.io.q26g71zxqgwz/v1

This tool was entirely made by a group of undergrads (now graduate students) in UC Santa Cruz. We interviewed many eDNA scientist to learn how to help them best and after a few years of development and publishing our paper it is ready to be used. We hope you find this useful and helps you with your workflow, if you do use we would appreciate you citing our published paper above.

# Installation Guide

While the tool is hosted in the UCLA servers (http://shiny.eeb.ucla.edu/BlueWaltzBio/Coverage/) you can also install set it up on your local machine and modify the code as you want.

In fact, we look forward to see what the community can add or think of adding to this tool!

To install:
1. You need to have R and R Studio installed. Everything was tested with R version 4.1.2.
2. Clone our repo to your local machine.
3. Run the rsbpackages.R script which will automatically install all the required packages with the right versions.
4. Download the CRUX Database files from here: https://drive.google.com/file/d/106-Gxe8cWZiFEb_0WoIcAjRFtDfXG08b/view?usp=sharing
5. You are all set now run the app using the RStudio "Run App" button or by executing the command shiny::runApp('Coverage')

<p align="center">
  <img src="https://github.com/SamuelLRapp/BlueWaltzBio/blob/master/BlueWaltzBioButterfly.jpg" />
</p>
