<p align="center">
  <img src="https://github.com/SamuelLRapp/BlueWaltzBio/blob/master/BlueWaltzBioIcon.jpg" />
</p>

# BlueWaltzBio Reference Sequence Browser

Our team iteratively designed a Graphic User Interface (GUI) Shiny application called the Reference Sequence Browser (RSB) that provides users efficient and intuitive access to multiple genetic databases regardless of computer programming expertise. The application returns the number of publicly accessible barcode markers per organism in the NCBI Nucleotide, BOLD, or CALeDNA CRUX Metabarcoding Reference Databases. Depending on the database, we offer various search filters such as min and max sequence length or country of origin. Users can then download the FASTA/GenBank files from the RSB web tool, view statistics about the data, and explore results to determine details about the availability or absence of reference sequences.

To learn more about our the purpose of this tool please refer to our paper published in PLOS ONE:

Paper link:

To learn more about the tool itself and how to use it please refer to the Protocol/User guide found here:

Protocol/User Guide: dx.doi.org/10.17504/protocols.io.q26g71zxqgwz/v1

This tool was developed by a dedicated group of undergraduates at UC Santa Cruz to support eDNA and metabarcoding scientists. To create this tool, we interviewed numerous eDNA researchers and integrated their feedback throughout the development process. After a few years of development work, we are excited to share that our paper has now been published, and the tool is ready for use. We hope it proves valuable to your workflow. If you find it useful, we would greatly appreciate your citation of our paper (linked above) and any feedback you may have to help us further enhance the tool.

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
