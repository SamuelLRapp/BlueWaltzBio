import pandas as pd
import numpy as np
import logging

LOG_FORMAT = "%(asctime)s | %(levelname)s | %(filename)s | %(message)s"
logging.basicConfig(filename = "run.log", level = logging.DEBUG, format = LOG_FORMAT)
logger = logging.getLogger()

# Set manual variable:
debugFlag = False # Default should be False but if you want more detail then make it true to see prints in the logger file
fileNameCrux = 'Raw_CRUX_results.csv' # CruxResults filename
TaxonomyNames = 'ALL_taxonomic_names.csv' # Taxonomy filename
BarcodeToCheck = input('Please input which Barcode you wish to verify: ')
CSVCruxName = BarcodeToCheck + '_CSV.csv' # Generate the CSV CRUX Name

CRUXresults = pd.read_csv(fileNameCrux) # Load the CRUX results csv file
taxonomyNames = pd.read_csv(TaxonomyNames) # Load the taxonomic csv file
barcodeDF = pd.read_csv(CSVCruxName) # Load the CRUX barcode csv file

logger.info("------ SET UP -------")
logger.info("debugFlag: " + str(debugFlag))
logger.info("CruxResults fileName: " + fileNameCrux)
logger.info("CruxResults fileName: " + TaxonomyNames)
logger.info("Barcode we are going to check: " + BarcodeToCheck)
logger.info("Name of CSV file with the CRUX database information: " + CSVCruxName)
logger.info("---------------------")


logger.info("------ Building Final Dataframe -------")
df = pd.DataFrame()
i = 0 # Iteration variable
while(i != len(CRUXresults.index)):
    genusSpecies = CRUXresults['genusspecies'][i] # start by selected the genusspecies from the cruxresults datraframe
    x = taxonomyNames[taxonomyNames['genusspecies'] == genusSpecies] # find a match in the taxonomy file
    x = x.drop_duplicates(subset=['genusspecies']) # Drop any duplicates that may have been found
    if len(x) == 0: # if the legnth is 0 that means nothing was found in this case it is very likely because it is not a genus species
        x = pd.DataFrame(data={'genusspecies': [np.nan], 'genus': [np.nan], 'familia': [np.nan],
                               'ordo': [np.nan], 'classis': [np.nan], 'phylum': [np.nan], 'regio': [np.nan]})
    df = df.append(x, ignore_index = True) # Add to the dataframe
    i += 1


# Get the column used to verify data
combinedCol = [df['genusspecies'], df['genus'], df['familia'], df['ordo'],
               df['classis'], df['phylum'], df['regio'], CRUXresults[BarcodeToCheck]]

# Create new dataframe with all the useful data to verify
finalData = pd.concat(combinedCol, axis=1,
                      keys=['genusspecies','genus', 'familia', 'ordo', 'classis', 'phylum', 'regio', BarcodeToCheck])

# Add true result and ERROR Columns
finalData['True Result'] = -1
finalData['ERROR'] = -1

if debugFlag == True:
    logging.info('dataframe head - {}'.format(finalData.to_string()))

logger.info("--------------------------------------")

# List of taxonomic ranks
listTaxRanks = ['genusspecies', 'genus', 'familia', 'ordo', 'classis', 'phylum', 'regio']

# List of possible results of taxonomic ranks for the CRUX search
CRUXresultsList = ['genus', 'family', 'order', 'class', 'phylum', 'kingdom']


logger.info("------ Beginning Data Verification -------")
errorFlag = 0
i = 0 # Iteration variable
while(i != len(finalData.index)):
    if finalData[BarcodeToCheck][i] == '0':     # If the value is a 0 in CRUX
        # 1. Check that the Genus Species is not in the barcode dataframe
        # 2. Check that Domain is not, or phylum is not, etc.
        # 3. If 1 and 2 are both TRUE then the result is correct if it isnt then FALSE and output why
        for j in range(len(listTaxRanks)): # Loop through the Taxonomic Ranks
            result = len(barcodeDF[barcodeDF[listTaxRanks[j]]==finalData[listTaxRanks[j]][i]].index) # number of genusspecies in barcodeDF
            if result != 0: # If the result is not 0 then that is an ERROR
                if j > 0: # Then it is not a genusspecies so write the actual word
                    finalData['True Result'][i] = listTaxRanks[j]
                else: # if it is 0 then write the number
                    finalData['True Result'][i] = result
                finalData['ERROR'][i] = 'ERROR' # Mark the Error regardless
                errorFlag = 1
            if errorFlag == 1:
                break
        if errorFlag == 0: # If the erroFlag is 0 then there is no ERROR
            finalData['True Result'][i] = result
            finalData['ERROR'][i] = np.nan
    elif finalData[BarcodeToCheck][i] in CRUXresultsList:    # If the value is a word in CRUX
        # 1. Check that the Genus Species is in the barcode Dataframe is 0 if not there is an Error
        # 2. If the number is 0 go up the taxonomic ranks to see which is the true result
        # If 1 results in an error then its finished output the number
        # if 1 does not result in an error then apply #2 and find the right taxonomic rank
        result = len(barcodeDF[barcodeDF['genusspecies']==finalData['genusspecies'][i]].index) # number of genusspecies in barcodeDF
        if result != 0: # If the result is not 0 then that is an ERROR
            finalData['True Result'][i] = result
            finalData['ERROR'][i] = "ERROR"
        else:
            for j in range(1, len(listTaxRanks)): # Loop through the taxonomic rank
                result = len(barcodeDF[barcodeDF[listTaxRanks[j]]==finalData[listTaxRanks[j]][i]].index) # number of genusspecies in barcodeDF
                if result != 0:
                    finalData['True Result'][i] = CRUXresultsList[j-1] # Set the word as the true result
                    #finalData.loc[:, ('True Result', i)] = CRUXresultsList[j-1]
                    if CRUXresultsList[j-1] == finalData[BarcodeToCheck][i]: # if True Result and the cruxresult are equal then no ERROR
                        finalData['ERROR'][i] = np.nan
                        #finalData.loc[:, ('ERROR', i)] = np.nan
                    else: # Else if they are different there is an ERROR
                        finalData['ERROR'][i] = 'ERROR'
                        #finalData.loc[:, ('ERROR', i)] = 'ERROR'
                    break
            if finalData['True Result'][i] == -1: # if TrueResult is -1 that means that the result should be 0
                finalData['True Result'][i] = 0
                finalData['ERROR'][i] = 'ERROR'
    else: # If it is a number other than 0
        # It is a number we have to check that it is equal than the number in the CRUX search
        # if it isn't then output the actual number if it is then not then Error
        # it could also happen that it is zero if that is the case we gotta go up the taxonomic ranks and find the lowest
        result = len(barcodeDF[barcodeDF['genusspecies']==finalData['genusspecies'][i]].index) # number of genusspecies in barcodeDF
        if str(result) == finalData[BarcodeToCheck][i]: # Check if the numbers match if it does we are done no ERROR
            finalData['True Result'][i] = result
            finalData['ERROR'][i] = np.nan
        elif result != finalData[BarcodeToCheck][i] and result > 0: # If it is a non zero number but they are not equal then ERROR
            finalData['True Result'][i] = result
            finalData['ERROR'][i] = "ERROR"
        else: # Else it is a zero so we have to go up the taxonomic ranks
            for j in range(1, len(listTaxRanks)):
                result = len(barcodeDF[barcodeDF[listTaxRanks[j]]==finalData[listTaxRanks[j]][i]].index) # number of genusspecies in barcodeDF
                if result != 0: # if when going up the result is not zero then print Error and the taxonomic rank
                    if finalData[BarcodeToCheck][i] == str(result): # Check if the numbers match
                        finalData['True Result'][i] = str(result) + " " + CRUXresultsList[j-1] # True result add the number plus the taxonomic rank
                        if finalData['genusspecies'][i] != finalData[listTaxRanks[j]][i]: # check if the genusspecies (which is really the input species) matches the
                            finalData['ERROR'][i] = "ERROR"
                        else:
                            finalData['ERROR'][i] = np.nan
                    else: # else there is an ERROR
                        finalData['True Result'][i] = CRUXresultsList[j-1] + " " + str(result)
                        finalData['ERROR'][i] = "ERROR"
                    break
        if finalData['True Result'][i] == -1: # if TrueResult is -1 that means that the result should be 0
            finalData['True Result'][i] = 0
            finalData['ERROR'][i] = "ERROR"
    errorFlag = 0
    i = i + 1

logger.info("--------------------------------------")

# Sort values by the Barcode
finalData = finalData.sort_values(by=BarcodeToCheck)

# Write to CSV
finalData.to_csv(BarcodeToCheck + 'resultsVerification.csv')


