library(modules)
library(future)
library(promises)
library(ipc)

server_functions <- modules::use("Coverage/server_functions.R")

# There are X possible cases


function_template <- function(expectedOutput, inputString, inputBool, testName){
  print("------------------------------------")
  print("Beginning test ", testName)
  genSearchOutput <- server_functions$getGenomeSearchFullResults()
  fileName <- paste("Coverage/tests/test_output/fullGenomeSearchTestOutput/", 
                    testName, ".csv", sep="")
  write.table(genSearchOutput, 
              file=fileName,
              row.names = FALSE,
              col.name = FALSE)
  orgVector <- c()
  if(file.info(fileName)$size != 0){
    orgVector <- read.table(fileName)[1]$V1
  }
  print(genSearchOutput)
  print(orgVector)
  print(expectedOutput)
  print(paste("results of test ", testName, ":"))
  print(identical(orgVector, expectedOutput))
}

# boolean_off ------------------------------------------------------------------
function_template(c("Homo saapiens", "Homo sapiens"),
                  "Homo saapiens, Homo sapiens",
                  FALSE,
                  "boolean_off")




