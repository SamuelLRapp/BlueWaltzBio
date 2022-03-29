library(modules)
library(future)
library(promises)
library(ipc)

orgListHelper <- modules::use("Coverage/orgListHelper.R")

# The list of fundamental cases (not combined):
# 1) We toggle the second argument on or off
# 2) We can have duplicate inputs
# 3) We can have "odd" characters in the string
# 4) double commas
# 5) leading or trailing whitespace
# 6) double spaces inbetween words in the same org name
# 7) 


function_template <- function(expectedOutput, inputString, inputBool, funcName){
  orgListHelper$taxizeHelper(inputString, inputBool) %...>%{
    taxizeHelperOutput <- .
    fileName <- paste("Coverage/tests/test_output/", funcName, ".csv", sep="")
    write.table(taxizeHelperOutput, 
                file=fileName,
                row.names = FALSE,
                col.name = FALSE)
    orgVector <- read.table(fileName)[1]$V1
    print(taxizeHelperOutput)
    print(orgVector)
    print(expectedOutput)
    print(identical(orgVector, expectedOutput))
  }
}

# boolean_off ------------------------------------------------------------------
function_template(c("Homo saapiens", "Homo sapiens"),
                  "Homo saapiens, Homo sapiens",
                  FALSE,
                  "boolean_off")

