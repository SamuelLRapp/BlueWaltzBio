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


function_template <- function(expectedOutput, inputString, inputBool, testName){
  print("------------------------------------")
  print(paste("Beginning test ", testName))
  taxizeHelperOutput <- orgListHelper$taxizeHelper(inputString, inputBool)
  fileName <- paste("Coverage/tests/test_output/taxizeTestOutput/", testName, 
                    ".csv", sep="")
  write.table(taxizeHelperOutput, 
              file=fileName,
              row.names = FALSE,
              col.name = FALSE)
  orgVector <- c()
  if(file.info(fileName)$size != 0){
    orgVector <- read.table(fileName)[1]$V1
  }
  print(taxizeHelperOutput)
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

# empty_string -----------------------------------------------------------------
function_template(c(),
                  "",
                  TRUE,
                  "empty_string")

# simple_test ------------------------------------------------------------------
function_template(c("Homo sapiens", 
                    "Homo sapiens subsp. Denisova"),
                  "Homo saapiens",
                  TRUE,
                  "simple_test")

# duplicates_bool_on -----------------------------------------------------------
function_template(c("Gallus gallus"),
                  "Gallus gallus, Gallus gallus",
                  TRUE,
                  "duplicates_bool_on")

# duplicates_bool_off ----------------------------------------------------------
function_template(c("Homo saapiens"),
                  "Homo saapiens, Homo saapiens",
                  FALSE,
                  "duplicates_bool_off")

# double_comma_bool_on ---------------------------------------------------------
function_template(c("Homo sapiens", 
                    "Homo sapiens subsp. Denisova", 
                    "Gallus gallus"),
                  "Homo saapiens,, Gallus gallus",
                  TRUE,
                  "double_comma_bool_on")

# double_comma_bool_off --------------------------------------------------------
function_template(c("Homo saapiens", "Gallus gallus"),
                  "Homo saapiens,, Gallus gallus",
                  FALSE,
                  "double_comma_bool_off")


