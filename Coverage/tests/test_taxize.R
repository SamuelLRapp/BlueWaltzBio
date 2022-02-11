library(testthat)
library(modules)
library(future)
library(promises)
library(ipc)

orgListHelper <- modules::use("../orgListHelper.R")

# The list of fundamental cases (not combined):
# 1) We toggle the second argument on or off
# 2) We can have duplicate inputs
# 3) We can have "odd" characters in the string
# 4) double commas
# 5) leading or trailing whitespace
# 6) double spaces inbetween words in the same org name
# 7) 

test_that("boolean_off", {
  future_promise({c("Homo saapiens")}) %...>% {
    expectedOutput <- .
    orgListHelper$taxizeHelper("Homo saapiens", FALSE) %...>%{
      taxizeHelperOutput <- .
      if(taxizetaxizeHelperOutput != expectedOutput){
        
      }
      expect_equal(taxizeHelperOutput, expectedOutput)
    }
  }
})

# test_that("empty_string", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("", TRUE)
#   expectedOutput <- c("") #                                                  VERIFY THIS PLEASE 
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })

# test_that("simple_test", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("Homo saapiens", TRUE)
#   expectedOutput <- c("Homo saapiens", "Homo sapiens", "Homo sapiens subsp. Denisova")
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })
# 
# test_that("duplicates_bool_on", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("Gallus gallus, Gallus gallus", TRUE)
#   expectedOutput <- c("Gallus gallus")
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })
# 
# test_that("duplicates_bool_off", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("Homo saapiens, Homo saapiens", FALSE)
#   expectedOutput <- c("Homo saapiens")
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })
# 
# test_that("double_comma_bool_on", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("Homo saapiens,, Gallus gallus", TRUE)
#   expectedOutput <- c("Homo saapiens", "Homo sapiens", "Homo sapiens subsp. Denisova", "Gallus gallus")
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })
# 
# test_that("double_comma_bool_off", {
#   taxizeHelperOutput <- orgListHelper$taxizeHelper("Homo saapiens,, Gallus gallus", TRUE)
#   expectedOutput <- c("Homo saapiens", "Gallus gallus")
#   expect_equal(taxizeHelperOutput, expectedOutput)
# })