library(testthat)
library(modules)

orgListHelper <- modules::use("orgListHelper.R")

test_that("boolean_off", {
  taxizeHelperOutput <- orgListHelper$taxizeHelper("Homo saapiens", FALSE)
})

test_that("empty_string", {
  taxizeHelperOutput <- orgListHelper$taxizeHelper("", TRUE)
})

test_that("empty_string", {
  taxizeHelperOutput <- orgListHelper$taxizeHelper("", TRUE)
})

test_that("empty_string", {
  taxizeHelperOutput <- orgListHelper$taxizeHelper("", TRUE)
})

test_that("empty_string", {
  taxizeHelperOutput <- orgListHelper$taxizeHelper("", TRUE)
})