app <- ShinyDriver$new("../../")
app$snapshotInit("CruxCsvUpload")

app$uploadFile(uCRUXfile = "../LargeCruxSearch(717items).csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
app$setInputs(uploadCRUXButton = "click")
app$snapshot()
