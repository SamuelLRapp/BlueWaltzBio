app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("FullGenomeCsvUpload")

app$setInputs(mainPage = "Full Genome Search")
app$snapshot()
app$uploadFile(uploadGenomeFile = "../LargeCruxSearch(717items).csv") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()
app$setInputs(FullGenomeStart = "click")
app$snapshot()
