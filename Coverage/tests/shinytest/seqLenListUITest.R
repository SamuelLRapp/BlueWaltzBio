app <- ShinyDriver$new("../../", seed = 1)
app$snapshotInit("seqLenListUITest")

app$setInputs(seqLengthOption = TRUE)
app$snapshot()
app$setInputs(barcodeOptionCO1 = "click")
app$snapshot()
app$setInputs(barcodeOptionITS1 = "click")
app$snapshot()
app$setInputs(barcodeOptiontrnl = "click")
app$snapshot()
app$setInputs(ITS1 = c(500, 2000))
app$setInputs(`(CO1; COI; COX1)` = c(0, 200))
app$setInputs(trnl = c(0, 0))
app$snapshot()
app$setInputs(NCBIorganismList = "Ga")
app$setInputs(NCBIorganismList = "Gallus ga")
app$setInputs(NCBIorganismList = "Gallus gallus")
app$snapshot()

