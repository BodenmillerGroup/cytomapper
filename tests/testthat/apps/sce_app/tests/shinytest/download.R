app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("download")

app$setInputs(assay = "counts")
app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$setInputs(labelCellsBy = "Test_cells")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
