app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("assay_selection")

app$setInputs(assay = "exprs")
app$snapshot()
app$setInputs(assay = "counts")
app$snapshot()
app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$setInputs(assay = "exprs")
app$snapshot()
app$setInputs(Marker_2 = "PIN")
app$snapshot()
app$setInputs(assay = "counts")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
