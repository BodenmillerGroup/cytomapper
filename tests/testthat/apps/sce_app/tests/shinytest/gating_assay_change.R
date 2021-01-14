app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("gating_assay_change")

app$setInputs(assay = "counts")
app$setInputs(plotCount = 3)
app$setInputs(Marker_1 = "CD99")
app$setInputs(Marker_3 = "CD8a")
app$setInputs(Marker_5 = "H3")
app$snapshot()
app$setInputs(assay = "exprs")
app$snapshot()
app$snapshot()
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()