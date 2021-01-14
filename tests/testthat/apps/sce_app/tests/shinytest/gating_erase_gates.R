app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("gating_erase_gates")

app$setInputs(assay = "counts")
app$setInputs(plotCount = 3)
app$setInputs(Marker_1 = "CD99")
app$setInputs(Marker_3 = "PIN")
app$setInputs(Marker_2 = "H3")
app$setInputs(Marker_4 = "CDH")
app$setInputs(Marker_5 = "CD99")
app$setInputs(Marker_6 = "CDH")
app$snapshot()
app$snapshot()
app$snapshot()
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
