app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("gating_marker_change")

app$setInputs(plotCount = 3)
app$setInputs(Marker_1 = "PIN")
app$setInputs(Marker_3 = "CD99")
app$setInputs(Marker_5 = "PIN")
app$snapshot()
app$setInputs(Marker_5 = "H3")
app$snapshot()
app$setInputs(Marker_3 = "CD8a")
app$snapshot()
app$setInputs(Marker_1 = "H3")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()