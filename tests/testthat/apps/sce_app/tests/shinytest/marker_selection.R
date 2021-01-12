app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("marker_selection")

app$setInputs(plotCount = 2)
app$setInputs(plotCount = 3)
app$snapshot()
app$setInputs(Marker_1 = "H3")
app$snapshot()
app$setInputs(Marker_2 = "CD99")
app$snapshot()
app$setInputs(Marker_3 = "PIN")
app$snapshot()
app$setInputs(Marker_4 = "CD8a")
app$snapshot()
app$setInputs(Marker_5 = "CDH")

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
