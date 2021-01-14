app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("gating_1")

app$setInputs(plotCount = 3)
app$snapshot()
app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$snapshot()
app$setInputs(Marker_3 = "CD8a")
app$snapshot()
app$setInputs(Marker_5 = "CDH")
app$snapshot()
app$snapshot()
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()