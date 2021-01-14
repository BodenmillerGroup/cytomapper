app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("gating_plot_reset")

app$setInputs(assay = "counts")
app$setInputs(plotCount = 2)
app$setInputs(plotCount = 3)
app$setInputs(Marker_1 = "CD99")
app$setInputs(Marker_3 = "CD8a")
app$setInputs(Marker_5 = "H3")
app$snapshot()
app$setInputs(plotCount = 2)
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()