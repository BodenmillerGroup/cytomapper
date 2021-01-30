app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("contrasts_set")

brush1 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 5,
               ymax = 40,
               mapping = list(x = "sample",
                              y = "PIN"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

Sys.sleep(0.5)
app$setInputs(Marker_1 = "PIN")
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(resetMarkers = "click")
#app$snapshot()
app$setInputs(contrast_marker_1 = 3)
#app$snapshot()
app$setInputs(contrast_marker_1 = 5)
#app$snapshot()
app$setInputs(exprs_marker_2 = "CD99")
#app$snapshot()
app$setInputs(contrast_marker_2 = 3)
#app$snapshot()
app$setInputs(contrast_marker_2 = 5)
#app$snapshot()
app$setInputs(sample = "2")
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(sample = "3")
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(contrast_marker_1 = 1)
#app$snapshot()
app$setInputs(contrast_marker_2 = 1)
#app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
