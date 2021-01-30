app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("simple_gating")

brush1 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 12,
               ymax = 34,
               mapping = list(x = "sample",
                              y = "CD99"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
Sys.sleep(0.5)
#app$snapshot()
app$setInputs(resetMarkers = "click")
#app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
