app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("complex_assay_change")

brush1 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 12,
               ymax = 34,
               mapping = list(x = "sample",
                              y = "CD99"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

brush2 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 5,
               ymax = 15,
               mapping = list(x = "sample",
                              y = "CDH"),
               direction = "xy",
               brushId = "plot_brush2",
               outputId = "scatter2")

brush3 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 10,
               ymax = 20,
               mapping = list(x = "sample",
                              y = "PIN"),
               direction = "xy",
               brushId = "plot_brush3",
               outputId = "scatter3")

app$setInputs(plotCount = 3)
app$setInputs(Marker_1 = "CD99")
app$setInputs(Marker_3 = "CDH")
app$setInputs(Marker_5 = "PIN")
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$setInputs(plot_brush2 = brush2, allowInputNoBinding_ = TRUE)
app$setInputs(plot_brush3 = brush3, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
app$setInputs(resetMarkers = "click")
#app$snapshot()
app$setInputs(assay = "exprs")
app$snapshot()

brush1 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = asinh(12),
               ymax = asinh(34),
               mapping = list(x = "sample",
                              y = "CD99"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

brush2 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = asinh(5),
               ymax = asinh(15),
               mapping = list(x = "sample",
                              y = "CDH"),
               direction = "xy",
               brushId = "plot_brush2",
               outputId = "scatter2")

brush3 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = asinh(10),
               ymax = asinh(20),
               mapping = list(x = "sample",
                              y = "PIN"),
               direction = "xy",
               brushId = "plot_brush3",
               outputId = "scatter3")

app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$setInputs(plot_brush2 = brush2, allowInputNoBinding_ = TRUE)
app$setInputs(plot_brush3 = brush3, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(assay = "counts")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
