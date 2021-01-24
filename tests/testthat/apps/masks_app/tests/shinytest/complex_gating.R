app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("complex_gating")

brush1 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 12,
               ymax = 34,
               mapping = list(x = "sample",
                              y = "CD99"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

Sys.sleep(0.5)
app$setInputs(Marker_1 = "CD99")
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
Sys.sleep(0.5)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
app$setInputs(resetMarkers = "click")
app$snapshot()
app$setInputs(Marker_2 = "PIN")
app$snapshot()

brush1 <- list(xmin = 5,
               xmax = 34,
               ymin = 10,
               ymax = 60,
               mapping = list(x = "CD99",
                              y = "PIN"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
app$setInputs(resetMarkers = "click")
app$snapshot()
app$setInputs(plotCount = 3)
app$snapshot()
app$setInputs(Marker_1 = "CD99")
app$setInputs(Marker_2 = "PIN")
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$setInputs(Marker_3 = "CDH")
app$snapshot()

brush2 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 8,
               ymax = 15,
               mapping = list(x = "sample",
                              y = "CDH"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

app$setInputs(plot_brush2 = brush2, allowInputNoBinding_ = TRUE)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
app$setInputs(resetMarkers = "click")
app$snapshot()
app$setInputs(tabbox1 = "tab1")
app$setInputs(Marker_5 = "CD8a")

brush2 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 0,
               ymax = 15,
               mapping = list(x = "sample",
                              y = "CDH"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

brush3 <- list(xmin = 0.5,
               xmax = 1.5,
               ymin = 0,
               ymax = 10,
               mapping = list(x = "sample",
                              y = "CD8a"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")
app$setInputs(plot_brush2 = brush2, allowInputNoBinding_ = TRUE)
app$setInputs(plot_brush3 = brush3, allowInputNoBinding_ = TRUE)

app$snapshot()
app$setInputs(tabbox1 = "tab2")
app$setInputs(resetMarkers = "click")
app$snapshot()
