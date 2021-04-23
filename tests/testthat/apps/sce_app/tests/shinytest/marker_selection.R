app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("marker_selection")

brush1 <- list(xmin = 5,
               xmax = 34,
               ymin = 10,
               ymax = 60,
               mapping = list(x = "CD99",
                              y = "H3"),
               direction = "xy",
               brushId = "plot_brush1",
               outputId = "scatter1")

brush2 <- list(xmin = 5,
               xmax = 30,
               ymin = 0,
               ymax = 40,
               mapping = list(x = "PIN",
                              y = "CDH"),
               direction = "xy",
               brushId = "plot_brush2",
               outputId = "scatter2")

brush3 <- list(xmin = 0,
               xmax = 20,
               ymin = 10,
               ymax = 40,
               mapping = list(x = "CD8a",
                              y = "CD99"),
               direction = "xy",
               brushId = "plot_brush3",
               outputId = "scatter3")

app$setInputs(assay = "counts")
app$setInputs(plotCount = 2)
app$setInputs(plotCount = 3)
app$snapshot()
app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$setInputs(Marker_2 = "H3")
app$snapshot()
app$setInputs(plot_brush1 = brush1, allowInputNoBinding_ = TRUE)
app$setInputs(Marker_3 = "PIN")
app$snapshot()
app$setInputs(Marker_4 = "CDH")
app$snapshot()
app$setInputs(plot_brush2 = brush2, allowInputNoBinding_ = TRUE)
app$setInputs(Marker_5 = "CD8a")
app$snapshot()
app$setInputs(Marker_6 = "CD99")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
