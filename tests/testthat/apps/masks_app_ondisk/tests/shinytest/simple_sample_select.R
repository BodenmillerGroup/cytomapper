app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("simple_sample_select")

Sys.sleep(0.5)
app$setInputs(Marker_1 = "CD99")
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(sample = "2")
Sys.sleep(0.5)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()
app$setInputs(sample = "3")
Sys.sleep(0.5)
app$snapshot()
app$setInputs(tabbox1 = "tab2")
#app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
