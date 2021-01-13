app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("side_panel")

app$setInputs(sidebarItemExpanded = "Plots")
app$snapshot()
app$setInputs(sidebarItemExpanded = character(0))
app$snapshot()
app$setInputs(sidebarItemExpanded = "Generalcontrols")
app$snapshot()
app$setInputs(sidebarItemExpanded = "Plots")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()