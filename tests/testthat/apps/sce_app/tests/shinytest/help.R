app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("help")

app$snapshot()
app$setInputs(SessionInfo = "click")
app$snapshot()
app$snapshot()
app$setInputs(Help = "click")
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
