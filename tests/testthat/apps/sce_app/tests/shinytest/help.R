app <- ShinyDriver$new("../../", loadTimeout = 100000, seed = 1234)
app$snapshotInit("help")

app$snapshot()
app$setInputs(SessionInfo = "click")
expect_equal(app$findWidget("SessionInfo")$getValue(), 1)
app$snapshot()
app$setInputs(Help = "click")
expect_equal(app$findWidget("Help")$getValue(), 1)
app$snapshot()

p <- app$.__enclos_env__$private$shinyProcess
p$interrupt()
p$wait()
