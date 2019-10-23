app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(speciesInput = "Merluccius mer")
app$setInputs(speciesInput = "Merluccius merluccius")
app$snapshot()
app$setInputs(addSpecies = "click", timeout_=10000)
app$snapshot()
