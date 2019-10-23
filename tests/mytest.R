app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$snapshot()
app$setInputs(speciesInput = "Merluccius ")
app$setInputs(speciesInput = "Merluccius mer")
app$setInputs(speciesInput = "Merluccius merlucciu")
app$setInputs(speciesInput = "Merluccius merluccius")
app$snapshot()
