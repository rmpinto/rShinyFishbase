app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(speciesInput = "Merluccius merlucciu")
app$setInputs(speciesInput = "Merluccius merluccius")
app$snapshot()
app$setInputs(addSpecies = "click", wait_=TRUE, timeout_=600000)
app$snapshot()
