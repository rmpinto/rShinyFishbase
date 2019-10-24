app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(
  speciesInput = "Merluccius merluccius",
  addSpecies = "click"
)
Sys.sleep(120)
app$snapshot()
app$setInputs(getData = "click")
Sys.sleep(120)
app$snapshot(
  screenshot = TRUE,
  items = list(
    output = "tbl-species"
  )
)

