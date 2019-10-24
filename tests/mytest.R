app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$setInputs(
  speciesInput = "Merluccius merluccius",
  addSpecies = "click"
)
Sys.sleep(60)
app$snapshot()
app$setInputs(getData = "click")
Sys.sleep(60)
app$snapshot(
  screenshot = TRUE,
  items = list(
    input = TRUE,
    output = "tbl-species"
  )
)

