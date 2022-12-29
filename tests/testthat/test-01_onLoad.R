test_that(".onLoad sets options appropriately", {
  .onLoad()
  expect_false(getOption("gpttools.valid_api"))
  expect_null(getOption("gpttools.openai_key"))
})
