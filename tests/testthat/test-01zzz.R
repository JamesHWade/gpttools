test_that(".onLoad sets options appropriately", {
  .onLoad()
  expect_false(getOption("gpttools.valid_api"))
  expect_null(getOption("gpttools.openai_key"))
  expect_equal(getOption("gpttools.max_tokens"), 500)
})
