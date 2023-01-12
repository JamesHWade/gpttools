test_that("Sitrep works", {
  expect_snapshot(gpt_sitrep())
})

test_that("Sitrep works with local options", {
  withr::local_options(
    gpttools.valid_api        = TRUE,
    gpttools.openai_key       = TRUE,
    gpttools.max_tokens       = 500,
    gpttools.valid_rstudioapi = TRUE,
    gpttools.code_style       = "tidyverse"
  )
  expect_snapshot(gpt_sitrep())
})
