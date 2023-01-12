test_that("Sitrep works", {
  withr::local_envvar("OPENAI_API_KEY" = "a4b5dc06-43ca-442b-84cb-9cbb7fac77f9")
  expect_snapshot(gpt_sitrep())
})

test_that("Sitrep works with local options", {
  withr::local_envvar("OPENAI_API_KEY" = "a4b5dc06-43ca-442b-84cb-9cbb7fac77f9")
  withr::local_options(
    gpttools.valid_api        = TRUE,
    gpttools.openai_key       = TRUE,
    gpttools.max_tokens       = 500,
    gpttools.valid_rstudioapi = TRUE,
    gpttools.code_style       = "tidyverse"
  )
  expect_snapshot(gpt_sitrep())
})
