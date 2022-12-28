test_that("Commenting code works", {
  mockr::local_mock(
    gpt_edit = function(model, instruction, temperature) {
      list("text" = "new text")
    })
  expect_type(comment_code_addin(), "list")
})

test_that("Inserting roxygen works", {
  mockr::local_mock(
    gpt_insert = function(model, prompt, temperature) {
      list("text" = "new text")
    })
  expect_type(add_roxygen_addin(), "list")
})

test_that("Script to function works", {
  mockr::local_mock(
    gpt_edit = function(model, instruction, temperature) {
      list("text" = "new text")
    })
  expect_type(script_to_function_addin(), "list")
})

test_that("Suggesting a unit test works", {
  mockr::local_mock(
    gpt_insert = function(model, prompt, temperature, append_text, max_tokens) {
      list("text" = "new text")
    })
  expect_type(suggest_unit_test_addin(), "list")
})
