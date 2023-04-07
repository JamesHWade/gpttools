withr::local_options(gpttools.max_tokens = 24)

test_that("Commenting code has correct inputs", {
  skip("Need to rewrite test")
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "x < 24"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(comment_code_addin(), "character")
})

test_that("Inserting roxygen works", {
  skip("Need to rewrite test")
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "add_xy <- function(x, y) x + y"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(add_roxygen_addin(), "character")
})

test_that("Inserting roxygen works [on CI]", {
  skip("Need to rewrite test")
  mockr::local_mock(
    gpt_insert =
      function(model, prompt, temperature, max_tokens, append_textf) {
        "#' @param x an int"
      }
  )
  expect_snapshot(add_roxygen_addin())
})

test_that("Script to function works", {
  skip("Need to rewrite test")
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "x <- 1;y <- 2;z <- x + y;z"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(script_to_function_addin(), "character")
})

test_that("Script to function works [on CI]", {
  skip("Need to rewrite test")
  mockr::local_mock(
    gpt_edit = function(model, instruction, temperature) "z <- \\(x, y) x + y"
  )
  expect_snapshot(script_to_function_addin())
})

test_that("Suggesting a unit test works", {
  skip("Need to rewrite test")
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "add_xy <- function(x, y) x + y"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(suggest_unit_test_addin(), "character")
})

test_that("Suggesting a unit test works [on CI]", {
  skip("Need to rewrite test")
  mockr::local_mock(
    gpt_insert = function(model, prompt, temperature, max_tokens, append_text) {
      "add_xy(1, 2) == 3"
    }
  )
  expect_snapshot(suggest_unit_test_addin())
})

test_that("Suggesting code improvements works", {
  skip("Need to rewrite test")
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "add_xy <- function(x, y) x + y"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(suggest_code_improvements(), "character")
})

test_that("Suggesting code improvements work [on CI]", {
  skip("Need to rewrite test")
  mockr::local_mock(
    gpt_insert = function(model, prompt, temperature, max_tokens, append_text) {
      "add_xy(1, 2) == 3"
    }
  )
  expect_snapshot(suggest_code_improvements())
})
