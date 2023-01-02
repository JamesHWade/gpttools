withr::local_options(gpttools.max_tokens = 24)

test_that("Commenting code has correct inputs", {
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "x < 24"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(comment_code_addin(), "character")
})

test_that("Inserting roxygen works", {
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "add_xy <- function(x, y) x + y"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(add_roxygen_addin(), "character")
})

test_that("Script to function works", {
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "x <- 1;y <- 2;z <- x + y;z"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(script_to_function_addin(), "character")
})

test_that("Suggesting a unit test works", {
  skip_on_ci()
  skip_if_offline()
  mockr::local_mock(
    get_selection = function() list(value = "add_xy <- function(x, y) x + y"),
    insert_text = function(improved_text) improved_text
  )
  expect_type(suggest_unit_test_addin(), "character")
})
