test_that("Only use get_selection() if rstudioapi is available", {
  expect_error(get_selection())
})

test_that("Only use insert_texty() if rstudioapi is available", {
  expect_error(insert_text("Here is some text"))
})
