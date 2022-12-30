test_that("Summarize mtcars works", {
  expect_snapshot(summarize_data(mtcars, "skimr"))
  expect_snapshot(summarize_data(mtcars, "skimr_lite"))
  expect_snapshot(summarize_data(mtcars, "column_types"))
  expect_snapshot(summarize_data(mtcars, "summary"))
})

test_that("Summarize airquality works", {
  expect_snapshot(summarize_data(iris, "skimr"))
  expect_snapshot(summarize_data(iris, "skimr_lite"))
  expect_snapshot(summarize_data(iris, "column_types"))
  expect_snapshot(summarize_data(iris, "summary"))
})
