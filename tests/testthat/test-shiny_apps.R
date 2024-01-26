test_that("shiny apps run", {
  doc_data <- run_document_data()
  expect_equal(class(doc_data), "shiny.appobj")
})
