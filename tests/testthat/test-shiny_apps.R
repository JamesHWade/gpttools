test_that("shiny apps run", {
  freeform <- run_gpt_freeform()
  expect_equal(class(freeform), "shiny.appobj")
  doc_data <- run_document_data()
  expect_equal(class(doc_data), "shiny.appobj")
})
