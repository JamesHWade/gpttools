test_that("multiplication works", {
  skip("Not implemented yet.")
  skip_if_offline()
  save_history_and_embeddings(role = "user", content = "this is a test")
  save_history_and_embeddings(
    role = "user", content = "this is a test",
    overwrite = TRUE
  )
})
