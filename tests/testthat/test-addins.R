test_that("Commenting code works", {
  mockr::local_mock(
    gpt_edit = function(
    model = "code-davinci-edit-001",
    instruction = "add comments to each line of code, explaining what the code does",
    temperature = 0.1,
    top_p = 1) {
      list("text" = "new text")
    })
  expect_type(comment_code_addin(), "list")
})

test_that("Inserting roxygen works", {
  mockr::local_mock(
    gpt_insert = function(
    model = "code-davinci-edit-001",
    prompt = "insert roxygen to document this function",
    temperature = 0.1,
    top_p = 1) {
      list("text" = "new text")
    })
  expect_type(add_roxygen_addin(), "list")
})


